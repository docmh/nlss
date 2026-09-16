#!/usr/bin/env python3
"""Bounded release/standalone-maintenance checks; no personal harness changes."""
# SPDX-License-Identifier: Apache-2.0
import argparse
from datetime import datetime, timezone
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
from urllib.parse import unquote
import zipfile

REPO = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("build_release", REPO / "scripts/build_release.py")
builder = importlib.util.module_from_spec(spec)
sys.dont_write_bytecode = True
spec.loader.exec_module(builder)


def stamp():
    return datetime.now(timezone.utc).isoformat()


def tree(path):
    return {p.relative_to(path).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(path.rglob("*")) if p.is_file()}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=os.environ.get("NLSS_TEST_ROOT"))
    parser.add_argument("--keep", type=int, default=os.environ.get("NLSS_KEEP_RUNS"))
    args = parser.parse_args()
    # Reuse the existing YAML reader/configuration, not another test-default source.
    settings = json.loads(subprocess.check_output(["Rscript", "--vanilla", "-e",
        'cat(jsonlite::toJSON(yaml::read_yaml("tests/tests.yml")$tests, auto_unbox=TRUE))'], cwd=REPO, text=True))
    keep = settings["keep_runs_default"] if args.keep is None else args.keep
    if keep < 0:
        parser.error("--keep must be nonnegative")
    root = Path(args.root or REPO / settings["output_dir"]).resolve()
    root.mkdir(parents=True, exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix="phase5-packaging-", dir=root))
    report = {"started": stamp(), "exit_status": None, "checks": [], "commands": [],
              "source": {**{n: builder.sha(b) for n, b in builder.collect(REPO).items()},
                         "tests/phase5/run_packaging_tests.py": builder.sha(Path(__file__).read_bytes()),
                         "scripts/build_release.py": builder.sha((REPO / "scripts/build_release.py").read_bytes())}}

    def save():
        (work / "results.json").write_bytes(builder.json_bytes(report))

    def check(label, passed):
        report["checks"].append({"label": label, "passed": bool(passed)})
        save()
        if not passed:
            raise AssertionError(label)
        print("PASS", label, flush=True)

    unrelated = work / "unrelated cwd ü"
    unrelated.mkdir()

    def run(command, expected=0, env=None):
        command = [str(x) for x in command]
        entry = {"command": command, "cwd": str(unrelated), "started": stamp(), "exit_status": None}
        report["commands"].append(entry)
        save()
        proc = subprocess.run(command, cwd=unrelated, env=env, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=120)
        entry.update(ended=stamp(), exit_status=proc.returncode)
        name = f"command-{len(report['commands'])}.log"
        (work / name).write_text(proc.stdout, encoding="utf-8")
        entry["log"] = name
        save()
        check(f"command {len(report['commands'])}: expected exit {expected}", proc.returncode == expected)
        return proc.stdout

    save()
    try:
        one = builder.build(REPO, work / "build1")
        two = builder.build(REPO, work / "build2")
        check("same input produces identical metadata and archives", one == two and tree(work / "build1") == tree(work / "build2"))
        check("working-tree identity includes new installer", one["source"]["mode"] == "selected-working-tree" and "scripts/R/install_nlss.R" in one["source"]["files_sha256"])
        version = one["version"]
        skill = work / "unpacked skill ü"
        plugin_parent = work / "unpacked plugin ü"
        for kind, dest in (("skill", skill), ("plugin", plugin_parent)):
            with zipfile.ZipFile(work / "build1" / f"nlss-{version}-{kind}.zip") as archive:
                check(f"{kind}: safe plain files only", all(not n.startswith("/") and ".." not in Path(n).parts and not (i.external_attr >> 16 & 0o170000) == 0o120000
                      for i in archive.infolist() for n in [i.filename]))
                archive.extractall(dest)
        skill /= "nlss"
        catalogue = plugin_parent / "nlss-release"
        plugin = catalogue / "plugins/nlss"
        check("plugin and standalone scientific payloads identical", tree(skill) == tree(plugin / "skills/nlss"))
        check("payload contains no test/development/private output", not any(n.startswith(("tests/", "outputs/", "packaging/", ".git/", "review-")) or "__pycache__" in n for n in tree(skill)))
        check("complete runtime and license/demo shipped", all((skill / n).is_file() for n in builder.collect(REPO)))
        for manifest in ("plugin.json", ".codex-plugin/plugin.json", ".claude-plugin/plugin.json"):
            data = json.loads((plugin / manifest).read_text())
            check(f"{manifest}: consistent version and namespace", data["name"] == "nlss" and data["version"] == version)
        portable = json.loads((plugin / "plugin.json").read_text())
        check("portable manifest closed standard fields", portable["$schema"] == builder.SCHEMA and set(portable) <= {"$schema", "name", "version", "description", "author", "homepage", "repository", "license", "keywords", "extensions"})
        for descriptor in (".agents/plugins/marketplace.json", ".claude-plugin/marketplace.json"):
            data = json.loads((catalogue / descriptor).read_text())
            source = data["plugins"][0]["source"]
            path = source["path"] if isinstance(source, dict) else source
            check(f"{descriptor}: resolves shared local plugin", (catalogue / path).resolve() == plugin)
        check("release README version", version in (plugin / "README.md").read_text())
        check("skill frontmatter version/name", re.search(r"(?m)^name: nlss$", (skill / "SKILL.md").read_text()) and f'nlss.version: "{version}"' in (skill / "SKILL.md").read_text())
        missing_links = []
        for path in [skill / "SKILL.md", skill / "AGENTS.md", *sorted((skill / "references").rglob("*.md"))]:
            prose = re.sub(r"(?ms)^```.*?^```[^\n]*", "", path.read_text())
            for ref in re.findall(r"\[[^\]]*\]\(([^)]+)\)", prose):
                ref = ref.split("#")[0]
                if not ref or re.match(r"[a-zA-Z]+:", ref) or "<" in ref:
                    continue
                if not (path.parent / unquote(ref)).exists():
                    missing_links.append((str(path.relative_to(skill)), ref))
        check(f"local runtime Markdown links resolve: {missing_links}", not missing_links)
        # No host R packages available: installer and help still run using base R.
        empty_library = work / "empty-library"
        empty_library.mkdir()
        isolated = {**os.environ, **{key: str(empty_library) for key in ("R_LIBS", "R_LIBS_USER", "R_LIBS_SITE", "NLSS_R_LIBRARY")}}
        helper = skill / "scripts/R/install_nlss.R"
        target = work / "user skills ü/nlss"
        sentinel = work / "research/sentinel.txt"
        sentinel.parent.mkdir()
        sentinel.write_text("Research remains researcher-owned.\n")
        unrelated_skill = work / "user skills ü/other/SKILL.md"
        unrelated_skill.parent.mkdir(parents=True)
        unrelated_skill.write_text("another skill")

        def install(action, source=skill, approve=True, destination=target, expected=0):
            command = ["Rscript", "--vanilla", helper, "--action", action, "--destination", destination]
            if action in ("install", "update"):
                command += ["--source", source]
            if approve:
                command += ["--approve"]
            return run(command, expected, isolated)

        run(["Rscript", "--vanilla", helper, "--help"], env=isolated)
        install("install", approve=False)
        check("preview writes no destination", not target.exists())
        install("install")
        check("fresh installed files equal unpacked payload", tree(target) == tree(skill))
        before = tree(target)
        mtimes = {p: p.stat().st_mtime_ns for p in target.rglob("*") if p.is_file()}
        install("install")
        install("status")
        check("same-release repeat does not rewrite files", before == tree(target) and all(p.stat().st_mtime_ns == t for p, t in mtimes.items()))
        # A second isolated source with one canonical version change is an update fixture.
        source2 = work / "source2"
        inputs = builder.collect(REPO)
        for name in ("scripts/build_release.py", "packaging/plugin.json", "packaging/codex-interface.json"):
            inputs[name] = (REPO / name).read_bytes()
        for name, data in inputs.items():
            dest = source2 / name
            dest.parent.mkdir(parents=True, exist_ok=True)
            dest.write_bytes(data)
        config = source2 / "scripts/config.yml"
        config.write_text(config.read_text().replace(f'nlss_version: "{version}"', 'nlss_version: "9.9.9"'))
        info2 = builder.build(source2, work / "build-update")
        updated = work / "unpacked update/nlss"
        with zipfile.ZipFile(work / "build-update/nlss-9.9.9-skill.zip") as archive:
            archive.extractall(updated.parent)
        check("one canonical bump reaches built skill", 'nlss.version: "9.9.9"' in (updated / "SKILL.md").read_text() and info2["version"] == "9.9.9")
        install("install", source=updated, expected=1)
        install("update", source=updated, approve=False)
        check("update preview preserves old payload", tree(target) == before)
        install("update", source=updated)
        check("update activates entire new payload", tree(target) == tree(updated))
        current = tree(target)
        broken = work / "broken/nlss"
        shutil.copytree(skill, broken)
        (broken / "scripts/R/power.R").write_text("damaged")
        install("update", source=broken, expected=1)
        check("damaged update preserves current installation", tree(target) == current)
        (target / "README.md").write_text("researcher's local edit")
        edited = tree(target)
        install("update", expected=1)
        install("remove", expected=1)
        check("local edits preserved on update and removal refusal", tree(target) == edited)
        (target / "README.md").write_bytes((updated / "README.md").read_bytes())
        (target / "extra.txt").write_text("user file")
        install("remove", expected=1)
        (target / "extra.txt").unlink()
        unowned = work / "unowned/nlss"
        unowned.mkdir(parents=True)
        (unowned / "SKILL.md").write_text("unmanaged")
        install("install", destination=unowned, expected=1)
        install("remove", destination=unowned, expected=1)
        check("unrecognized destination untouched", (unowned / "SKILL.md").read_text() == "unmanaged")
        (sentinel.parent / "nlss-workspace.yml").write_text("research project sentinel")
        install("install", destination=sentinel.parent / "nlss", expected=1)
        install("remove", destination=plugin / "skills/nlss", expected=1)
        install("install", destination=skill, expected=1)
        # Inject IO failures into a sourced test environment; no production fault switch.
        for fault in ("copy", "activate"):
            driver = work / f"failure-{fault}.R"
            argv = ["--action", "update", "--source", str(skill), "--destination", str(target), "--approve"]
            rstr = lambda value: json.dumps(str(value), ensure_ascii=False)
            lines = [f'commandArgs <- function(trailingOnly = FALSE) if (trailingOnly) c({",".join(map(rstr, argv))}) else {rstr("--file=" + str(helper))}',
                     f'source({rstr(helper)})']
            if fault == "copy":
                lines += ['file.copy <- function(...) FALSE']
            else:
                lines += ['file.rename <- function(from, to) if (startsWith(basename(from), ".nlss-stage-")) FALSE else base::file.rename(from, to)']
            lines += ['status <- tryCatch({ nlss_install_main(); 0L }, error = function(e) { message(conditionMessage(e)); 1L })', 'quit(status = status)']
            driver.write_text("\n".join(lines) + "\n")
            run(["Rscript", "--vanilla", driver], 1, isolated)
            check(f"{fault} failure preserves old installation", tree(target) == current)
        check("normal/failure paths leave no backup stack", not list(target.parent.glob(".nlss-*")))
        install("remove", approve=False)
        check("remove preview preserves files", tree(target) == current)
        install("remove")
        check("removal affects only selected skill", not target.exists() and unrelated_skill.read_text() == "another skill")
        install("remove")
        install("install")
        check("selected prior release reinstalls", tree(target) == tree(skill))
        check("research files unchanged by maintenance", sentinel.read_text() == "Research remains researcher-owned.\n" and (sentinel.parent / "nlss-workspace.yml").read_text() == "research project sentinel")
        # Run actual packaged code, from unrelated cwd and space/Unicode paths.
        runtime = plugin / "skills/nlss"
        launcher = runtime / "scripts/R/run_nlss.R"
        project = work / "Demo study ü"
        sample = runtime / "assets/sample-data/golden_dataset.csv"
        sample_before = builder.sha(sample.read_bytes())

        def rscript(operation, *options):
            return run(["Rscript", "--vanilla", launcher, operation, *options])

        run(["Rscript", "--vanilla", launcher, "--help"], env=isolated)
        # No fitting matrix: each existing entrypoint still owns its own help/CLI.
        for entry in sorted((runtime / "scripts/R").glob("*.R")):
            if entry.stem not in ("run_nlss", "install_nlss"):
                run(["Rscript", "--vanilla", launcher, entry.stem, "--help"], env=isolated)
        for invalid in ("../regression", "unknown", "run_nlss", "install_nlss"):
            run(["Rscript", "--vanilla", launcher, invalid, "--help"], 1, isolated)
        missing = json.loads(run(["Rscript", "--vanilla", launcher, "project-create",
            "--project", project, "--source", sample], 42, isolated))
        check("launcher preflight identifies selected operation, with no project writes",
              missing["operation"] == "project_create" and missing["status"] == "missing_dependency" and not project.exists())
        run(["Rscript", "--vanilla", launcher, "regression", "--not-an-option", "value"], 1)
        recovery = json.loads(run(["Rscript", "--vanilla", launcher, "dependency-resolver",
            "--operation", "sem", "--", "--model", "y ~ x"], 42, isolated))
        check("dependency recovery keeps separator and target operation", recovery["operation"] == "sem" and recovery["status"] == "missing_dependency")

        project.mkdir()
        local_sample = project / "demo.csv"
        shutil.copyfile(sample, local_sample)
        created = rscript("project_create", "--project", project, "--source", local_sample, "--working", "data/current.parquet", "--name", "demo")
        check("packaged direct CSV demo creates visible working data", (project / "data/current.parquet").is_file() and '"status"' in created)
        rscript("data_explorer", "--project", project, "--dataset", "demo", "--user-prompt", "Explore demo")
        prompt = "Regression demo: ü, quotes ' \" and equals=a; ~+~ stays literal."
        rscript("regression", "--project", project, "--dataset", "demo", "--dv", "outcome_reg", "--ivs", "x1,x2", "--user-prompt=" + prompt)
        results = [json.loads(p.read_text()) for p in (project / ".nlss/runs").glob("*/result.json")]
        regression = next(r for r in results if r["module"] == "regression")
        request_path = next(p for p in (project / ".nlss/runs").glob("*/request.json") if json.loads(p.read_text())["module"] == "regression")
        request = json.loads(request_path.read_text())
        check("launcher retains audit module, options and prompt verbatim", request["module"] == "regression" and request["user_prompt"] == prompt and request["cli"]["ivs"] == "x1,x2")
        estimates = [row["estimate"] for row in regression["results"]["coefficients_df"]]
        expected = json.loads(run(["Rscript", "--vanilla", "-e", 'd <- read.csv(commandArgs(TRUE)[1]); cat(jsonlite::toJSON(unname(coef(lm(outcome_reg ~ x1 + x2, d))), digits=16))', sample]))
        check("packaged regression equals independent lm", len(estimates) == len(expected) and all(abs(a-b) < 1e-10 for a, b in zip(estimates, expected)))
        original_run = tree(request_path.parent)
        working_before = builder.sha((project / "data/current.parquet").read_bytes())
        rscript("replay-run", "--request", request_path)
        replay_path = next(p for p in (project / ".nlss/runs").glob("*/request.json") if json.loads(p.read_text()).get("replay_of") == request["run_id"])
        replay = json.loads((replay_path.parent / "result.json").read_text())
        check("packaged replay subprocess preserves regression values and Markdown",
              replay["results"]["coefficients_df"] == regression["results"]["coefficients_df"] and
              (replay_path.parent / "output.md").read_bytes() == (request_path.parent / "output.md").read_bytes())
        check("replay preserves prior run and visible data", tree(request_path.parent) == original_run and builder.sha((project / "data/current.parquet").read_bytes()) == working_before)
        rscript("plot", "--project", project, "--dataset", "demo", "--type", "scatter", "--x", "x1", "--y", "outcome_reg", "--user-prompt", "Plot demo")
        protocol = (project / "report_canonical.md").read_text()
        artifacts = re.findall(r'!?\[[^\]]*\]\(([^)]+)\)', protocol)
        artifacts = [ref for ref in artifacts if not re.match(r"[a-zA-Z]+:", ref) and not ref.startswith("#")]
        check("root protocol artifact links resolve", bool(artifacts) and all((project / unquote(ref.split("#")[0])).exists() for ref in artifacts))
        check("demo original and installed payload untouched", builder.sha(local_sample.read_bytes()) == sample_before and builder.sha(sample.read_bytes()) == sample_before and tree(runtime) == tree(skill))
        # Exercise the standalone payload too, without creating extra data copies.
        run(["Rscript", "--vanilla", target / "scripts/R/run_nlss.R", "project-inspect", "--project", project])
        before_refusal = tree(project)
        code_path = runtime / "scripts/R/lib/cli.R"
        original_code = code_path.read_bytes()
        try:
            code_path.write_bytes(original_code + b"\n# replay code-drift fixture\n")
            refused = run(["Rscript", "--vanilla", launcher, "replay-run", "--request", request_path], 1)
            check("launcher retains exact-code replay gate", "NLSS R code differs" in refused and tree(project) == before_refusal)
        finally:
            code_path.write_bytes(original_code)
        # Source mutation and inside-tree output must be refused, not silently packaged.
        try:
            builder.build(REPO, REPO / "assets/forbidden-build")
        except ValueError:
            check("in-source build output rejected", not (REPO / "assets/forbidden-build").exists())
        else:
            check("in-source build output rejected", False)
        try:
            builder.build(REPO, work / "build1")
        except ValueError:
            check("existing release not overwritten", tree(work / "build1") == tree(work / "build2"))
        else:
            check("existing release not overwritten", False)
        check("source payload unchanged during validation", all(builder.sha((REPO / n).read_bytes()) == h for n, h in report["source"].items()))
        report["exit_status"] = 0
    except Exception as exc:
        report["exit_status"] = 1
        report["error"] = str(exc)
        raise
    finally:
        report["ended"] = stamp()
        save()
        print("Evidence:", work, flush=True)
    if not args.root and keep > 0:
        old = sorted((p for p in root.glob("phase5-packaging-*") if (p / "results.json").is_file()), key=lambda p: p.stat().st_mtime, reverse=True)
        for path in old[keep:]:
            shutil.rmtree(path)
    print(len(report["checks"]), "packaging/maintenance checks passed.")


if __name__ == "__main__":
    main()
