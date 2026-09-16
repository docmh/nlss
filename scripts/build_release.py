#!/usr/bin/env python3
"""Assemble the selected working tree. Maintainer-only; Python standard library."""
# SPDX-License-Identifier: Apache-2.0
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess
import zipfile

SCHEMA = "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json"


def json_bytes(value):
    return (json.dumps(value, indent=2, ensure_ascii=False, sort_keys=True) + "\n").encode()


def sha(data):
    return hashlib.sha256(data).hexdigest()


def collect(repo):
    """Allowlisted runtime only, including new/untracked runtime files."""
    paths = [repo / x for x in ("SKILL.md", "AGENTS.md", "README.md", "LICENSE", "NOTICE", "scripts/config.yml")]
    for folder, suffix in (("scripts/R", ".R"), ("references", ".md"), ("assets", None)):
        for p in sorted((repo / folder).rglob("*")):
            rel = p.relative_to(repo)
            if any(part.startswith(".") or part == "__pycache__" for part in rel.parts):
                continue
            if p.is_symlink():
                raise ValueError(f"Runtime symlink is not distributable: {rel}")
            if p.is_file() and (suffix is None or p.suffix == suffix):
                paths.append(p)
    result = {}
    for p in paths:
        if p.is_symlink() or not p.is_file():
            raise ValueError(f"Missing/linked release input: {p}")
        name = p.relative_to(repo).as_posix()
        if not re.fullmatch(r"[A-Za-z0-9_. /-]+", name) or "\t" in name:
            raise ValueError(f"Unsupported release filename: {name}")
        result[name] = p.read_bytes()
    for name in ("assets/sample-data/golden_dataset.csv", "scripts/R/lib/bootstrap.R", "scripts/R/install_nlss.R", "scripts/R/run_nlss.R"):
        if name not in result:
            raise ValueError(f"Missing required runtime file: {name}")
    return dict(sorted(result.items()))


def version_from(data):
    matches = re.findall(r'^nlss_version: "([^"]+)"\s*$', data.decode(), re.M)
    if len(matches) != 1 or not re.fullmatch(r"(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-[0-9A-Za-z.-]+)?(?:\+[0-9A-Za-z.-]+)?", matches[0]):
        raise ValueError("Expected one quoted semantic nlss_version in scripts/config.yml")
    return matches[0]


def build(repo, out):
    repo, out = Path(repo).resolve(), Path(out).resolve()
    if out == repo or repo in out.parents:
        raise ValueError("Release output must be outside the source tree")
    if out.exists() and (not out.is_dir() or any(out.iterdir())):
        raise ValueError("Choose a new or empty output directory; releases are never overwritten")
    payload = collect(repo)
    version = version_from(payload["scripts/config.yml"])
    # config.yml is the single version authority, including the built skill.
    skill, count = re.subn(r'(?m)^  nlss\.version: .*$', f'  nlss.version: "{version}"', payload["SKILL.md"].decode())
    if count != 1:
        raise ValueError("Expected one skill version metadata field")
    payload["SKILL.md"] = skill.encode()
    hashes = {name: sha(data) for name, data in payload.items()}
    identity = sha(json_bytes(hashes))
    payload[".nlss-release.dcf"] = f"Name: nlss\nVersion: {version}\nPayload-SHA256: {identity}\n".encode()
    # MD5 is a base-R copy/edit check, NOT a signature or authenticity guarantee.
    payload[".nlss-files.tsv"] = ("md5\tpath\n" + "".join(
        f"{hashlib.md5(data).hexdigest()}\t{name}\n" for name, data in sorted(payload.items()))).encode()
    metadata = json.loads((repo / "packaging/plugin.json").read_text())
    if metadata.get("name") != "nlss" or "version" in metadata:
        raise ValueError("Packaging metadata must name nlss and defer version to config.yml")
    metadata["version"] = version
    portable = {"$schema": SCHEMA, **metadata}
    codex = {**metadata, "skills": "./skills/", "interface": json.loads((repo / "packaging/codex-interface.json").read_text())}
    plugin = {f"skills/nlss/{name}": data for name, data in payload.items()}
    plugin.update({"plugin.json": json_bytes(portable), ".codex-plugin/plugin.json": json_bytes(codex),
                   ".claude-plugin/plugin.json": json_bytes(metadata),
                   "LICENSE": payload["LICENSE"], "NOTICE": payload["NOTICE"]})
    plugin["README.md"] = (f"# NLSS {version}\n\nLocal R research skill. "
        "[Installation and maintenance](skills/nlss/references/installation.md).\n\n"
        f"Payload SHA-256: `{identity}`.\n").encode()
    distribution = {f"nlss-release/plugins/nlss/{name}": data for name, data in plugin.items()}
    # Static release catalogues, never personal configuration or a service.
    distribution["nlss-release/.agents/plugins/marketplace.json"] = json_bytes({
        "name": "nlss-local", "interface": {"displayName": "NLSS local release"}, "plugins": [{
            "name": "nlss", "source": {"source": "local", "path": "./plugins/nlss"},
            "policy": {"installation": "AVAILABLE", "authentication": "ON_INSTALL"}, "category": "Productivity"}]})
    distribution["nlss-release/.claude-plugin/marketplace.json"] = json_bytes({
        "name": "nlss-local", "owner": metadata["author"], "plugins": [{"name": "nlss", "source": "./plugins/nlss", "version": version}]})
    distribution["nlss-release/README.md"] = (f"# NLSS {version} — release package\n\n"
        "[Install once for your harness](plugins/nlss/skills/nlss/references/installation.md).\n"
        "This catalogue is not registered or published automatically.\n").encode()
    try:
        head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo, text=True, stderr=subprocess.DEVNULL).strip()
        dirty = bool(subprocess.check_output(["git", "status", "--porcelain"], cwd=repo, text=True))
    except (OSError, subprocess.CalledProcessError):
        head, dirty = None, None
    inputs = {**hashes, "SKILL.md": sha((repo / "SKILL.md").read_bytes()),
              **{n: sha((repo / n).read_bytes()) for n in ("scripts/build_release.py", "packaging/plugin.json", "packaging/codex-interface.json")}}
    info = {"version": version, "payload_sha256": identity, "source": {"mode": "selected-working-tree",
            "git_head": head, "git_dirty": dirty, "files_sha256": inputs}, "archives_sha256": {}}
    out.mkdir(parents=True, exist_ok=True)
    for kind, files in (("skill", {f"nlss/{n}": b for n, b in payload.items()}), ("plugin", distribution)):
        path = out / f"nlss-{version}-{kind}.zip"
        with zipfile.ZipFile(path, "x", compression=zipfile.ZIP_DEFLATED) as archive:
            for name, data in sorted(files.items()):
                entry = zipfile.ZipInfo(name, date_time=(2020, 1, 1, 0, 0, 0))
                entry.create_system = 3
                entry.external_attr = 0o100644 << 16
                entry.compress_type = zipfile.ZIP_DEFLATED
                archive.writestr(entry, data)
        info["archives_sha256"][path.name] = sha(path.read_bytes())
    (out / "release.json").write_bytes(json_bytes(info))
    checksums = {**info["archives_sha256"], "release.json": sha((out / "release.json").read_bytes())}
    (out / "SHA256SUMS").write_text("".join(f"{value}  {name}\n" for name, value in sorted(checksums.items())), encoding="utf-8")
    return info


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()
    try:
        info = build(args.source, args.out)
    except (ValueError, OSError) as exc:
        parser.exit(1, f"Release not built: {exc}\n")
    print(json.dumps({"version": info["version"], "payload_sha256": info["payload_sha256"], "out": str(args.out.resolve())}))


if __name__ == "__main__":
    main()
