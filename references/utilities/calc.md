---
name: calc
description: Evaluate numeric expressions without a dataset, print plain/json/csv results, and preserve calculation reports and utility audit records; distinguish restricted calculations from explicitly unrestricted R evaluation.
license: Apache-2.0
---

# Calc (Utility)

## Overview

Use Calc for parameter derivations and quick numerical checks, not as a substitute
for a statistical analysis. It does not load or activate a dataset. It prints the
requested result format **and** appends the current project's root Markdown
protocol, with utility evidence in `.nlss/utility-runs/` and no extra JSONL log.

## Intent/Triggers

Examples include adjusting alpha, deriving f² from R², converting an effect-size
parameter, or checking a distribution quantile. The mathematical result does not
validate the assumptions or substantive interpretation of a chosen formula.

## Inputs and Script

Pass expressions and optional named constants to `scripts/R/calc.R`:

```bash
Rscript scripts/R/calc.R --expr "0.05/3"
Rscript scripts/R/calc.R --set "r=0.3|k=3" \
  --expr "d=2*r/sqrt(1-r^2)|r2=r^2|alpha=0.05/k" --digits 6
Rscript scripts/R/calc.R --expr "qnorm(0.025)|qnorm(0.975)" --format json
```

`|` separates expressions or assignments; it is not an escaped string delimiter.
Each item must parse as one R statement and return a nonempty, non-complex numeric
value. `--set` requires `name=expression` items. In `--expr`, assignments are
optional; unnamed items receive `expr_1`, `expr_2`, etc. Names start with an ASCII
letter and contain letters, digits, periods or underscores.

Assignments execute in order and become available to later expressions. Reusing
a name retains the last value in named stdout/JSON results; the audit also keeps
every expression row in execution order. Constants `pi` and `e` are available
in restricted mode.

## Options

Defaults come from `scripts/config.yml`; validated partial overrides use
`NLSS_CONFIG_PATH`, and CLI flags win.

- `--expr <text>`: required expressions, separated by `|`.
- `--set <text>`: optional named constants, separated by `|`.
- `--digits <n>`: finite integer from 0 through 15; default `defaults.digits`.
  Rounding affects presentation, not stored numeric values.
- `--format plain|json|csv`: default `modules.calc.format` (`plain`).
- `--unsafe TRUE|FALSE`: default `modules.calc.unsafe` (`FALSE`).
- `--template <ref|path>`: select a template key or file; an explicit missing,
  directory or malformed template is rejected before evaluating expressions.
- `--user-prompt <text>`: researcher intent retained in the log/audit context.
- `--log TRUE|FALSE`: default `defaults.log`; controls optional standalone logging, not current-project evidence or the root protocol.
- `--interactive`: prompt for these inputs.
- `--help`: print usage without calculation or publication.

Unknown or duplicate CLI flags, malformed Booleans, invalid numeric settings and
failed expressions return a nonzero exit status. A failed computation is not
published as a successful calculation. Numeric stdout is emitted after successful
publication; unrestricted expressions can themselves write arbitrary stdout.

## Restricted and Unrestricted Evaluation

Restricted mode exposes parentheses; `+ - * / ^ %% %/%`; and
`abs sqrt log exp round min max sum mean sd var pnorm qnorm pt qt pf qf`.
Other functions, namespace access, file/system access and general assignment
operators are not exposed. Explicit `name=expression` items supply numeric
bindings without exposing general R assignment.

The restriction is a small calculator vocabulary, **not an operating-system
sandbox for hostile code**. For example, general vector construction with
`c(...)` remains outside restricted mode.

Use `--unsafe TRUE` only when the researcher explicitly authorizes the particular
unrestricted expression. It retains full R evaluation in the process global
environment, including vector calculations, arbitrary functions, packages,
randomness and side effects:

```bash
Rscript scripts/R/calc.R --expr "pnorm(c(-1,0,1))" --unsafe TRUE --format json
```

Unrestricted evaluation is always marked as not deterministically reproduced
from the recorded inputs. A manually seeded expression does not authorize
automatic re-execution of its other effects. Utility publication protects its
own report/log files, not files, network requests or process state changed by
unrestricted R. The normal output location is resolved and checked for symlink
ancestors before expression evaluation; changing the working directory inside
an expression does not redirect the canonical calculation output.

## Outputs and Audit

Select `--project` or use the nearest current ancestor marker; there is no
child/sibling search. Unmarked standalone output uses `defaults.output_dir`. Calc does
not create a dataset, Parquet copy, active-dataset selection or planning folder.

- Stdout: `plain` prints `name = value`; `json` prints one JSON object;
  `csv` prints `name,value` rows. Numeric vectors retain the existing plain/CSV
  `c(...)` presentation and JSON array representation.
- `report_canonical.md`: append-only calculation table and narrative.
- `.nlss/utility-runs/<id>/request.json`: resolved expression/settings evidence,
  utility identity, code hash and R/environment metadata.
- `.nlss/utility-runs/<id>/result.json`: successful result values, ordered expression
  rows, warnings, value shapes and explicit nonfinite statuses.
- `.nlss/utility-runs/<id>/values.rds`: exact numeric results/constants, preserving
  vector attributes and the distinctions between NA, NaN and signed infinity.
- `.nlss/utility-runs/<id>/output.md` and `stdout.txt`: preserved deterministic
  presentation of this invocation's returned numbers.
- `.nlss/utility-runs/<id>/template.md`: exact template bytes when a template is used.

Nonfinite values preserve the legacy display (`NA` in plain/CSV, `null` in JSON).
They are not silently converted into finite estimates; the audit distinguishes
`NA`, `NaN`, `positive_infinity` and `negative_infinity`. JSON always uses the
required decimal point, including when unrestricted code changes R's `OutDec`.

The project publication lock protects canonical/log/manifest projections and
immutable bundle publication against ordinary failures. Only final published
directories are successful records. A `.pending-*` directory is diagnostic
evidence, not success; power-loss recovery and unrestricted external side effects
are not covered.

These records are explicitly `kind: utility`, without a fabricated dataset.
**Neither safe nor unsafe Calc records are accepted by automatic statistical
replay.** Safe calculations are deterministic given their recorded expressions,
bindings and execution environment; unsafe calculations make no equivalent
claim. Preserved arithmetic is evidence for semantic research reporting, not a
template-constrained final interpretation.

Authored expressions are retained exactly in the private audit. Quoted external
paths and path-bearing comments are masked in canonical Markdown and legacy
JSONL; apply appropriate access controls to private audit artifacts.

## Templates

The default is `templates.calc.default`, normally
`assets/calc/default-template.md`. CLI file/key overrides remain available.
The selected template is validated and frozen before expression evaluation,
then archived with its hash. An invalid explicit selection does not silently
fall back to the default.

`table.columns` supports `name`, `expression` and `value`.
`narrative.row_template` receives these keys plus `full_sentence`.
`expression_count` supplies the number of evaluated expression rows. Standard
note/narrative token controls remain available. A separate calculation-scope
notice cannot be suppressed by a custom presentation template.

## Non-Goals and Dependencies

Calc is a utility, not a subskill, metaskill, dataset loader or model estimator.
Its numerical functions are base R/`stats`; the shared configuration, formatting
and audit layer requires `yaml`, `jsonlite` and `digest`.
It does not provide automatic replay, a general sandbox, or validation of a
researcher's chosen effect-size conversion or inferential design.
