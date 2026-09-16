# History and utility-publication contracts

Registered in `tests/tests.yml`. Run `run_history_tests.R` for standalone
historical-log readers and `utility_contract.R` for the shared publication
boundary. Both accept `--root`, `--keep`, `--match`, `NLSS_TEST_ROOT` and
`NLSS_KEEP_RUNS`; zero retention means no automatic pruning.

History fixtures are created independently of NLSS producers. They cover
MD5/XOR checksum versions 1–3 with LF and CRLF, positional/environment CLI
compatibility, malformed JSON/blocks, exact UTF-8 report reconstruction,
repeated semantic-report versions, legacy block routing, idempotence,
explicit overwrite, source/output collisions and symlink protection.
Successful checksum extraction is not numerical replay or a digital signature.

Utility publication checks cover all four callers, immutable evidence and
artifact hashes, exact RDS values, partial/failed retrieval states, callback/
artifact/final-rename failures, project locks, manifest rollback, path bounds,
root/projection symlinks and retained recovery bytes when rollback is incomplete.
These tests prove ordinary handled-error behavior, not power-loss recovery.

Keep legacy module runners for independent integration with real NLSS-produced
logs. Stateful runners require separate disposable source copies.
