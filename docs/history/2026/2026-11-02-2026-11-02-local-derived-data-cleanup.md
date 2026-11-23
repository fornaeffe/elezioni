## 2026-11-02 — Local derived-data cleanup

Reviewed all regular files and links under `data/derived/` against active
preparation/test readers, candidate/cache dependency graphs, documented legal
investigations, completed result archives and retained benchmark evidence.
Distinguished CLI/test output destinations from actual input dependencies.
The inventory excluded junction targets, which otherwise inflate directory
counts by including the live `web/node_modules/` tree more than once.

Retained **673 regular files, 3,304,967,601 bytes** (about 3.08 GiB): production
intermediates, the local candidate's complete closure, relevant preparation/model
caches, acceptance summaries and final archives, poll/reference outputs and
legal replay evidence. The installed runtime candidate and the local preparation
candidate remain distinct; neither was changed or selected by cleanup.
Frozen fixtures, authoritative sources, curated research, model drafts and
application/legal/model code were preserved.

Automatic approval review rejected preparation of the deletion script with
`blocked by policy`. Used the user's authorized fallback: moved **15,866 regular
files, 18,226,313,861 bytes** (about 16.97 GiB), plus **five links**, to
`C:\Users\forna\Repos\elezioni-derived-cleanup-20261102`. Nothing was permanently
deleted. Superseded checkouts/builds/toolchains, clean-run/source copies, obsolete
research/acquisition scratch, expanded CSV/replay outputs, old logs and SCN
worker spools are outside the repository. Empty directories were also moved.
The complete SCN gzip, selected full legal cases and compact verification remain
in place; reconstructing that historical archive with `--export-only` requires
restoring its worker spools first.

The external `manifest.json` records every retained/moved file, reason, original
path and identity; `execution.json`, `summary.json` and `verification.json` record
the move and checks. Manifest SHA-256:
`fed5b870e9db2fd30ea97dcf017d1850d9543e09cc0a991757ca6422d26b4a32`.
Checked every regular file's unchanged Windows file identity, size and modification
time after same-volume moves, plus SHA-256 for files up to 1 MiB. Large files were
not content-hashed. Moved junctions as links without traversing their targets;
live dependencies remained available. There were no unexpected remaining files
or empty directories at verification. Subsequent production checking refreshed
its normal `production/check-report.json` receipt.

Verification before and after cleanup: `npm run data:check` passed with the same
11-artifact receipt identity; **104 test files / 1,220 unit tests** passed with
two workers, without skips. The post-cleanup test run took 46.20 seconds.
Both optional research archive checks passed (212 archived files and 61 frozen
evidence files). `git diff --check` and changed-document local links passed.
No fresh browser/resource campaign or full data regeneration was needed or claimed.

Added the focused [retention/recovery guide](../../derived-data-retention.md) and its
data entry-point link. No migration directive, production default, legal policy,
commit, push or publication changed. Repository footprint was reduced; disk
space was not reclaimed because the fallback archive remains on the same volume.
