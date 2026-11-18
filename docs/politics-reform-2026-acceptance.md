# Production application acceptance

The [CP-08 procedure](common-politics-scenario/acceptance.md) owns current
preparation, the complete two-base production gate, three supported pairs,
sequential national campaigns and strict receipt requirements. Use it for
commands and report paths. Public deployment remains a separate task.

The [application map](politics-reform-2026-application.md) identifies execution,
files, lifecycle and result documentation. Frozen legal/checksum regressions
remain blocking; no skipped production data or debug fallback is accepted.

## Diagnostic frequencies

The result-service `diagnostics` query streams the saved **Warnings and errors**
ledger after execution. It requires a native per-simulation ledger; imported
archives are rejected because older aggregations retain only the first simulation
ID and cannot establish distinct-run frequencies. Reports separate `errors`, `warnings` and `information`.
Each exact `(code, message)` reason records occurrence count, distinct affected
simulations, context occurrences, and frequencies per requested/processed run.
The additional `codes` summaries merge reasons while counting each simulation
once per code; distinct-run counts must not be obtained by summing reason counts.
Repeated notices within one simulation count once toward its frequency, while
retaining all occurrences. Different reasons can overlap in the same run, so
their frequencies need not sum to the total failure rate. Context notices have
no simulation ID and contribute no simulation frequency. No message normalization
merges distinct reasons. Session execution/storage failures remain in manifest
warnings and fail benchmark completion; they are not legal per-run failures.
The post-run query/delete interval is outside measured execution latency.

## Scope and legal evidence

Acceptance covers domestic allocation and candidate proclamations, conditional
on supplied/generated preferences and selected chamber profiles. Synthetic
candidate identities are not individual forecasts; sensitivity profiles are not
calibrated uncertainty intervals. Non-computable runs retain the original
requested denominator, with no reroll. Acceptance does not extend to foreign
seats or later supplementary elections.

The census-based I08 tables remain labelled provisional. Official election-specific
seat decrees and legal/model corrections require separately reviewed versions.
The retained previous-law Camera flipper dead end must be assessed legally;
an expected-failure regression cannot establish a successful national campaign.
See migration history for dated source review and measured acceptance evidence.
