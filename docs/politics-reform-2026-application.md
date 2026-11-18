# Politics application integration

Both `/politics` and `/politics/editor` use the common scenario, editor,
controller and durable result service. Fresh startup selects reform/M02;
Rosatellum/M01 and reform/M01 are also supported. Regional elections keep their
independent route, scenario and worker. Missing production data fails explicitly.

| Task | Guide |
| --- | --- |
| Use the editor, switch laws/models and run | [User workflow](politics-reform-2026-editor.md) |
| Scenario validation and inactive declarations | [Common contract](common-politics-scenario/contract.md) |
| Preparation, exact dependencies and law adapters | [Execution](common-politics-scenario/execution.md) |
| Automatic fitting, workers, persistence and recovery | [Lifecycle](common-politics-scenario/lifecycle.md) |
| Scenario, result, archive and checkpoint import/export | [Strict formats](common-politics-scenario/files.md) |
| Tables, charts and counting definitions | [Results](politics-reform-2026-results.md) |
| Production checks and national campaigns | [CP-08 acceptance](common-politics-scenario/acceptance.md) |
| Scrutiny algorithms and legal references | [Legal core map](../web/src/lib/politics/reform-2026/README.md) |

Structured result exports retain the immutable executed scenario, worker result
and exact execution provenance. Importing or browsing results leaves the draft
unchanged; only loading a session's scenario replaces it. CSV remains table-oriented.
Old politics files are rejected; there is no automatic converter.

All politics pairs use bounded workers and disk-backed result chunks. Cancellation
or infrastructure/storage failure retains the committed prefix and original
denominator. Legal failures remain distinct, with no reroll or renormalization.
The former Rosatellum 1,000-run UI ceiling is removed; measured acceptance remains
limited to 1,000 simulations per pair.
