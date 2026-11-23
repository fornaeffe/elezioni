## 2026-10-23 — Selected poll archive and reconciliation pilot

Added the approved three-substep poll task immediately after completed migration
steps. Completed only import/reconciliation; poll-informed live references and
historical bias calibration remain open. The focused code/operation map is in
[`polls-preparation.md`](../../polls-preparation.md).

Selected the government archive rather than YouTrend aggregates: the seven
user-supplied PDFs retain individual pollster metadata and cost only 117,701
bytes. Together with reviewed metadata, question translations and entity aliases,
the selected Git source set is 163,233 bytes (159.4 KiB), below its enforced 1 MiB
budget. `.gitignore` allows only those selected files and `.gitattributes` pins
JSON LF/PDF binary bytes. Other downloaded sources and derived output stay ignored.
Fresh clones verify and prepare this archive offline; there is no recurring
website scrape, PDF dependency or poll-specific executable parser.

Eight reviewed questions span fieldwork 6–19 October 2026 and five pollsters.
There are seven survey groups and three conditional questions: LAB21's reform
question and SWG's two leader alternatives. These groups prevent alternatives
from acquiring separate sample sizes; they do not assert independence between
repeated waves. Coalition totals/nonresponse remain source metadata, simulated
seats and unrelated questions are excluded, and Eumetra's attached cross-page
table is transcribed. Effective question sample sizes remain unknown rather
than silently becoming full-survey N. Repeated labels and the substring shared
by the Italian start/end-date headings made automatic field extraction unsafe;
the retained metadata was reviewed/corrected against the PDFs, with distinct
start/end dates and known source totals asserted in tests.

The common typed helper reuses verified aliases and normalized historical
correspondences, sums many-to-one mappings and records inferred splits. Missing
support becomes an explicit residual, allocated by available baseline weights
or equal eligible-list weights. The reviewed FN/Lega proxy is an assumption,
not measured voter transfer. Present FN keeps its polled support; missing
geography uses a recorded donor/uniform-filed-support recipe for substep 2.
Corrupt sources, malformed contracts and absent legal target support still fail;
missing political evidence does not make reconciliation non-computable.

`data:polls:check` verifies every selected byte and produces 16 separate ordinary
Camera/Senato previews against tracked defaults, with 86 estimated assignments.
`data:polls:generate` writes immutable observation/audit/preview artifacts under
`data/derived/polls/`. Observation version is
`poll-observations-v1-e2965f1e70d1`; preview version is
`poll-reconciliation-v1-46440e3c9230`. Two generations retained identical SHA-256
values: observations `50ce506230750e241a01fa695a3432f3bd13ec24b33559325919c75f5cf306e5`,
preview `d53f1e95376068e86de2037f558e1e51fb341a1aa06289d6d42e9e6616fd01dd`.
All 16 vectors were finite/nonnegative and conserved total support to 1e-12.
The preview applies no live shares/turnout, alters no scenario/default and makes
no accuracy/calibration claim. Special-territory inputs keep their own boundary.

Verification on Windows/Node 24: 14 focused tests passed, including an isolated
source-only copy with networking forbidden, checksum corruption, size-budget
failure, conditional questions, rounding/missing totals, mapping precedence,
splits, proxies and mass conservation. `npm run check:data` and `npm run check`
passed (zero Svelte errors/warnings). The full suite passed 939 tests in 67 files
without skips (13.52 s). `npm run build` passed; the subsequent
`npm run test:e2e` passed all 29 root-base Chromium tests (28.5 s). Chunk-size,
plugin-timing and terminal-color notices were informational. No UI/worker/legal
generation changes, commit or publication were made.
