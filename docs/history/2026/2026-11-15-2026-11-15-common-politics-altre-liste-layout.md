## 2026-11-15 — Common politics Altre liste layout

Replaced the duplicated residual fieldset and ordinary list row with one
dedicated row. Activation uses a searchable explicit checkbox selection; active
options expose existing memberships, advanced references and coverage, and
deactivation explains dependent removal before confirmation. Fixed identity,
linked initial chambers and validated domain edits are unchanged. The coverage
tree omits candidate counts for this candidate-free category.

Verification: `npm run check` passed without diagnostics; production builds
passed at `/` and `/elezioni`. The focused Other-list Playwright suite passed
4/4 at each base, covering activation, search, narrow layout, simulations,
persistence and reactivation. An additional `/elezioni` run of the Rosatellum
case passed with membership and coverage panel checks. An initial run exposed
the test's stale scrutiny-control label; updating it to the existing **Legge
elettorale** label resolved the timeout.
### 2026-11-15 — Collapsible result import and selection

- Grouped result import, law/model filters, session selection, loading the
  selected session's scenario, and JSON.gz/CSV downloads under a collapsed
  “Caricamento, selezione e download dei risultati” section.
- Kept result summary and tables outside the section so the selected results
  remain visible when it is collapsed.
- Verification: `npm run check` passed with zero errors and warnings; the Svelte
  autofixer reported no issues. No browser review was performed.

### 2026-11-15 — Uniform collapsible results sections

- Made general information and simulation messages collapsible and closed by
  default. Matched their summary styling to the other results panels.
- Moved the bonus winners panel ahead of elected averages and left it open by
  default. Made the elected averages and valid-vote percentages a single open
  collapsible section.
- Moved boxplot grouping and chamber controls into the boxplot section, which
  is closed by default; the chart body is mounted when that section opens.
- Verification: `npm run check` passed with zero Svelte errors and warnings;
  the Svelte autofixer reported no issues in the changed statistics component.
  No browser review was performed.

### 2026-11-15 — Close affected simulation list

- Added a close button to the affected simulations panel in simulation messages.
  Closing clears the selected message and its detail filters, returning to the
  message summary.
- Marked the migration plan item complete.
- Verification: not run.

### 2026-11-15 — Archive import/export performance and progress

- Implemented the approved assessment for migration-plan lines 139–140.
  Scalar table rows now use bounded framing and native parsing; object headers
  retain the duplicate-key-rejecting reader. Export buffers small strings before
  UTF-8/gzip. Archive scans batch IndexedDB reads within a 4 MiB target (at most
  16 chunks), while ordinary pages still fetch one chunk and uncommitted
  sequences remain invisible.
- Added an operation-owned two-worker decompression pool for stored archives
  at least 2 MiB on browsers reporting four or more logical processors. Ordered
  replies, retained compressed inputs, serial retry on failures/timeouts and
  cleanup preserve the existing staged publication boundary. Storage writes
  and the outer archive gzip stream remain ordered. No schema or legal/model
  algorithm was changed. Four workers added little benefit in the exploratory
  100,000-row comparison, so production uses two.
- Request-scoped progress reports input bytes, rows and verification/export
  totals through the existing application status panel, throttled within each
  phase to ten updates per second. Validation, saving and file finalization
  remain explicit phases; completion follows durable saving/export registration.
- Added `npm run benchmark:archives` and the focused task map and reproduction
  commands in [archive performance](../../archive-performance.md). Chrome
  `157.0.8103.79`, three sequential trials, constructed M01 archive with
  1,000,000 rows / 16 cells each, 154,364,079 decoded input bytes and 20,840,845
  gzip bytes. Baseline source was
  `d21b119777b1ef4cc05f9110003f67b56debb591`.

  | Median operation | Baseline | Optimized serial | Optimized two workers |
  | --- | ---: | ---: | ---: |
  | Import, including storage and membership validation | 16.890 s | 7.657 s | 7.466 s |
  | Export through gzip and OPFS | 11.164 s | 4.052 s | 3.698 s |

  Final import is 2.26 times faster; export is 3.02 times faster. Two workers
  improve export another 8.7% over the optimized serial path, and import 2.5%.
  Every trial verified import/export/reimport/reexport row counts and SHA-256
  of all decoded bytes, normalizing only export time. Receipts are under
  `data/derived/archive-performance/`: `baseline-1000000.json`,
  `serial-1000000.json`, `final-1000000.json`, plus 100,000-row exploration.
  These are constructed transfer measurements, not a new national M02 campaign.
- Sampled renderer-wide private peaks were 1.923 GiB baseline and 2.061 GiB
  optimized (181 and 80 samples). They include synthetic input construction,
  whole-file hash verification, workers and GC; they neither isolate codec
  memory nor establish a hard memory maximum. Serial optimized peak was
  2.222 GiB. The bounded operation buffers are separate implementation limits.
- Fixed an existing standalone M01 `.json.gz` result-import error: after
  decoding, its temporary JSON archive now has a `.json` filename rather than
  being decompressed twice. Added browser regression coverage.
- Verification: 120 Vitest files / 1,332 tests passed with
  `npm test -- --maxWorkers=4 --reporter=dot`; `npm run check` reported zero
  errors/warnings; `npm run check:data`, final `npm run data:check` and
  production builds at `/` and `/elezioni` passed. Focused browser suites
  `common-politics-files`, `common-politics-lifecycle`, `result-store` and
  `politics-file-isolation` passed 12/12 at both `/` and `/elezioni`, including mounted progress,
  all three law/model archive/checkpoint roundtrips, interruption/quota recovery,
  draft isolation, regional routing and prefetch hiding uncommitted chunks.
- Initial `data:check` correctly rejected stale generated release artifacts
  with `RELEASE_IMPLEMENTATION_MISMATCH`: the repository's transitive code hash
  includes archive dependencies. Ran the prescribed `npm run data:prepare`,
  which passed with 19 artifacts and receipt SHA-256
  `a10c3e13920ca5325809fa95b17913ee8cca36de78bc9fc5aca886e6978b2565`.
  No generated defaults were edited by hand. The broad 70-test browser suite
  was attempted, then interrupted after existing editor label/layout selectors
  failed (`Algoritmo di scrutinio`, the former non-valid reference section and
  mobile status). It is not reported as passed. Updated only the relevant
  transfer lifecycle/draft-isolation tests for current control names and closed
  sections; broader editor-test maintenance remains separate. No claim of full
  release acceptance or a new 1,000-simulation national campaign is made.
