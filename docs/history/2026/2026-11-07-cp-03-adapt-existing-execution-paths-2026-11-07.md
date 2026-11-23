## CP-03 — Adapt existing execution paths (2026-11-07)

Implemented the common scenario's preparation/generation/assembly boundary under
`web/src/lib/politics/execution/`; the task map and decoded asset contracts are in
[the execution guide](../../common-politics-scenario/execution.md). It strictly dispatches
Rosatellum/M01, reform/M01 and reform/M02 through the CP-02 supported-pair registry.
Original-column sources, explicit common-18+ conditional electorates, catalogues,
reference geography and active law/model parameters are checksum/schema/version
checked before use. Inactive declarations retain structural validation without
loading inactive numerical/geographic bundles. Source/question/choice encoding is
normalized explicitly; poll edits retain observed counts and metadata. M01's
saved parameter date stays separate from the evidence cutoff.

Moved original-source M01 compilation, latest-publication polling, municipal list
shocks and college reconstruction into vote-only modules under `politics/generation/`.
Extracted M02 physical-cell generation behind the same count/trace boundary.
Existing reform entry points are thin envelopes around those implementations,
so CP-01 remains an executable unchanged baseline. Rosatellum receives M01 votes
without constructing a fake reform legal scenario. Its legal adapter validates an
explicit physical/uni/pluri/constituency/region crosswalk, conserves conditional
electorates and reuses the existing candidature sampler, reuse share and five-bin
candidature distribution. Candidate pin tokens and a collision-free generated
namespace preserve declared identities, including IDs that resemble generated IDs.
The fixed-input scrutiny implementations and accepted tie/failure behavior are
unchanged; M00-to-M01 statistical and generated-ID differences are the approved
D01 replacement, not an M00 distribution comparison.

CP-02's pluri-only Rosatellum filing vocabulary could not represent actual VdA
ballots or Senate TAA: the frozen geography has null proportional codes there.
Added statutory uni-only filing locations, retained their zero-seat/null legal
context rows and rejected uni filings that have an ordinary pluri parent. The
same eligibility helper now initializes new lists under every law, including all
eight chamber-specific uni-only locations. Ordinary college IDs are not remapped
on law switching; the previous-law grid does not acquire the reform Bolzano overlay.

Reform legal assembly retains its existing bonus reference plan, candidature,
preferences and supplied-evidence validators. Only the shared D11 resolver's
automatic/eligible explicit links enter assembly. Retained competing declarations
stay saved, eligibility restoration reactivates the original endpoints, empty
counterparts fail separate participation checks and single renewal emits no joint
links. Active declared candidates retain availability, biography, external
candidacies and compatibility checks.

Assessed cross-law support against M01's published contract and retained **2.0.0**
for both laws: the existing statistical law already consumes conditional
electorates and geographic filing context; the new law adapter adds no model
capability or probability parameter. Adapter `rosatellum-m01-v1`, scrutiny metadata,
implementation hashes, data pins and schema versions remain separate. Added
consumed-input preparation/generation/assembly keys plus an actual transitive
execution implementation hash. Draft name/color, inactive-law/model and candidate
edits do not invalidate unrelated statistical preparation. Workers check their own
compiled implementation ID and repeat common parsing, exact dependency checks,
supported-pair validation, preparation and assembly-key checks.

Added `init-politics` to the real compute worker/client. All three pairs reuse
one-simulation-at-a-time execution, pull-driven bounded encoding and the existing
ordered pool; the common boundary has no 1,000-run ceiling. Generation/legal
failures retain simulation/chamber context and error diagnostics without rerolling.
This is the execution adapter layer: authored defaults/packaging remain CP-04,
new files/drafts CP-05 and editor/controller/session integration CP-06/07. In
particular, this initial common packet repeats preparation per initialized compute
worker; shared prepared-state reuse and full resource campaigns remain later work.
The current workspace UI is transitional and is not an old-format converter.

During extraction the only initial CP-01 mismatch was JSON property order in the
recorded input digest; restoring the compatibility envelope's original order kept
the stored baseline unchanged. The first new Rosatellum browser replay comparison
also compared measured scrutiny time; the test now excludes only its two timing
columns, retaining every electoral result/diagnostic comparison. A read-only data
check correctly caught a stale implementation pin after the last source cleanup;
rerunning the complete production pipeline regenerated the pin and passed the
same checks. No frozen fixture, checksum manifest or legal expectation was refreshed.


Verification on Windows x64, Node 24.20.0, Intel i7-14700KF (28 logical
processors), Playwright 1.60.0/Chromium, from `web/`:

- `npm run data:prepare`: passed the complete pinned-source pipeline, final run
  124.90 s. `npm run data:check`: passed, 24.41 s. Production receipt SHA-256:
  `bcddec80635e6c7e2d3fc393764543fc48f78511db9d950a254d752d88c8dc23`.
  Existing defaults/catalogues were generated through their normal compilers;
  no generated default was edited by hand and their tracked bytes did not change.
- `npm run check:data`: passed. `npm run check`: zero errors/warnings.
- `npm test -- --maxWorkers=2`: 108 files, **1,282 tests passed**, no skips,
  56.80 s. This includes unchanged frozen checksum/legal failures and CP-01's exact
  14-case retained-generation/candidature report, plus the common executor's
  single-renewal, restricted-support, residual/new-list, manual-pin, D11,
  dependency/version, implementation-binding and invalidation checks.
- After the final nested model-schema guard and per-chamber timing correction,
  the focused common execution suite passed all 18 tests again (13.93 s).
  Timings use actual chamber observations; they are not part of deterministic
  electoral replay comparisons.
- `npm run build`: passed at `/` (final 4.94 s) and with `BASE_PATH=/elezioni`
  (final 5.27 s). Existing plugin-timing/chunk-size notices remain informational.
- With each matching build already prepared, `ACCEPTANCE_PREBUILT=1 npm run
  test:e2e -- --workers=2 --reporter=line`: **50/50 passed at `/`** (58.4 s)
  and **50/50 at `/elezioni`** (56.0 s), no retries/skips. These cover current
  workspaces, regional isolation, fixed legal paths, storage/cancellation and
  the actual common compute worker for every required pair.
- Final `common-politics-worker.spec.ts` rechecks after the last guards/timing
  change passed all three pairs at both bases (6.3 s / 5.9 s). Each actual worker
  initializes from checksum-pinned common inputs and replays IDs 7, 1, 7 through
  bounded transferable chunks, independently of wall-clock timing.
- Documentation link and whitespace audits, including `git diff --check`, passed.

Marked CP-03 complete and linked its API/adapter guide. CP-04–08 and the parent
workspace merger remain open. No 1,000-run campaign, final resource acceptance,
public deployment or commit is claimed by this step.
