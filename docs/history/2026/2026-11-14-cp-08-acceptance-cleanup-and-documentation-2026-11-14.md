## CP-08 — Acceptance, cleanup and documentation (2026-11-14)

Implemented the [current acceptance procedure](../../common-politics-scenario/acceptance.md).
The full gate and national campaigns are complete under the clarified same-votes
criterion. CP-08 and its parent merger are closed; final evidence follows below.

Retired the superseded previous-law/reform/fast workspace and editor trees,
M00 worker dispatch, old politics draft writers, the old default-model loader and
the obsolete candidate-UI build flag. The shared numerical implementation pins
remain enforced. The former M00 scenario/default and old transport/file harnesses
are isolated under `web/tests/fixtures/retired-*`, with no application imports.
The independent regional worker now uses `workspaces/regional-runner.ts`.
Production preparation no longer generates or receipts the M00 default; the
current full offline preparation passed with **19 required artifacts** and
receipt SHA-256 `391a21d8b0d3993602be5d4cfda5e3c57643a01c095e9cd66d45c31c47847162`.
Authored politics pins, model laws/versions and frozen legal/checksum fixtures
were not changed.

Retargeted browser checks to the common draft, strict files and durable session
index. Chart/diagnostic tests keep their detailed synthetic saved-table evidence;
old schema support is confined to test-only harnesses. The public result service
always uses the strict common reader, including without the former opt-in flag.
The production gate now runs the complete general/editor/national file matrix at
both `/elezioni` and `/`, rejecting skips, TODOs, expected failures and retries.
Its explicit test-process bound also applies to dedicated editor/unified checks.

Acceptance review found and repaired these integration gaps:

- The common result view now restores the lazy single-simulation reform seat
  chart, querying at most 1,000 saved rows for one requested simulation. Its
  tested presentation helper resolves names from the immutable common scenario.
  A zero-prefix interrupted session no longer queries a nonexistent result table.
- The common editor now exposes **Attiva/Disattiva Altre liste** with an explicit
  choice of free components/pools, fixed identity/color, linked initial chambers
  and complete per-law filings. No component is silently reassigned. Real worker
  checks verify a positive residual vote reference with zero seats for all three
  supported pairs, plus reload and fresh reactivation. Regional controls retain
  their independent behavior.
- Rosatellum's retained scrutiny requires the residual category's canonical
  label, whereas the common scenario uses `__other__`. Its adapter now translates
  that identifier at the legal boundary and restores stable IDs in presentation;
  the scrutiny guard and allocation rules remain unchanged. Tests distinguish a
  known zero result from missing saved evidence.
- A Run requested after failed preparation retains the original diagnostic and
  dispatches no simulations. The exploratory all-zero sole-filed residual case
  remains an infeasible M01 override; it was not converted to missing support or
  repaired by changing the model. The positive-reference case tests the approved
  candidate-free exclusion directly.
- The static acceptance server now resolves `politics.html` when the adjacent
  `politics/` directory exists for `politics/editor.html` but has no index page.
  Extracted historical transport tests retain their invalid-request error replies.
  These are local acceptance fixes, not hosted Pages acceptance.

Current workspace, preparation, user, residual-category and M01/M02 workflow docs
now point to the common editor/lifecycle/files and one acceptance procedure.
D19 comparisons retain the CP-01 corpus and fixed-input legal regressions without
keeping a second application architecture. Candidate-count fraction conversion
was extracted from the old editor while preserving its arithmetic order. No
M00-to-M01 distribution equivalence or model improvement is claimed.

Focused verification before the final gate: all **four** regional/common residual
browser cases passed at `/elezioni`; the dedicated editor checks passed **3/3**,
and national scenario/result/archive checks passed **6/6** across Chromium and
Firefox before the final residual follow-up. A one-draw installed-Chrome resource
smoke passed with process sampling and complete archive import/re-export byte
comparison. This smoke is not a 1,000-run campaign. Current preparation/application
types and **1,321 unit tests** passed; the authoritative two-base gate and campaign
outcomes follow below. Svelte tooling reported no issues on the changed common
subjects/results components, and mobile viewport inspection found no overflow.

The national harness preserves exact execution identities, actual workers,
complete/partial/failed membership, original denominators and legal error reasons.
It records all 1,000 actual legal outcomes and separately records post-execution
archive timing/memory and every decoded-byte digest, normalizing only export time.
The initial all-complete criterion was superseded by the clarification below;
failed membership and original receipts remain explicit. No legal draw is rerolled.

### CP-08 final acceptance (2026-11-15)

Luca clarified that `Non ho a chi dare il seggio eccedente` errors are acceptable:
the target is the **same political output, including expected errors, given the
same votes**. Updated the guidelines, acceptance guide and campaign policy
accordingly. The policy permits the retained Rosatellum Camera diagnostic only
with its original execution-error code and scrutiny-failed reason, preserves
failed membership, and rejects unexplained failures or inconsistent membership.
Three new policy tests cover accepted errors and rejected changes, laws/chambers,
missing diagnostics and mismatched membership. The scrutiny cores, frozen legal
inputs, expected outputs and checksum manifests remain unchanged.

The complete production gate passed at **both `/elezioni` and `/`**, with all
required production data, preparation/application types, **1,321 unit tests at
gate time**, and **152 browser checks**: 67 general, 3 editor and 6 real national
Chromium/Firefox checks per base. There were no skips, TODOs, expected test
failures or retries. Its receipt is `data/derived/cp08-acceptance/report.json`.
After restoring an inadvertently replaced pre-existing presentation test and
adding the acceptance-policy tests, the final full suite passed **1,325/1,325**,
with no skips/TODOs, and both type checks passed with **0 Svelte errors/warnings**.
The final unit receipt and type logs are `data/derived/cp08-final-*`.

Ran the three national campaigns **sequentially**, with the authored fitting and
simulation seeds and automatic worker policy, using installed Chrome
**157.0.8103.79** on this machine (28 reported processors, 32 GiB device-memory
hint). Each includes browser preparation, generation, both chambers' scrutiny and
durable results. No campaign exceeded 1,000 draws or rerolled an error.

| Pair | Actual workers | Complete / partial / failed | Preparation (s) | Fitting + execution (min) | Sampled private / working-set peak (GiB) |
| --- | ---: | --- | ---: | ---: | --- |
| Rosatellum/M01 | 1 | 981 / 0 / 19 | 5.56 | 10.69 | 1.457 / 1.492 |
| Reform/M01 | 2 | 1,000 / 0 / 0 | 5.54 | 12.28 | 2.415 / 2.440 |
| Reform/M02 | 4 | 1,000 / 0 / 0 | 28.18 | 17.75 | 3.429 / 3.398 |

All pairs meet the under-60-minute fitting-plus-execution target and have no
infrastructure failure, page error or renderer crash. Rosatellum's 19 failures
all retain the same Camera error at simulation IDs **35, 137, 167, 174, 263, 291,
307, 391, 431, 458, 486, 487, 520, 586, 644, 732, 805, 869, 875**. The campaign
finished and its archive roundtrip passed before the clarification; its original
receipt remains `state: failed` because the initial assertion required 1,000
complete outcomes. Reassessed that saved evidence under the clarified criterion
without running another draw. The two reform receipts passed directly. This
acceptance does not relabel failed legal outcomes as successful elections.

Each complete archive was exported, imported and re-exported, preserving exact
execution and session membership. Re-read both saved archive copies in a separate
final assessment and matched every decoded byte, normalizing only export time:

| Pair | Decoded archive bytes | Export/import/re-export (s) | Sampled private / working-set peak during archive check (GiB) |
| --- | ---: | ---: | --- |
| Rosatellum/M01 | 135,233,412 | 53.66 | 0.783 / 0.894 |
| Reform/M01 | 3,595,452,519 | 382.34 | 1.360 / 1.468 |
| Reform/M02 | 2,867,655,237 | 368.72 | 1.739 / 1.834 |

These additional archive timings are separate from fitting/execution. Memory
figures are sampled whole-renderer Windows private/working-set bytes, including
nested workers and native allocations; they are neither JS heap nor guaranteed
maxima. Reform/M02 stores exactly one immutable checkpoint, no inline checkpoint,
and a **1,170,554-byte manifest**. Resource/quota failure retention, cancellation,
interrupted recovery, worker-count determinism and session transitions also
passed in the production gate's focused lifecycle suites.

The [tracked evidence summary](../../common-politics-scenario/acceptance-2026-11-15.json)
contains SHA-256/byte fingerprints of the original receipts and both compressed
archive copies, decoded archive digests, exact model/law/adapter/data/build/RNG
versions, seeds, worker provenance and the explicit Rosatellum reassessment.
Full receipts and archives remain under `data/derived/cp08-resources/`;
the final assessment did not overwrite them. Fixed-input legal and CP-01
comparison regressions passed in the full unit suite, preserving same-vote
outputs and expected errors without requiring old seeded generation parity.
`git diff --check` passed and frozen `test/fixtures/` has no changes.

Marked CP-08 and the parent merger complete. Deferred D14 editors, performance
above 1,000 draws, legal/model corrections and public deployment remain separate.
No commit, deployment or manual UI acceptance is claimed.
