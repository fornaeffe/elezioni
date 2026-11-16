# November 2026 model acceptance

This is the production follow-through to the
[chamber-aware research](../research/vote-generation/campaign-2026-11/results.md).
It does not reclassify the six-family historical scores as full-list predictive
validation. Receipts live under ignored `data/derived/model-acceptance-2026-11/`;
the curated JSON beside this document records their identities and outcomes.

## Model decisions

| Model law | Adoption decision |
| --- | --- |
| M01 2.0.0 | Accepted for both laws; unchanged law and seeded CP-01 evidence. |
| M02 3.0.0 | Accepted for both laws; shared field replaces 2.0.0. |
| M06 research-v1 | Rejected at the full-component law gate; not offered in the UI. |

M06's research adapter assigns whole tickets to five named families and changing
Other. It never splits joint tickets. That supplies observable six-family vectors,
but does not supply separate vectors for the production component basis. The
installed selection has 30 fitted components; **each of its eight elections** has
joint national groups and incomplete municipal component vectors. The published
endpoint error/covariance and municipal-state calculations cannot be transferred
to that basis without a new grouped-data inference rule. Missing and new
components also need explicit endpoint support rules. No equal/proportional
historical count split was introduced to manufacture a pass. Browser resource
acceptance was therefore not entered for M06. This rejects adoption of the
currently specified law, not future grouped-data Student-t models.

M01 remains the accepted marginal/tail-risk baseline. Its research chamber gaps
remain a qualification. M02 assumes **fixed paired and reference-induced chamber
contrasts through future movement**. The research's age-confounded out-of-time
contrast test is not calibrated evidence for future same-age elections. Neither
model is certified here for future joint-bonus probabilities.

## Full-panel checks

- `full-panel.json`: exact source identity, 30-component basis and the eight
  original grouped-history boundaries underlying M06's rejection.
- `camera-parity.json`: current M02 versus isolated pre-adoption Git source on
  all 30 historical components, all offered, no explicit references. Camera
  integer counts match exactly for draws 1, 2 and 7. This is a marginal-law
  preservation check; Senate and filing/reference extensions deliberately change.
- `shared-full-panel.json`: the actual default's 36 future components, new
  components and joint lists under both laws; nonnegative integer counts and
  elector bounds; gap sensitivities 0/1/2; removal of a physical Camera filing
  leaves every Senate count identical. Explicit chamber references remain active.
- `shared-field.test.ts`: zero-gap long-horizon coupling, distinct explicit
  chamber references, a Senate-only new component with explicit Camera zero,
  and single-chamber renewal. Existing grouped-fit, physical allocation,
  deterministic-worker and fixed-vote legal regressions remain required.

The sensitivity receipt reports conditional elector-share gaps with actual
filings and overrides. It is not a held-out signed-difference coverage test.

## Browser, legal and resource gates

Run from `web/`, after `npm run data:prepare`:

```powershell
node --import tsx scripts/acceptance/model-panel.ts
node --import tsx scripts/acceptance/shared-model-parity.ts
node --import tsx scripts/acceptance/shared-model-acceptance.ts
$env:ACCEPTANCE_TEST_WORKERS = '2'
$env:ACCEPTANCE_OUTPUT = '../data/derived/model-acceptance-2026-11/production-gate'
npm run acceptance:reform
```

The production gate requires all generated bundles, both type checks, all unit
tests with no skips, and `/elezioni` plus `/` static builds. The common worker
and lifecycle matrices cover M01 and M02 for both laws, including one/two-worker
equivalence, exact checkpoints and archive roundtrips. The independent regional
and existing full national browser workflows remain included.

Run all four current-source resource campaigns sequentially, with no other build, fit or suite:

```powershell
$env:CP_RESOURCE_OUTPUT = '../data/derived/model-acceptance-2026-11/resources'
$env:CP_RESOURCE_SIMULATIONS = '1000'
$env:CP_RESOURCE_MODEL = 'M01'
$env:CP_RESOURCE_WORKERS = '4'
$env:CP_RESOURCE_LAW = 'politiche-r-parity-v1'
npm run benchmark:common-politics
$env:CP_RESOURCE_LAW = 'politiche-reform-2026-v1'
npm run benchmark:common-politics
$env:CP_RESOURCE_MODEL = 'M02'
$env:CP_RESOURCE_LAW = 'politiche-r-parity-v1'
npm run benchmark:common-politics
$env:CP_RESOURCE_LAW = 'politiche-reform-2026-v1'
npm run benchmark:common-politics
```

These use four explicitly requested workers on the measured 28-thread, 63.8 GiB
host. The browser's capped memory hint makes the automatic Rosatellum estimate
more conservative; the automatic policy is unchanged.

Each includes automatic browser fitting, generation, both scrutinies and durable
results, then archive export/import/re-export verification. Fitting plus execution
must finish in under 60 minutes. Use authored seeds; preserve all simulation IDs,
legal failures and original denominators. The
[existing legal-error criterion](common-politics-scenario/acceptance.md#national-performance-campaigns)
continues unchanged. Renderer memory samples include workers and native
allocations; they are not guaranteed maxima or isolated model workspace.

The [15 November M01 receipts](common-politics-scenario/acceptance-2026-11-15.json)
remain historical evidence for its unchanged law. Their source bundle differs
from the current November bundle, so all four law/model pairs receive fresh
1,000-draw campaigns here. No old timing is reclassified as a current-source run.

## Recorded outcomes

Local adoption completed on 16 November 2026. Both type checks and complete
production data preparation/checking passed. All **1,346 unit tests** and
**168 browser checks** passed without skips or retries. The six browser receipts
are 73 general + 3 checkpoint/editor + 8 national checks at each base path.
The [curated evidence](model-acceptance-2026-11.json) pins their inputs, reports,
implementation identity, campaign receipts and archive digests.

All four fresh campaigns used **four workers and 1,000 requested IDs**:

| Model | Law | Fit + execution min | Archive check min | Sampled private GiB | Legal outcomes |
| --- | --- | ---: | ---: | ---: | --- |
| M01 2.0.0 | Rosatellum | 3.10 | 0.33 | 3.27 | 985 complete; 15 approved failures |
| M01 2.0.0 | Reform 2026 | 6.75 | 3.49 | 4.06 | 1000 complete |
| M02 3.0.0 | Rosatellum | 12.52 | 0.27 | 1.70 | 998 complete; 2 approved failures |
| M02 3.0.0 | Reform 2026 | 15.09 | 3.41 | 3.16 | 1000 complete |

Memory is the largest recorded renderer-private sample across execution and
archive work, including workers/native allocations; it is not a guaranteed
maximum. Archive time is separate from the under-60-minute fitting/execution
gate. Every decoded archive byte except the export timestamp matched after
import/re-export. There were no infrastructure failures, page errors or crashes.

The Rosatellum failures are the previously approved Camera surplus-seat path;
their failed membership, exact reasons, IDs and 1,000-ID denominators remain in
the receipts. They were not reclassified as complete elections or rerolled.
Both reform campaigns completed all 1,000 legal outcomes.

The project-path browser and checkpoint stages passed before stale national
archive-disclosure locators were repaired. The remaining stages were completed
on the same data/computation build, with pinned-input and compiled-identity
checks. `production-gate/report.completed.json` consolidates all passing stage
reports; the initial failed driver report remains preserved. This continuation
changes no acceptance criterion.

## Version and packaging boundary

Both law selectors use the exact public registry. M02 2.0.0 is no longer offered
or executed, and is never silently relabelled 3.0.0. Old unsupported scenario and
checkpoint identities fail closed. Fresh defaults retain reform/M02, now 3.0.0,
and saved Rosatellum/M01. Both laws declare exact M01/M02 parameter pins.

For a future model-law update, use
`node --import tsx scripts/workspaces/refresh-model-pins.ts`, review the authored
default diff, then run `npm run data:prepare`. Generated releases/defaults are
not edited by hand. The historical M02 specification and CP-01 M02 receipt
remain evidence of 2.0.0; only the unchanged M01 records are replayed for parity.
Protected model drafts and frozen historical scrutiny fixtures remain untouched.
