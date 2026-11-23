## 2026-10-18: R26-08 electoral figures and preference rankings

Implemented only R26-08 under `web/src/lib/politics/reform-2026/`; R26-09
admission is the next step. The [module contract](../../../web/src/lib/politics/reform-2026/README.md#electoral-figures-and-rankings-r26-08)
documents calls, scopes and remaining boundaries. Consulted the
[promulgated act](https://www.gazzettaufficiale.it/eli/id/2026/10/09/26G00201/sg)
and [official comparison](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf)
against the scoped transcription/specification. This step does not change the
normative source version or claim the future annotated republication was checked.

### Delivered boundary

- `distributeBonusOnlyVotes()` implements N04 at the ordinary plurinominal
  college, using R26-06's exact quota/remainder primitive. The independent
  `(60,30,10), B=7` fixture gives quota `100/7`, initial additions `(4,2,0)`,
  quotient fractions `(1/5,1/10,7/10)` and final additions `(4,2,1)`.
  Already attributed totals bypass allocation and draw consumption. Mixed
  representations are rejected. Zero only-ballots and single linked lists
  avoid division; positive only-ballots with multiple zero-support lists return
  a non-computable outcome with no invented or partial totals. All groups are
  preflighted before draws; group order and tied alternatives are canonical.
- `aggregateElectoralFigures()` implements N06 over disjoint attributed ordinary
  and special uninominal college rows. It supplies list/coalition figures and
  exact shares at college, territory, region and national scope. National figures
  include VdA/TAA for admission, first place and bonus eligibility; ordinary
  allocation figures exclude both. All list ballots stay in denominators and
  coalition totals include sub-1% members. Special-only lists remain represented.
  Missing/extra colleges or incomplete/duplicate/non-contesting list rows fail
  explicitly rather than silently producing partial national figures.
- Camera TAA proportional figures are a view of its uninominal territorial sum,
  not an additional ballot partition. The envelope's TAA `ordinaryVotes` row
  is not supplied as an ordinary aggregation contribution; its slate continues
  to carry preferences. No second copy of those ballots enters national totals.
  Raw special candidate-only attribution remains N10/R26-10: callers must supply
  attributed special list totals or future N10 output, never add candidate totals
  to list figures. The functions require validated political/geographic references;
  they are legal-stage helpers, not a composed scrutiny entry point.
- `rankPreferenceCandidates()` implements N05: capolista excluded, preferences
  descending, filed order breaking ties without a draw. Missing preferences
  remain a separate outcome from explicit zeros. Filed unavailable candidates
  stay in this ranking for later proclamation/recovery stages to handle.
  `calculatePercentages()` uses all valid college votes (TAA constituency votes
  for its proportional slate), with exact fractions in percentage units. A zero
  denominator yields an explicit undefined percentage, while zero-vote counts
  themselves can still be aggregated. Unsafe scalar counts/sums throw; bigint
  protects intermediate percentage multiplication.
- Representation-equivalence tests for both chambers use matching candidate
  identities/preferences and final totals after a deliberately tied raw
  attribution. Separate attribution/downstream streams give exactly equal
  aggregates, rankings and downstream tie traces. This is an accounting and
  reproducibility check; it does not establish statistical absence of bias.
  I03 ties retain simulation-specific warnings and JSON-safe traces.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  Final `npm run check`: **zero errors/warnings**. `npm run test`:
  **32 files / 351 tests passed**, 4.50 s, including **23 new tests** in three
  files. Tests cover worked arithmetic, college-before-aggregation rounding,
  conservation across integer cases, zero support, direct-total bypass,
  multi-subject draw ordering/preflight, all aggregation scopes, special ballots,
  subthreshold contributions, preference ties/denominators, missing versus zero,
  exact arithmetic/overflow, owned outputs and downstream equivalence.
- `npm run build`: static build succeeded, 4.45 s reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  12.6 s, covering the existing politics/regional workers and result imports.
  Build and e2e ran sequentially. Existing plugin-timing and color-environment
  notices were non-blocking. An initial readonly assignment in a permutation
  test failed typechecking; replacing it with an owned object copy resolved it.
- `git diff --check` passed. No R code, persistent data, generated defaults,
  registry, workers or UI were changed. No reform application path, admission,
  special scrutiny, generator/calibration, full composition or performance
  acceptance is implemented by this step. Changes are left uncommitted.
