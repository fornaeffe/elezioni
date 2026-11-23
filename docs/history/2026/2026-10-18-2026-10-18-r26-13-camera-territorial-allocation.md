## 2026-10-18: R26-13 Camera territorial allocation

Implemented N15-N18 in `reform-2026/camera-territorial.ts` with the scoped matrix
engine in `camera-territorial-matrix.ts`, exported through the core index. Use
the [stage boundary](../../../web/src/lib/politics/reform-2026/README.md#camera-territorial-allocation-r26-13)
for API order, matrix contracts, correction rules and blockers. Rechecked the
applicable [Camera amendments, article 1(22)(l)-(n) and (23)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
and unchanged C83(h) wording in the
[official consolidated comparison](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf)
against the scoped transcription and N15-N18/I03/I04/I05 specification. No
normative source version changed. Applicable operative excerpts/references
remain beside the translated operations; the later republication/correction
and decree checks remain R26-21 acceptance work.

### Delivered boundary and rationale

- `allocateCameraConstituencies()` consumes final N13/N14 subject targets,
  ordinary constituency figures and the selected endowments. N15 uses integer
  local quotas, exact decimal parts, ordinary national figures and statutory
  draws. N17 instead uses the corrected majority/minority national divisors,
  truncates both sides before normalization, and records `I05-v1` explicitly.
  `allocateCameraCapConstituencies()` is also independently callable.
- `allocateCameraCoalitionListsInConstituencies()` uses only the national
  admitted internal-list targets. Initial N16 ties use constituency list votes;
  corrections import h's national criteria. Excluded linked votes still count
  in the coalition's subject figure, but not its internal denominator.
- `allocateCameraColleges()` uses only positive constituency list targets.
  The normal branch applies college integer quotas with constituency-vote
  residual ties. The cap branch first calculates majority/minority group
  quotients and both truncated college indices, reconciles groups within each
  college, then allocates each group's lists separately. Zero-target groups
  are inactive and do not evaluate an undefined quota/index.
- `allocateCameraTerritorialSeats()` composes these three communications,
  retaining each stage result. It does not proclaim candidates, distribute
  coalition bonus identities, execute recovery, or register a full algorithm.
  Missing preferences do not prevent computing ordinary seat entitlements.
- Matrices expose capacities, divisor/quotient arithmetic, raw/truncated indices,
  integer parts, decimal parts, reached-target exclusions, provisional/final
  seats, used/unused/consumed remainders and ordered movements. Integer parts
  are computed across the full matrix before reached-target residual exclusions,
  avoiding an arbitrary geographic ordering. Inputs are unmodified; results
  are separately owned and JSON-safe. Draws are canonical/injected; missing
  correction tie-breaks warn under I03, separately from statutory initial draws.
  Upstream events are not re-emitted by downstream stages.

### Correction accounting and explicit limits

- N15/N16/N17 process surplus subjects by excess seats, then ordinary national
  figure. They search all eligible same-constituency movements before falling
  back across constituencies. Donors use ascending used decimals; recipients
  use descending unused decimals. Same-constituency recipient ties use national
  figures; unspecified cross-constituency destination ties use I03. Movements
  consume donor remainders and use recipient remainders once.
- The independent worked fallback `(A,B,C)` fixture has capacities `(2,2,3)` and
  votes `(40,35,25)`, `(45,40,15)`, `(100,45,155)`. Initial allocations total
  `(3,2,2)` against targets `(2,3,2)`. Removing A's used `.8` in the first
  constituency and assigning B's unused `.45` in the third yields the required
  national targets and local totals `(1,2,4)`. Thus a statutory fallback need
  not preserve the original individual constituency capacities.
- If that corrected local communication cannot match its fixed college
  endowment, N18 returns `constituency-capacity-target-mismatch` and the affected
  territory, retaining complete N15/N16 results. No compensating seat movement
  or decree-table rescaling is invented. Any subsequent reconciliation requires
  reviewed legal support. The scoped coordinator must preserve the blocker;
  this step does not claim a composed full scrutiny for that exceptional case.
- N18 normal/list-within-group movements rank donor and recipient colleges
  independently and can change college totals while preserving constituency
  list entitlements and total seats. Its cap-group correction instead conserves
  each college capacity. These operations remain distinct. Undefined arithmetic,
  impossible residuals or no eligible correction progress return explicit
  non-computable results without final targets.

### Verification

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`:
  **41 files / 548 tests passed**. The **42 new tests** cover independently
  worked matrices, all three national paths, local versus national tie criteria,
  surplus ordering, same/cross-area movements, consumed remainders, no-progress
  failures, zero groups, I05 sixth/seventh-decimal boundaries and changed-seat
  cases, special exclusion, stage prerequisites, seeded replay/permutations,
  ownership and JSON round trips. Existing validated single/joint-envelope tests
  now continue through N18 with **384/314** ordinary entitlements respectively.
- Fixture development caught impossible expectations: four required recipient
  remainder seats cannot be supplied by three single-use remainder cells, and
  a cap-group donor without a remainder seat cannot cede an integer seat under
  the specified route. Corrected those hand-worked fixtures; separate tests
  retain the explicit no-progress behavior. Typechecking also caught an overly
  narrow mutable-fixture assertion; cloning an explicitly typed input fixed it.
- `npm run build`: static build succeeded, **3.95 s** reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  **12.6 s**, covering politics/regional workers and result import. Build and
  e2e ran sequentially. Plugin-timing/color notices were non-blocking.
- No R code, generated snapshots/defaults, registry, worker or UI changed.
  Senate allocation, proclamations, multiple-election precedence, recovery and
  application integration retain their subsequent steps. Both plans now continue
  with R26-14; changes are left uncommitted.
