## 2026-10-18: R26-10 special territories, proportional seats and cap ledger

R26-09 was already implemented at the start of this task; retained its admission
and preliminary-winner behavior and completed R26-10 only. The
[module boundary](../../../web/src/lib/politics/reform-2026/README.md#special-territories-r26-10)
documents stage order, inputs, outputs and remaining dependencies. Checked the
[promulgated Camera amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticoloDefault/originario?atto.codiceRedazionale=26G00201&atto.dataPubblicazioneGazzetta=2026-10-09&atto.tipoProvvedimento=LEGGE)
and [Senate amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
against the scoped C92-C93-octies and S20-S21 transcription and N09-N11/I01-I04
specification. No normative source version changed; the future annotated
republication remains an acceptance check.

### Delivered boundary and rationale

- `attributeSpecialVotes()` translates N10 separately from preliminary winner
  calculation. Candidate-only ballots are attributed at each uninominal college,
  using exact quota/remainders for each candidate's linked lists, before any
  territorial sum. Single-list and zero-only cases avoid division; positive
  multi-list only-votes without direct support return a named non-computable
  result. The entire college is preflighted before draws; candidate/list order is
  canonical. Equal residuals use warned I03 draws without an invented secondary
  vote criterion. Senate uses the approved I02 cross-reference reading.
- Attributed special inputs bypass redistribution. Candidate totals remain
  unchanged in both representations; their ballots enter national admission and
  bonus figures once, including VdA-only lists, and never enter ordinary allocation
  figures. Traces retain additions, exact quotas/remainders and draw/warning events.
- `allocateCameraTaaSeats()` implements N11 with exact subject and internal-list
  quotas, then local electoral figures and express statutory draws on equal
  remainders/figures. The independent `(10,3,1)`, six-seat fixture yields
  `(4,1,1)`, rather than the ordinary integer-quota `(5,1,0)`. Its capacity is the
  proportional college's own invariant endowment, with no second subtraction of
  uninominal seats. I01 local standalone exceptions are unioned/deduplicated with
  the national communication without changing nationwide admission.
- Clarified the N11 specification: C93-septies(2)(b) allocates inside an admitted
  coalition among its locally filed coalition lists. It lacks C83(1)(g)'s express
  limitation to national (2-bis)/(2-ter) members. A dedicated fixture keeps a list
  nationally excluded while awarding it local proportional seats. This is the
  special provision's distinct allocation route, not a change to N07 decisions.
- TAA provisional candidates follow capolista then preference rank, with filed
  order on equal counts and all-valid territorial percentage denominators.
  Missing preferences permit the head but leave later identities unresolved;
  explicit zero counts remain computable. Unavailable/exhausted candidates retain
  entitlements with explicit R26-16/17 recovery requirements. Whole-chamber
  multiple-election precedence and refilling are still deferred to those steps.
- Corrected TAA preference-budget validation to use the territorial special-list
  ballots, excluding raw candidate-only components. Previously the compatibility
  `ordinaryVotes` row could incorrectly reject preferences supported by actual
  territorial ballots. Schema version 1 still requires that row; it never supplies
  extra ballots or a separate preference budget. Tests cover both a valid count
  exceeding its duplicate view and rejection above the raw direct-list budget.
- `proclaimSpecialUninominalWinners()` consumes the same matching N09 winner
  communication and accepts no draw dependency. Excluded linked lists cannot
  remove a winner. Availability may block the identity but not the entitlement.
  Links and personal affiliation remain separate; coalition seats have no
  invented individual-list attribution. Nested N09 traces are retained for
  evidence, not emitted as another set of top-level events.
- `summarizeSpecialSeats()` counts known uninominal and TAA entitlements once per
  subject for later caps. Validated Camera/Senate fixtures produce 8/7 special
  seats. Missing preferences do not change known counts; missing/non-computable
  Camera proportional entitlement remains null. Ordinary/foreign seats and cap
  decisions are outside this helper. Public exports and reading maps are updated.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`:
  **37 files / 438 tests passed**, 4.54 s, including **47 new tests** in three
  files. The focused reform suite passed **15 files / 242 tests** (469 ms).
  Coverage includes college-before-aggregation rounding, integer conservation,
  multi-candidate preflight/draw ordering, raw/attributed downstream equivalence
  with isolated draw streams, local/national access separation, both exact quota
  stages, local tie criteria/statutory draws, provisional candidates, special
  restrictions, winner reuse, owned JSON-safe results and validated-envelope
  integration. Existing N07/N08 and younger-Camera/older-Senate tests still pass.
- An initial discriminated-union narrowing error was corrected before the final
  typecheck. One fixture intended to exclude a coalition list initially gave it
  the best national subthreshold figure; increasing the other subthreshold
  member's national figure fixed the independent admission expectation.
- `npm run build`: static build succeeded, 12.50 s reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  13.2 s, including existing politics/regional worker and result-import paths.
  Build and e2e ran sequentially. Existing plugin-timing/color notices were
  non-blocking. `git diff --check` and whitespace scans of tracked/untracked
  additions passed.
- No R code, persistent data, generated defaults, algorithm registry, worker or
  UI changed. Joint bonus, ordinary allocations/caps, final multiple-election
  precedence/recovery, generators/calibration and application integration remain
  R26-11 onward. Later supplementary elections remain outside initial scrutiny.
  The plan now continues with R26-11. Changes are left uncommitted.
