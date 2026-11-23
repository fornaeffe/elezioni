## 2026-10-18: R26-09 admission and preliminary special winners

Implemented only R26-09 under `web/src/lib/politics/reform-2026/`;
R26-10 special territories is next. The [module contract](../../../web/src/lib/politics/reform-2026/README.md#admission-and-preliminary-special-winners-r26-09)
documents inputs, outputs and the dependency order. Consulted the
[promulgated act and its Camera/Senato amendment articles](https://www.gazzettaufficiale.it/atto/vediMenuHTML?atto.codiceRedazionale=26G00201&atto.dataPubblicazioneGazzetta=2026-10-09&tipoSerie=serie_generale&tipoVigenza=originario)
and [official comparison](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf)
against C83(1)(e), S16-bis(1)(e), C93(2)-(3), C93-septies(1)(a)-(b),(2)
and S21(1)-(2) in the scoped transcription/specification. No normative source
version changed; this does not check the future annotated republication.

### Delivered boundary

- `determineCameraAdmission()` and `determineSenateAdmission()` implement N07/N08
  with distinct coalition, standalone-list and internal-member decisions, stable
  reason codes and legal references. Evidence retains exact national shares,
  qualifying regions, protected-minority shares and required/actual uninominal
  wins. Comparisons use bigint products of counts at inclusive 3%, 10% and 20%;
  display percentages do not decide access. Special ballots remain in national
  figures and excluded votes remain in every applicable denominator.
- A coalition needs both national 10% and a qualifying member. Qualifying
  members enter under (2-bis); (2-ter) adds exactly one highest-national-figure
  otherwise excluded list, including a list below 1% or with zero votes. It
  adds none when every member already qualifies. Equal figures use canonical,
  injected I03 draws with simulation/chamber-specific warnings. Coalition order
  and tied alternatives cannot change the seeded draw sequence.
- Failed-coalition lists are evaluated for independent access. Camera's 20%
  minority route requires protected recognition, autonomous-region status and
  exclusive presentation. The quarter-uninominal-wins alternative is reserved
  to unlinked minorities and cannot qualify a coalition or its (2-bis) members.
  The independent fixture with five uninominal colleges requires two wins, not
  one. A failed-coalition minority below national 3% remains excluded nationally;
  `taaLocalExceptions` records I01 local 20% access separately, preserving
  C93-septies(2)'s anomalous (2-bis) reference without extending nationwide access.
- Senate's regional 20% route applies to any list, ordinary or special region,
  independently or inside an admitted coalition, and is not restricted to a
  minority or to allocation in the qualifying region. Zero-valid-vote regions
  cannot supply a 20% pass. Zero national valid votes return a non-computable
  stage with no invented exclusions/admissions or admission draws.
- `determineSpecialWinners()` implements preliminary N09 before admission,
  independent of list access or special proportional allocation. It uses the
  adjudicated candidate totals already present in both vote representations:
  most votes, then younger Camera/older Senate candidate, then warned I03 on
  equal dates. Counts/references for all colleges are preflighted before any
  draws; college and alternative order are canonical. The ledger preserves
  candidate, college/territory/region, votes, political subject, all list links,
  separate affiliation and tie traces. Camera admission requires the same
  simulation's complete preliminary winners, never recomputes their outcome
  and does not emit their draws/warnings again. R26-10 must reuse this ledger.
- The functions require validated political/geographic/candidate references and
  N06 aggregates from the same simulation. Focused completeness guards do not
  replace envelope validation. Full validated Camera/Senato fixtures run through
  preliminary winners, attributed aggregation and admission without generators
  or TAA proportional allocation. Public exports and the module reading map are
  updated; the existing result envelope and algorithm registry are unchanged.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`:
  **34 files / 391 tests passed**, 4.49 s, including **40 new tests** in two
  files. Coverage includes below/at/above thresholds, coalition qualification,
  independent access, minority coverage, I01, rounded-up quarter wins, regional
  Senate access, sub-1%/zero-vote rescue, multiple canonical I03 draws,
  same-preliminary-winner reuse, candidate age precedence, raw/attributed winner
  equivalence, all-zero denominators, large exact counts, JSON-safe owned results,
  swapped chambers and missing/duplicate special contributions.
- An initial fixture-array inference error was fixed with explicit input-array
  types; subsequent typechecking passed. A final strengthening of the vote-over-age
  test passed all **10 special-winner tests** again (254 ms).
- `npm run build`: static build succeeded, 3.95 s reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  12.7 s, covering existing politics/regional workers and result imports.
  Build and e2e ran sequentially. Existing plugin-timing and color-environment
  notices were non-blocking.
- `git diff --check` and a whitespace scan including untracked additions passed.
  No R code, persistent data, generated defaults, registry, workers or UI changed.
  Special list-vote attribution and TAA proportional seats remain R26-10;
  joint bonus, later allocations/proclamations/recovery, composition, generation,
  integration and reform application/performance acceptance remain pending.
  Changes are left uncommitted.
