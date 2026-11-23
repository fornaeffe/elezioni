## 2026-10-18 R26-07 geography, census preparation and seat endowments

Implemented R26-07 and its [reproducible preparation workflow](../../../data/README.md).
R26-08 is next. Luca approved migrating shared acquisition/geography/census now,
using `data/sources` and `data/derived`, retaining `dati` as legacy, checking the
usability of the original spreadsheets, and permitting explicit population
estimates when finer data are unavailable. Historical vote/preference importers
and model calibration remain later consumers, including R26-19.

### Delivered boundary and source decisions

- Added reusable Node/TypeScript source acquisition, checksum verification,
  cached/manual import and immutable artifact writing. The tracked manifest pins
  **12 downloads / 145,471,137 bytes**; raw and intermediate files are ignored.
  Generation is offline, independent of R caches and scenario workbooks. Census,
  geography, actual used-source hashes, method, seed, population assumptions and
  reconstruction availability determine replayable versions.
- Investigated all four definitive 2020 XLSX tables. Explicit continuation/header
  handling extracts **8,073 A1 memberships / 147 colleges**, **146 A2 relations /
  49 colleges**, **8,073 B1 memberships / 74 colleges**, and **67 B2 relations /
  26 colleges**. A2/B2 relations agree with the machine-readable DBF. The primary
  catalog uses the government's **2022-updated `UT_Collegi2020.dbf`**, avoiding
  presentation-format dependence, obsolete proposal membership and manual Lazio
  patches. It includes Misiliscemi and the two municipalities moved to
  Emilia-Romagna. No polygon/GIS processing or second runtime is required.
- Applied the reform's Bolzano U04/U05/U06 overlay. Independent parsing of
  statutory annexes 4 and 5 yields the same **116 municipal memberships**;
  U04 contains Bolzano and Laives. A documented alias reconciles Montagna's
  post-census rename. Both autonomous provinces retain three Senate contests.
- Rechecked [Istat's electoral-population reference](https://www.istat.it/notizia/popolazione-censuaria/):
  the pinned 2021 legal census covers **7,904 municipalities / 59,030,133 people**.
  No applicable reform allocation decree was verified, so generated endowments
  are explicitly **provisional I08-v1**, not official decree allocations.
- Used Istat's 2021 population for Roma's **155 urban zones**, totaling exactly
  **2,749,031**. Their code crosswalk assumes continuity with electoral geography;
  geometric boundary identity has not been independently verified. An Italian
  warning is retained in the artifact and legal input's additive optional
  `SeatTableSource.populationNotes` field, without changing existing schema-1
  constructed inputs. `seatTableMessages()` separates this warning from ordinary
  provisional-table information; R26-20 must render the messages at integration.
- Missing fine-geography population uses available observations, historical
  weights and, only without informative weights, equal weights. Reconstruction
  conserves the legal municipal total and records method, inputs, estimates and
  seeded ties. Missing submunicipal files produce a different immutable version,
  rather than making the application depend on their availability. Corrupt
  downloads still fail integrity checks. Unused change-history/section-population
  files are cached for future importers, without claiming them as consumed inputs.
- Added pure catalog/table validation, exact I08 allocations, constrained Senate
  minima, alternative college capacities, a validated chamber adapter and a small
  tracked offline bundle. Camera has 54 active colleges and Senato 33. Endowments
  reconcile to Camera **8 special + 384 ordinary**, or **8 + 314 + 70 bonus**;
  Senato **7 special + 189 ordinary**, or **7 + 154 + 35 bonus**. Camera TAA has
  four uninominal seats plus three proportional seats. Official tables can later
  replace capacities with distinct decree provenance and versions, preserving
  earlier artifacts. Incompatible statutory totals/capacities remain errors.

### Verification, problems and limits

- On Windows, Node **v24.20.0**, npm **11.19.0**, from `web/`:
  `npm run check` reported **zero errors/warnings**; `npm run check:data` passed;
  `npm run test` passed **29 files / 328 tests** in 5.52 s. The **35 new tests**
  cover acquisition, source parsing, real municipal coverage/Bolzano, missing-data
  estimation, minima, accounting, immutable versions, official replacement,
  deterministic I03 draws and an independent Alabama-paradox example
  (`29 -> [14,1,14]`, `24 -> [11,2,11]`). After a validator guard refinement,
  preparation typechecking and the affected **28 tests** passed again. A final
  zero-information reconstruction refinement passed its **7 population tests**
  and preparation typechecking; all-zero inconsistent observations use available
  historical weights before falling back to equal weights.
- Real-source preparation tests explicitly skip when their ignored archives are
  absent. Tracked table/core fixtures and synthetic format/I/O fixtures require
  no network or large downloaded artifacts. The missing-submunicipal-file test
  generates and validates a usable complete seat bundle with estimation notices.
- `npm run data:download -- --sources ../data/derived/clean-clone-sources`
  downloaded/verified all originals into an initially empty cache. Generation
  into a separate empty output directory reproduced the published JSON **byte
  for byte**. Table version: `r26-seats-census-2021-i08-v1-9672c20529b2`;
  compact JSON-content SHA-256:
  `02bc849b58bbc1178c3216e898e7a09d07c46238a8e8d2d486ba30bf11ab97a3`.
- A separate Python `fractions.Fraction` audit over normalized populations,
  without importing TypeScript allocation helpers, independently reproduced
  **244 territorial/bonus/college-capacity comparisons**. No production
  remainder-cutoff ties occurred. Python was used only for this independent
  verification, not added to the preparation workflow.
- `npm run build` produced the static site (13.43 s; Vite plugin timing advisory).
  Subsequently `npm run test:e2e -- --reporter=line` passed **all 7 Chromium
  tests** in 13.3 s, including existing politics/regional workers and result
  import/restore. Build and e2e ran sequentially; browser output also included
  existing color-environment notices.
- Workarounds: the geographic archive is approximately 98 MB despite the source
  page's 23 MB label; its DBF attributes require Windows-1252 although the `.cpg`
  says UTF-8. Istat's SDMX endpoint returns HTTP 500 (`languageTag1`) for Node's
  default wildcard language header; the manifest's explicit `Accept-Language: it`
  resolves it and reproduces the pinned CSV bytes.
- The four added preparation dependencies are pinned dev dependencies. The npm
  audit reports nine findings in the existing SvelteKit/Vite/Vitest toolchain
  and transitive dependencies; none names the added preparation packages.
  Unrelated dependency upgrades were not folded into this step.
- All **122 local document links/anchors** resolved. `git diff --check` and a
  whitespace scan including untracked additions passed.
- Legacy R logic/data, snapshots/defaults, registry, workers and UI behavior
  remain unchanged. No reform algorithm is activated, no statistical model is
  fitted and no application/performance acceptance is claimed. No dependent
  legacy defaults require regeneration from this separate geography artifact.
