## 2026-10-19: R26-15 Senato list and college allocation

Implemented N21/N22 in `reform-2026/senate-territorial.ts`, exported through the
core index. Use the [stage boundary](../../../web/src/lib/politics/reform-2026/README.md#senato-list-and-college-allocation-r26-15)
for inputs, arithmetic, traces, failures and remaining scope. Rechecked S17
against the scoped transcription and [official article 2(10)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1).
No source version or interpretation policy changed; acceptance-time source and
decree checks remain R26-21.

### Delivered behavior and rationale

- Regional coalition-list allocation uses corrected N19/N20 subject targets,
  only admitted 2-bis/2-ter members, integer quotas, exact remainders, regional
  list votes and statutory draws. Single-college regions take those targets
  directly. Ordinary list allocation never divides the coalition's 35 bonus
  seats among member lists.
- Multi-college ordinary allocation uses integer quotas, reached-target
  exclusion, independently ranked surplus/deficit movements and college votes
  for initial remainder ties. The corrected-cap path first allocates majority
  and minority groups with both indices truncated to six decimals; group ties
  also use college votes. Group corrections stay within each college, then
  S17(4) allocates the lists within each group. Region and regional list totals
  are conserved; independent list corrections may change college totals.
- Extracted the existing Camera matrix arithmetic to `territorial-matrix.ts`,
  with explicit chamber and caller-selected criteria. Camera criteria, context
  IDs and algorithms retain their behavior. Equivalent college correction and
  index formulas are shared; Senate regional and college tie figures remain
  distinct. Matrix traces now identify the chamber explicitly.
- Stage communications check source/run identity where available, final subject
  and admitted-list coverage, regional capacities and college/regional figure
  reconciliation. N06/N08 still lack run IDs, so their pairing is the caller's
  responsibility. Zero-seat stages/groups skip undefined divisions. Invalid
  communications throw; non-computable prerequisites, I04 failures and missing
  lawful correction progress retain traces without final targets. Statutory
  draws and warned I03 correction draws remain distinct, and upstream events
  are not emitted again by downstream stages.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**; commands from `web/`.
  Final `npm run check`: **zero errors/warnings**. `npm run test`:
  **43 files / 649 tests passed**, **6.15 s**, including **38 new tests**.
  Existing Camera matrix/territorial and old-law regressions passed.
- Worked fixtures cover integer versus exact coalition quotas, admitted-member
  denominators, single/multiple colleges, independent regions and shared-region
  territories, regional list targets, surplus/deficit corrections, reached-target
  exclusion, zero-capacity colleges/groups, six-decimal changed-seat examples,
  college-versus-regional tie figures, statutory/I03 draws, no-progress failures,
  source/run/figure checks, ownership, JSON and seeded permutation replay.
  Validated single/joint envelopes now continue through N22: 189/154 ordinary
  seats respectively, with the joint majority retaining 71 ordinary seats plus
  7 special and 35 bonus seats from N20.
- Initial fixture runs exposed missing contesting slates, shared source-object
  aliases, fixture admission below threshold, readonly fixture typing and an
  expectation omitting the separate I03 recipient draw. Corrected the fixtures;
  final checks above passed. The validated multi-college envelopes legitimately
  require seeded correction draws rather than the previous no-draw test context.
- `npm run build`: static build succeeded, **5.46 s** reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  **15.5 s**. Build and browser checks ran sequentially. Plugin-timing/color
  notices were non-blocking.
- No R code, generated snapshots/defaults, registry, worker or UI changed.
  Candidate proclamations, multiple-election precedence, recovery and application
  integration retain their later steps. Both plans now continue with R26-16;
  changes are left uncommitted.
