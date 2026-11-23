## 2026-10-18: R26-12 Camera national allocation and cap

Implemented N13/N14 in `reform-2026/camera-national.ts`, exported through the
core index. The [stage boundary](../../../web/src/lib/politics/reform-2026/README.md#camera-national-allocation-r26-12)
documents inputs, traces and subsequent dependencies. Rechecked the applicable
[Camera amendments, article 1(22)(g)-(n)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
against the scoped C83 transcription and N13/N14/I04/I08 specification. No
normative source version changed. Operative excerpts and article references
remain beside the translated logic; later corrections and annotated
republication remain acceptance checks.

### Delivered boundary and rationale

- `allocateCameraNationalSeats()` consumes the validated Camera input and the
  same run's N06/N07/N10/N12 communications. It isolates provisional subject
  allocation, the cap check, cap correction and internal coalition-list targets.
  No bonus allocates 384 ordinary seats; bonus paths first allocate 314 and
  retain 70 bonus seats under the winning subject's ownership.
- Ordinary list figures exclude VdA/TAA votes. At subject level, coalitions
  include every linked list's votes; internal allocation uses only admitted
  (2-bis)/(2-ter) members, including the single best excluded list. All quota
  allocations discard the quota fraction before assigning integer seats and
  residuals, using exact remainders, ordinary national figures and injected
  statutory draws. Zero-seat coalitions do not require division.
- The cap check includes linked special seat entitlements, even when candidate
  identities remain unresolved. Exactly 220 is unchanged; 221 triggers a fresh
  national allocation. For five special seats, majority ordinary entitlement is
  145, minority entitlement is 169 and the winner retains 220 domestic seats
  including the bonus. The majority quota is floored for R26-13 without
  reapportioning its directly fixed target. Estero has no allocation/cap input.
- Output preserves provisional and final targets separately, majority/minority
  quotas, coalition allocations, remainder consumption and seeded draw evidence.
  Bonus seats are never divided among member lists. Results are owned,
  JSON-safe data and preserve seat-table provenance. The R26-07 bundled table
  remains provisional census data; the 392 = 384 ordinary + 8 special ledger
  and 314 + 70 ordinary/bonus alternatives are checked before allocation.
  Incompatible endowments are rejected rather than rescaled.
- Incomplete admission/bonus or required special totals block before allocation
  draws; I04 arithmetic failures retain the failed stage and attempted traces
  without publishing final targets. Unknown bonus capacity remains null.
  Upstream draw/warning events are not re-emitted. The N12 simulation ID is
  checked; pairing N06/N07/N10 communications remains the caller's responsibility
  because those records lack independent run IDs.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`:
  **39 files / 506 tests passed**, 4.52 s, including **38 new national-allocation
  tests**. Coverage includes all three paths, 220/221, zero through eight linked
  special seats, 314-seat conservation, quota/remainder traces, coalition-member
  filtering and corrected minority coalitions, identity collisions, statutory
  draws/replay/permutation invariance, I04 failures, endowment checks, ownership
  and validated single/joint-envelope composition through preceding legal stages.
- Verification caught two fixture expectations that used an incorrect raw-quota
  fraction and a TypeScript mutable-fixture assertion mismatch. Corrected the
  expectations and supplied an explicitly typed input before cloning; the final
  typecheck and full suite passed.
- `npm run build`: static build succeeded, 3.95 s reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  12.1 s, including politics/regional worker and result-import regressions.
  Build and e2e ran sequentially; plugin-timing/color notices were non-blocking.
- No R code, generated data/defaults, registry, worker or UI changed. This step
  establishes national seat entitlements, not territorial allocation or candidate
  outcomes. R26-13 onward retain their implementation scope. Both plans now
  continue with R26-13; changes are left uncommitted.
