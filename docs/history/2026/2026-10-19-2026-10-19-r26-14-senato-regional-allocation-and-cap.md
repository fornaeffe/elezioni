## 2026-10-19: R26-14 Senato regional allocation and cap

Implemented N19/N20 in `reform-2026/senate-regional.ts`, exported through the core
index. Use the [regional stage boundary](../../../web/src/lib/politics/reform-2026/README.md#senato-regional-allocation-r26-14)
for inputs, arithmetic, ordered cap movements, failure states and later-stage
limits. Rechecked S16-bis against the scoped transcription and the
[official Senate amendments, article 2(9)(f)-(h)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1).
No source version changed. Operative excerpts and references remain beside the
translated operations. Acceptance-time correction/decree checks remain R26-21.

### Delivered behavior and accounting

- `allocateSenateRegions()` consumes same-run N06/N08/N10/N12 communications.
  N19 allocates subjects independently in each ordinary region, using integer
  quotas, exact remainders, regional electoral figures and statutory draws.
  Every linked list contributes to its coalition's figure; coalition-to-list
  allocation remains N21. National totals are sums of regional allocations,
  never a national proportional allocation redistributed to regions.
- Endowments conserve 196 domestic seats: 189 ordinary plus 7 special, or
  154 ordinary plus 35 bonus plus 7 special. Regional alternative-capacity
  differences must match regional bonus seats. Incompatible tables are rejected
  without rescaling. Bundled census-table provenance remains provisional I08.
- `applySenateCap()` exposes the independently callable N20 boundary. Exactly
  113 is unchanged; 114 triggers correction. The majority ordinary target is
  `78 - linkedSpecialSeats`, retaining the 35 bonus seats. Special entitlements
  count once regardless of unresolved candidate identities; Estero is absent.
  No bonus means no cap check or correction.
- The ascending national removal ranking uses raw vote remainders that supplied
  majority seats. Regional quotient decimal parts would give a different order
  when regional quotas differ. Equal values share a rank until a required
  removal invokes I03, avoiding draws for irrelevant ordering below the cap.
- After the single-use remainder removals, percentage passes rank exact
  majority shares against all valid regional votes, including excluded lists.
  The first pass excludes remainder-phase regions and protects a sole seat.
  Later passes readmit remainder-phase regions and protect the current final
  seat. The six-sentence structure of S16-bis(1-ter)(a) matters: sentence six
  repeats only sentences three/four (percentage ranking/one per region), not
  sentence five's initial exclusions, and specifies current-last-seat protection.
  Final review temporarily carried the initial exclusions into every pass after
  miscounting those sentences; checking the numbered wording corrected that
  interpretation before final verification. Tests cover re-entry, an empty
  first pass, and sole used-remainder seats which can become zero before the
  percentage protection applies.
- Every removal has a corresponding assignment in the same region. Recipients
  use their largest unused remainder once, then regional votes after all such
  remainders are exhausted. Equal removal remainders, percentages, recipient
  remainders and fallback votes use warned seeded I03 draws. No unexpressed
  regional-vote secondary criterion is imported into remainder-correction ties.
- Results retain provisional regions and national totals separately from final
  targets, exact quotas/remainders, `I06-v1`, ordered movements, pass numbers,
  donor/recipient seat counts and remainder states before/after, tie resolutions,
  draws and warnings. Donated used remainders become consumed; recipient unused
  remainders become used. Each paired movement preserves its region's capacity.
- Incomplete prerequisites block before allocation draws. I04 arithmetic failures
  retain the affected region and attempted quota trace. No eligible recipient
  or no lawful removal progress retains provisional data and attempted movements
  without publishing final targets. Outputs are owned and JSON-safe; input
  permutations preserve replay. Upstream warnings/draws are not re-emitted.
  N12's simulation ID is checked; N06/N08/N10 pairing remains the caller's
  responsibility because those communications lack independent run IDs.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. Final `npm run test`:
  **42 files / 611 tests passed**, 6.05 s, including **63 new tests**.
  Coverage includes all three paths, every zero-to-seven linked special-seat
  count at 113/114, regional rather than national allocation/ties, raw-remainder
  ranking, repeated passes and exclusions, last-seat protection, recipient
  exhaustion/fallback, I03 warnings, explicit no-progress and arithmetic failures,
  zero-seat regions, endowment/prerequisite checks, coalition/list identity
  collisions, ownership, JSON and seeded permutation replay. Validated single
  and joint envelopes continue through N19/N20 with 189/154 ordinary seats;
  the joint majority has 71 ordinary + 7 special + 35 bonus = 113.
- Fixture verification caught an inconsistent national capacity, an incorrect
  expectation for a floored quota and mutable/union fixture typing errors.
  Corrected the fixtures; the final checks above passed.
- `npm run build`: static build succeeded, **5.33 s** reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  **15.7 s**, covering politics/regional workers and result import. Build and
  browser checks ran sequentially; plugin-timing/color notices were non-blocking.
- No R code, generated snapshots/defaults, registry, worker or UI changed.
  Internal Senate lists/colleges, proclamations, recovery and full application
  integration retain their later steps. Both plans now continue with R26-15;
  changes are left uncommitted.
