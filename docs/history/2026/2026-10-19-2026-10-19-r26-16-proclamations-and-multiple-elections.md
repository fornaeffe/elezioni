## 2026-10-19: R26-16 proclamations and multiple elections

Implemented N23 in `reform-2026/proclamations.ts` and N24 in
`reform-2026/multiple-elections.ts`, exported through the legal-core index.
Use the [stage boundary](../../../web/src/lib/politics/reform-2026/README.md#proclamations-and-multiple-elections-r26-16)
for inputs, provisional batches, precedence decisions and the R26-17 handoff.
Rechecked Camera C83-bis/C84/C85/C93-octies and Senate S17/S17-bis/S20-bis
against the scoped transcription, [official Camera amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1),
[Senate amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
and the [Camera consolidated comparison](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf)
for unchanged cross-references. No source version or interpretation policy
changed; acceptance-time source and decree checks remain R26-21.

### Delivered behavior and rationale

- N23 receives final ordinary list targets and the existing special/TAA and
  bonus communications. It elects the winning bonus slate in filed order,
  retaining personal affiliation separately from coalition entitlement, then
  ordinary/TAA capolista and preference-ranked candidates. Special winners are
  reused without new draws; TAA seat IDs are preserved without double counting.
- Missing preferences leave dependent identities unresolved while preserving
  known heads and all entitled seats. Explicit zeros use filed preference order.
  Unavailable candidates and exhaustion remain explicit recovery requirements;
  no replacement route is run implicitly. Stage/run/source checks and target,
  figure and seat-total checks reject mismatched communications. Filed
  special/bonus incompatibilities are rejected before proclaiming candidates.
- N24 applies special uninominal, bonus and capolista precedence in order.
  Repeated heads use the lowest exact list percentage; other repeated wins use
  the highest exact individual percentage. Only the surviving role's wins enter
  comparisons. Final equal percentages use canonical seeded I03 draws with
  warnings; undefined percentages leave competing seats unresolved without
  fabricating a zero or a winner.
- Each person retains at most one filled seat. Released seats retain their ID,
  category and original entitlement, with explicit links to the former candidate
  and retained seat. Decisions record eligible/considered seats, legal references
  and draw IDs. Outputs are owned, JSON-safe and replay under source permutations.
  A complete N23 batch is provisional; released/unresolved seats keep N24 partial.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**; commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`: **45 files /
  699 tests passed**, **4.64 s**, including **50 new tests**. Focused N23/N24
  verification also passed **50/50**, including the final fixture refinement.
- Worked fixtures cover both chambers, every precedence, mixed roles, list
  percentage versus individual percentage, exact I03 ties, undefined values,
  missing/zero preferences, unavailable candidates, exhaustion, preserved seat
  identities, incompatible input, communication checks, ownership and JSON replay.
  Validated joint envelopes continue through N23/N24 with **392/196** entitled
  seats and **70/35** retained bonus members. Mixed-affiliation coalition slates
  keep coalition `listId: null`; a real special/TAA double election retains the
  special seat. Deliberately oversized fixture colleges remain exhausted/partial,
  correctly preserving entitlements rather than claiming complete recovery.
- Initial verification caught fixture-only readonly typing and an incorrect
  Camera helper import name; these were corrected before the final checks.
- `npm run build`: static build succeeded, **5.12 s** reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  **12.6 s**. Build and browser checks ran sequentially. Plugin-timing/color
  notices were non-blocking; existing politics/regional workflows passed.
- R26-17 still owns ordered exhaustion/replacement routes and repeated
  proclamation/recovery with a progress condition. R26-18 composition/registry
  and later generators, workers and UI remain inactive. No R code or generated
  snapshots/defaults changed. Both plans now continue with R26-17; changes are
  left uncommitted.
