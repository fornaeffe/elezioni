## 2026-10-19: R26-17 initial exhaustion and replacements

Implemented N25/N26/N27 in `reform-2026/recovery.ts`, exported through the
legal-core index. Use the [recovery boundary](../../../web/src/lib/politics/reform-2026/README.md#initial-exhaustion-and-replacements-r26-17)
for its inputs, immutable remainder evidence, rounds and R26-18 handoff.
Rechecked Camera C84/C86/C83(1-quinquies) and Senate S17-bis/S19/S16-bis against
the scoped transcription, [official Camera amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1),
[Senate amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
and the [Camera consolidated comparison](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf).
The approved I07 regional-first protocol remains unchanged; final official-source
and seat-decree checks remain R26-21.

### Delivered behavior and rationale

- Camera initial exhaustion follows all permitted C84 levels. Initial releases
  use C86(2)'s limited reference and cannot reach C84(7). Senate ordinary
  recovery exhausts the original regional list and coalition before residual
  same-list cross-region recovery. It never imports ordinary C84(7).
- Bonus shortages use the separate express C83/S16-bis route: local winning
  list/coalition, member-list unused then used remainder, best college list
  percentage, candidate ranking; only then other territories ranked by the
  bonus-winning subject. Coalition entitlement is preserved without invented
  member-list bonus quotas. Territorial admissions are explicit inputs, so an
  excluded filed list cannot become a coalition replacement.
- Final allocation decimals and states remain immutable priority evidence,
  separate from allocation-transfer traces. Already-consumed correction
  decimals are already-used, never unused. Recovery fills further eligible
  candidate slots without recalculating quotas or imposing an invented quota
  on the number of replacement identities a destination may supply.
- Proposal rounds apply N24 repeatedly. Each successful round adds an unelected
  identity; undefined comparisons freeze their seats and unchanged states stop.
  A released recovered seat searches at its actual election location while its
  original entitlement, category and ID survive. Missing preferences do not
  become zero and do not conceal independently proven candidate exhaustion.
  Missing applicable ranking evidence remains unresolved; exhausted routes
  remain unfilled. Lower already-used missing decimals do not block a known
  unused tier.
- Ordinary decimal ties use statutory C84(8) draws. Bonus ties retain I03.
  Extended N24 only for explicitly marked ordinary-recovery proposals into
  multiple bonus seats: no final comparison is supplied, so warned I03 chooses
  between those seats. Filed multiple bonus candidatures remain invalid.
  The specification records this I03 boundary. Every proposed I07 cross-region
  recovery warns, including subsequent releases; trace dispositions and draw
  references preserve the complete history.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**; commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`: **46 files /
  748 tests passed**, **4.53 s**, including **49 new recovery tests**.
  Focused recovery and proclamation verification passed **74/74**.
- Worked fixtures cover I07-A through I07-F, every route, used/unused ordering,
  coalition and bonus selection, all-used variants, statutory versus I03 draws,
  repeated subentro, fully exhausted candidates, undefined comparisons, missing
  preferences/quotients, admitted lists, ownership and source-permutation replay.
  A cross-region recovered seat released by N24 is replaced at its actual
  destination while preserving its original regional entitlement.
- Existing validated joint fixtures now continue through recovery with
  **392/196** entitlements, **70/35** bonus holders, unique final identities and
  unchanged special winners. Their deliberately oversized colleges correctly
  retain unfilled residue rather than claiming full scrutiny acceptance.
- Initial focused checks exposed source-order-dependent eligibility trace
  ordering; canonicalized that evidence. An equal-percentage fixture required
  corrected supplied counts for its intended no-draw comparison, and test
  mutations needed explicit union narrowing. Final focused/full checks passed.
- `npm run build`: static build succeeded, **3.85 s** reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  **12.6 s**. Build and browser checks ran sequentially. Plugin-timing/color
  notices were non-blocking. `git diff --check` passed.
- R26-18 still owns complete stage composition, admission/remainder adapters
  and registry activation. Later scenario/generator, worker and UI stages remain
  inactive. Future vacancies and supplementary elections remain outside scope.
  No R code, generated snapshots/defaults or persistent data changed. Both plans
  now continue with R26-18; changes are left uncommitted.
