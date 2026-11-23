## 2026-10-20 — N18 ordinary corrections with unequal local totals

Luca approved applying C83-bis(1)(b)(3) as independent subtraction of excess
list seats and assignment of missing list seats in the ordinary Camera branch
(no bonus or bonus without cap). This supersedes the blanket N18 local-capacity
blocker documented under R26-13. The controlling amendment, law 180/2026,
article 1(23), was rechecked against the
[promulgated text](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1).
The separate cap-group branch retains `constituency-capacity-target-mismatch`:
C83-bis(1-bis)(c)'s same-college transfers cannot reconcile unequal totals.

- Added an explicit `independent-colleges` matrix correction mode, restricted
  to Camera N18 list quotas. Initial integer quotas still use the supplied
  decree endowments. Remove excess seats by ascending used donor decimals,
  then assign missing seats by descending unused recipient decimals; preserve
  single-use remainder states and I03 draws for unspecified equal ranks.
  Other matrix modes retain their capacity-equality checks and paired behavior.
- Ordinary N18 now accepts different initial and communicated local totals,
  while checking preservation of national list entitlements. Corrections record
  genuine one-sided operations: removals have a null recipient and additions
  have a null donor, including in composed `SeatTransferTrace` results. Recovery
  continues to receive the exact final remainder values and states.
- Retained arithmetic, impossible-residual and no-eligible-donor/recipient
  blockers without publishing final college targets. The earlier `(2,2,3)`
  cross-constituency fixture still blocks in X with
  `no-eligible-donor-remainder`: its excess consists of integer seats.
  No integer-seat fallback, table rescaling or reroll was introduced.
- Added an independently worked N15–N18 regression with endowments `(4,2,4)`
  and corrected constituency totals `(3,3,4)`. Independent college removal and
  addition complete the allocation, preserving national list targets `(3,4,3)`.
  Separate tests cover the two-seat `60/40` example reaching targets `(2,1)`,
  removal without a recipient, both ordinary national paths, cap blocking,
  exhausted remainder pools, trace export, recovery evidence and seeded replay.
- Updated the operational plan, normative specification, legal-core README and
  generation limits to distinguish ordinary correction failures from the cap
  gap. An initial impossible-residual test fixture actually allowed residual
  allocation and reached a correction tie; replaced it with the independently
  checked exhausted-pool case. No production source or generated data changed.

Verification on Windows x64, Node **24.20.0**, npm **11.19.0**, from `web/`:

- Focused territorial suite: **55 tests passed** in two files.
- `npm run check` and `npm run check:data`: **zero diagnostics**.
- `npm run test`: **828 tests passed in 52 files**, no skips, **12.21 s**.
- `npm run build`: static build passed, **15.02 s** reported for the full Vite
  build; plugin-timing notices were informational. Build and e2e ran sequentially.
- `npm run test:e2e -- --reporter=line`: **10 Chromium tests passed, 19.9 s**,
  including reform generation and full scrutiny replay in browser workers.
- `git diff --check`: passed. No commit or publication performed.
