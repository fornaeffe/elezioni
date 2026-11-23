## 2026-10-21: I14 repeated initial Camera list remainder rankings

Luca authorized interpreting C83-bis(1)(b)(2)'s descending decimal ranking as
repeatable when a first pass cannot fill the college. Rechecked the
[promulgated Camera amendment, article 1(23)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1):
b(2) does not expressly prescribe a single traversal; b(3) expressly refers to
unused recipient decimals, and (1-bis)(d) imports the list rule within cap groups.
The earlier diagnosis of benchmark 165 as intrinsically defective depended on
the single-pass reading. I14 is a provisional interpretation, not a new legal
gap; the legal-gap register is unchanged. See the
[operative contract](../../politics-reform-2026-specification.md#initial-camera-list-remainder-passes-i14).

The compatibility review found no need to redesign inputs, result schemas,
workers, storage, exports or recovery. Existing `provisionalSeats - integerSeats`
counts initial remainder awards; `remainderSeat` records whether any were given.
One local correction change was necessary: a donor decimal that supplied several
initial seats remains `used` after partial removal and becomes `consumed` only
after the last such seat is removed. Transfer traces mark these removals with
I14; I13 integer fallback still starts only after remainder-awarded donors are
exhausted. Recovery already distinguishes unused from already-used evidence and
needs no multiplicity field or recomputed quotient.

Implementation preserves the simultaneous first pass. Additional whole-matrix
passes reuse established rankings and tie outcomes, excluding lists that reached
their constituency target at the pass boundary. Only N18 list matrices may
repeat, including internal cap lists; cap-group indices, Senate stages and
correction-recipient eligibility keep their previous contracts. Empty eligible
pools still block. Each additional college pass emits warning
`I14_REPEATED_REMAINDER_PASS`; composed metadata records `I14-v1`.

Verification on Windows x64, Node **24.20.0**, from `web/`:

- `npm run check` and `npm run check:data`: zero diagnostics.
- `npm run test`: **61 files / 881 tests passed**, **13.51 s**. Coverage includes
  two and more passes, newly reached targets, stable ties/permutations, empty
  pools, multiple donor removals before I13, internal cap-list allocation,
  recovery communications, metadata and JSON serialization.
- `npm run build`: static build passed, **11.68 s** reported; then
  `ACCEPTANCE_PREBUILT=1 npm run test:e2e -- --reporter=line`: **21 Chromium tests
  passed**, **30.3 s**. Build and e2e ran sequentially; plugin-timing/color notices
  were informational.
- Compared all **1,000 recorded benchmark IDs** against an isolated pre-I14
  matrix graph from commit `5925ef42e58e05c0183c17caf0f6e6009869aedf`, using the
  same generated inputs, scenario/seed, digest-verified production snapshot
  `r26-generation-v1-c5c4be9fafb5` and model `r26-models-v1-f71a72020aaa`.
  Results changed from **977 complete / 23 non-computable** to **985 complete /
  15 non-computable**, with **zero partials or exceptions**. Full serialized
  result objects for the other **992 IDs are identical**, including warnings,
  transfers, draws and recovery evidence. This is a direct Node comparison,
  not a browser latency benchmark.
- All eight previous N18 `impossible-residual` IDs (**165, 349, 435, 437, 488,
  511, 567, 673**) complete with **588 domestic seats** and one I14 warning each.
  In 165, Veneto 1's +Europa receives its third seat in the second pass;
  its original decimal reaches recovery marked `used`. The existing composed
  ledger checks preserve seat and elected-candidate uniqueness. Remaining root
  blockers are four Camera recipient-exhaustion and eleven Senate failures.
- `REFORM_BENCHMARK_COUNTS=200 REFORM_BENCHMARK_WORKERS=1,4
  REFORM_BENCHMARK_REPORT=../data/derived/r26-i14-review-20261021/browser-200.json
  npm run benchmark:reform`: **passed**, **181.46 s** with one compute worker
  and **44.69 s** with four. Both runs store **195 complete / 5 non-computable**
  results, including 165's I14 warning, with identical result tables, row counts,
  warning counts and diagnostics. The preceding Node comparison had finished
  before these browser runs. These measurements do not replace the historical
  full acceptance sweep or establish a pre/post performance comparison.

The reproducible comparison and exact summary are in
`data/derived/r26-i14-review-20261021/` (`replay.mjs`, `replay-summary.json`).
Historical reports and the previous investigation artifacts remain unchanged.
No commit, deployment or publication was performed.
