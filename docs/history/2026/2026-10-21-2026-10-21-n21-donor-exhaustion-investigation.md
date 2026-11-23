## 2026-10-21: N21 donor-exhaustion investigation

Replayed recorded benchmark IDs from 1 until the first current Senate
`no-eligible-donor-remainder`: simulation **31**, seed `r26-default-v1`,
digest-verified snapshot `r26-generation-v1-c5c4be9fafb5`, model
`r26-models-v1-f71a72020aaa`. This is an investigative Node replay, not a new
browser benchmark or a remeasurement of the historical 59 occurrences. The
existing post-I14 1,000-ID comparison records ten current donor-exhaustion
failures; this investigation independently checks one of them.

No bonus is awarded. Campania has 18 ordinary seats. Regional subject allocation
uses integer quota 168,720; coalition splitting yields M5S/+Europa/Azione-IV/
FdI/FI targets **11/3/1/2/1**. PD, AVS and Lega receive zero seats, so S17(4)(a)
excludes their combined 157,332 votes from the college denominators. The two
colleges have capacities 10/8, included votes 1,672,542/1,207,087 and integer
quotas **167,254/150,885**. M5S receives **7+5 integer seats**, already exceeding
its regional target. Reached-target exclusion prevents further remainder seats
but cannot undo this integer surplus. Initial complete college allocations are
M5S/+Europa/Azione-IV/FdI/FI **7/1/0/1/1** and **5/1/0/1/1**.

The first correction removes FI's remainder seat in `senato:P:150002`
(decimal about 0.468734) and gives it to Azione-IV in `senato:P:150001`
(about 0.429138). M5S remains at 12 instead of 11; +Europa remains at two
instead of three. Neither M5S cell was awarded a remainder seat, so the donor
pool is empty. Both +Europa recipient decimals remain unused. No draw occurs.
N21 blocks, followed by Senate N23/N24/N27 and joint N28 prerequisite failures.

Reopened the [controlling Senate amendment, article 2(10)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1).
S17(4)(c) has the same relevant donor wording as C83-bis(1)(b)(3). Under the
strict remainder-awarded-seat reading adopted for Camera LG01, this is the same
kind of legal gap, not an arithmetic defect in this replay. The ordinary list
wording remains interpretive: reading it as ranking donor colleges regardless
of seat origin would instead make the predicate an implementation restriction.
Do not confuse that alternative with the more explicit S17(5)(c) cap-group rule.
I13 remains expressly scoped to Camera; no Senate workaround was applied and
no new gap-register entry was added during this analysis.

Evidence under `data/derived/r26-n21-review-20261021/`: reproducible `inspect.mjs`,
full `simulation-31.json`, scoped `summary.json`, and `verify.mjs` with independent
BigInt quota/floor/decimal assertions, initial capacity checks, exact transfer
identities and donor/recipient pool checks (`arithmetic-verification.json`).
All assertions passed. No scrutiny implementation changed.
