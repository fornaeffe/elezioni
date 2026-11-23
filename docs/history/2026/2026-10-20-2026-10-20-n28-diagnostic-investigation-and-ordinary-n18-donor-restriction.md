## 2026-10-20: N28 diagnostic investigation and ordinary N18 donor restriction

`R26_N28_NON_COMPUTABLE` is the presentation of the composed N28 stage, not
an independent statutory operation. `scrutiny.ts` marks the result non-computable
when any renewed chamber has no recovery result or a non-computable recovery.
Its `stage-prerequisite-non-computable` reason propagates earlier blockers.
Unfilled/unresolved seats with computable recovery instead produce `partial`.
The historical browser report has 216 N28 occurrences; the previously recorded
post-I12 direct replay has 177. Neither count establishes the legal cause.

Replayed the recorded scenario and seed `r26-default-v1`, digest-verifying the
production snapshot. Current simulation 4 reproduces N18 Camera
`no-eligible-donor-remainder`, followed by N23/N24 ordinary-allocation blockers,
N25/N26 prerequisite blockers and joint N28. Senate recovery completes 196
seats. No bonus is awarded. In Basilicata (`camera:territory:22`), the single
college `camera:P:170001` has four seats and communicated list targets of one
each for M5S, Azione–Italia Viva, AVS and Forza Italia. Their included votes are
88,446 / 45,804 / 28,436 / 7,630: quota 170,316 / 4 = 42,579. Integer seats
are 2/1/0/0; the remainder goes to AVS, producing 2/1/1/0. M5S must lose one
seat and Forza Italia gain one. The code rejects M5S because neither seat was
awarded through a remainder.

Reopened the [controlling article 1(23)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1).
C83-bis(1)(b)(3) ranks donor colleges by the smallest decimal part without
requiring a remainder-awarded donor seat. C83-bis(1-bis)(c), the separate cap
group rule, expressly requires seats obtained through decimal parts. The
ordinary `independent-colleges` predicate imports this additional restriction.
This revises the earlier assumption that ordinary integer-seat donors must
block: this simulation demonstrates an implementation/interpretation restriction,
not an unavoidable failure of literal ordinary b(3). It does not classify every
N18/N21 blocker or settle repeated-removal semantics.

Verification was investigative, with no scrutiny-source change. The runner
under `data/derived/r26-n28-review-20261020/inspect.mjs` saves the input/result
and `observed-summary.json`. Its `--counterfactual` mode bundles an in-memory
change to only the ordinary donor predicate, permitting positive-seat donors
with defined decimals. Simulation 4 then completes all 588 domestic seats,
including Basilicata targets 1/1/1/1; `counterfactual-summary.json` records it.
This is a narrow causal experiment, not an implemented or generally validated
correction, nor a new benchmark. No migration checklist item was completed.
