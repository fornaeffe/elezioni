## 2026-10-21: LG03 disjoined Camera cap-group correction (I15)

Luca authorized a provisional solution for `constituency-capacity-target-mismatch`:
apply C83-bis(1-bis)(c) disjoined when the constituency's upstream entitlement
differs from its C3 college total. The controlling article 1(23) text was
rechecked against the Gazzetta Ufficiale on 21 October 2026. Its literal rule
pairs removal with reassignment in the same college; it does not expressly
authorize disjoining or additions from used decimals. I15-v1 therefore records
the approved simulator policy, without claiming established office practice.

Initial group indices/quotas retain C3 capacities. For mismatched Camera N18
cap-group matrices only, remove surplus group seats by ascending used decimal,
retaining I13 integer donor fallback after strict donors exhaust. Independently
fill deficits by descending unused decimal, then one pass over used decimals,
including those used by an earlier addition. Reuse each used decimal at most
once; recheck deficits after each seat. Exhausted pools and undefined arithmetic
remain non-computable. Ties retain seeded I03 resolution. Balanced Camera group
matrices, Senate rules and ordinary/internal list recipient rules are unchanged.

Final college totals may differ from C3, while every upstream group and
constituency/national list target stays binding. Detailed matrices retain
original capacity and final seats; corrected group counts become capacities for
the existing internal list split. Proclamation, multiple elections and recovery
need no algorithm change. Recovery still consumes final list remainders, never
group indices. No decree/endowment input is rewritten and no wire schema
version changes. `I15_DISJOINED_GROUP_CORRECTION` warns once per affected
constituency, survives later failure and supplies applied `I15-v1` metadata.
Disjoined group traces have one null endpoint and I15 metadata; I13 removals
retain I13 metadata. They remain group evidence rather than fictitious
candidate-seat transfers. The specification, legal-gap register, core README
and active plans now describe this boundary.

Verification:

- `npm run check:data` passed; `npm run check` found zero errors/warnings.
- `npm test`: **62 files / 894 passed, no skips**. New tests cover both mismatch
  directions, exact donor/recipient ordering, unused-before-used addition,
  bounded reuse/exhaustion, I13 interaction, deterministic I03 permutations,
  scope guards, JSON traces, internal list/recovery communications and national
  conservation. A tie fixture was corrected to guarantee an actual correction
  tie instead of depending on the initial statutory draw outcome.
- `npm run build` passed; plugin-timing notices were informational.
- `ACCEPTANCE_PREBUILT=1 npm run test:e2e`: **21 passed**.
- Replayed the existing full constructed N18 diagnostic input, not an
  unconditioned simulation: it previously failed LG03, now completes with
  **588 seats / 588 distinct elected candidates**, no non-computable stage and
  both chambers' recovery `complete / all-filled`. National allocation and
  the full upstream constituency-list communication remain byte-identical;
  all final college-list totals match those targets. I15 warns for Veneto 1
  (**11 -> 12**) and Puglia (**21 -> 20**). JSON roundtrip and deterministic
  replay match. Evidence: `data/derived/r26-i15-review-20261021/constructed-summary.json`.

After those tests, ran **1,000 paired simulations** directly in Node against
both an isolated pre-I15 module graph and the changed implementation, using the
saved acceptance scenario, seed `r26-default-v1`, snapshot
`r26-generation-v1-c5c4be9fafb5` and model `r26-models-v1-f71a72020aaa`.
Compressed/uncompressed production bundle SHA-256 checks passed. Saved acceptance
report SHA-256: `bf12f43b8019326203cfb46d6366aa35145d8e0fb37c781cad5f2d0ff4b19373`.
This is a correctness replay, not a new browser performance benchmark.

Both versions produce **994 complete / 0 partial / 6 non-computable**, with
**zero exceptions** and **all 1,000 full serialized results identical**.
Every complete result has 588 seats and 588 distinct elected candidates.
There are **zero LG03/I15 cases in this frequency sample**; the constructed
diagnostic above provides applicability evidence, not a measured failure rate.

Remaining root failures (first failing stage in each chamber):

| Chamber / stage | Reason | Simulations | Frequency / 1,000 | IDs |
| --- | --- | ---: | ---: | --- |
| Camera N18 | `no-eligible-recipient-remainder` | 4 | 0.4% | 42, 80, 598, 792 |
| Senate N21 aggregate / N22 cap path | `no-eligible-recipient-remainder` | 2 | 0.2% | 49, 242 |

Presentation error codes include propagation and overlap; they represent six
failed simulations, not the sum of the rows:

| Code | Occurrences | Affected simulations | Frequency / 1,000 |
| --- | ---: | ---: | ---: |
| `R26_N18_NON_COMPUTABLE` | 4 | 4 | 0.4% |
| `R26_N21_NON_COMPUTABLE` | 2 | 2 | 0.2% |
| `R26_N22_NON_COMPUTABLE` | 2 | 2 | 0.2% |
| `R26_N23_NON_COMPUTABLE` | 6 | 6 | 0.6% |
| `R26_N24_NON_COMPUTABLE` | 6 | 6 | 0.6% |
| `R26_N25_NON_COMPUTABLE` | 4 | 4 | 0.4% |
| `R26_N26_NON_COMPUTABLE` | 4 | 4 | 0.4% |
| `R26_N27_NON_COMPUTABLE` | 2 | 2 | 0.2% |
| `R26_N28_NON_COMPUTABLE` | 6 | 6 | 0.6% |

Simulation warnings, excluding informational notes and session context:

| Code | Occurrences | Affected simulations | Frequency / 1,000 |
| --- | ---: | ---: | ---: |
| `I03_DRAW` | 1 | 1 | 0.1% |
| `I07_CROSS_REGION_RECOVERY` | 99 | 58 | 5.8% |
| `I08_PROVISIONAL_SEATS` | 2,000 | 1,000 | 100% |
| `I12_TERRITORIAL_ELIGIBILITY` | 1,000 | 1,000 | 100% |
| `I13_INTEGER_SEAT_REMOVAL` | 189 | 164 | 16.4% |
| `I14_REPEATED_REMAINDER_PASS` | 8 | 8 | 0.8% |
| `I15_DISJOINED_GROUP_CORRECTION` | 0 | 0 | 0% |

All 1,000 simulations have warnings. Session-only warning context is
`R26_CONDITIONAL_CANDIDATES` and `R26_SEAT_TABLE_3`, one occurrence each and no
simulation frequency. Informational notes remain separate in the complete CSV.

Reproduction source, pre-I15 source copies, exact ID sets and reports are in
`data/derived/r26-i15-review-20261021/`: `replay.mjs`, `report.mjs`,
`replay-summary.json`, `diagnostics-by-code.csv`, `report.md` and the constructed
summary. Generated artifacts are intentionally ignored; no historical benchmark
was overwritten. No commit, deployment or publication was performed.
