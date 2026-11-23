## 2026-10-21: N18/N21/N22 recipient-exhaustion investigation

Replayed the six remaining recorded failures, Camera **42, 80, 598, 792** and
Senate **49, 242**, with the current implementation and the saved acceptance
scenario. Every run reproduces `no-eligible-recipient-remainder` in the
correction phase. Both Senate roots are internal minority-list allocation on
the active N22 cap path; N21 also reports the aggregate failure. This was a
six-ID diagnostic replay, not a new 1,000-run frequency or browser benchmark.

In every case the deficient list has used every college decimal and still
needs one seat. Its upstream target exceeds the sum of its integer seats plus
one decimal award per college by exactly one: Camera 42/792 PD **2 > 0+1**;
80 PD **4 > 1+2**; 598 PD **5 > 2+2**; Senate 49 PD **4 > 0+3**; 242 M5S
**4 > 1+2**. This impossibility under the single-use recipient reading exists
before correction ordering is chosen. Initial colleges are full in every
failing matrix. I13 can supply donors in 42/792/242, but cannot provide an
unused recipient; I14's initial pass and I15's mismatched cap-group exception
do not cover this condition.

Rechecked promulgated article 1(23), C83-bis(1)(b)(3)/(1-bis)(d), and article
2(10), S17(4)(c)/(5)(d), against the Gazzetta Ufficiale. Both list-correction
rules require an unused recipient decimal without prescribing an exhaustion
fallback. Added **LG04**, with worked Camera 42 and Senate 242 chains and the
boundary of this strict-reading classification. In 242, N19's coalition/M5S/PD
allocation 10/1/0 becomes 6/4/1 through four N20 cap movements; the subsequent
college split cannot give M5S its fourth seat using each decimal once.
No recipient policy or scrutiny implementation changed. The plan retains both
items as open, linking the completed investigation to the pending decision.

Evidence: `data/derived/r26-recipient-review-20261021/inspect.mjs`,
`overview.json`, `summary-{id}.json` and full `simulation-{id}.json` inputs/results.
Compressed and uncompressed SHA-256 checks pass for snapshot
`r26-generation-v1-c5c4be9fafb5`, model `r26-models-v1-f71a72020aaa`, seed
`r26-default-v1`; the saved acceptance report hash remains
`bf12f43b8019326203cfb46d6366aa35145d8e0fb37c781cad5f2d0ff4b19373`.
Independent BigInt checks verify each failing matrix's vote-based quota,
quotient integer/decimal parts and full initial college capacities. Assertions
replay every correction transfer and remainder-state transition, verify the
empty recipient pools and the single-use upper bounds, and confirm identical
full results on a second scrutiny invocation. All checks passed for all six IDs.
No historical benchmark was overwritten; no commit or publication was made.
