## 2026-10-26 — Manual archive: 19 non-computable runs traced to LG05/LG06

Inspected `data/derived/r26-manual-tests/r26-risultati (2).json.gz`
(135,131,224 bytes; SHA-256
`681673ac7027d62618f86625eecf8f56be810ad89fa7bb7ebfe086a5a8700045`).
The archive reports 1,000 requested, 981 complete, zero partial and 19
non-computable runs. All root blockers are in Camera; Senate has no
non-computable stage in these runs. Later N18/proclamation/recovery/N28 failures
propagate the first blocker and are not additional independent defects.

| Root stage/reason | Simulation IDs |
| --- | --- |
| N16 `no-eligible-donor-remainder` | 163, 251, 299, 331, 375, 381, 416, 767, 910, 932, 989 |
| N18 `zero-index-quota`, Molise | 311, 323, 571, 660, 819 |
| N18 `zero-index-quota`, Basilicata | 421, 449, 974 |

All eleven N16 failures have Sud chiama Nord exceeding its national entitlement
by one seat, with all its current seats obtained through integer quotas in
Sicilia 1/2. There is no remainder-awarded donor. Example 163 has target four,
initial/current five (two plus three); Lega is one seat short. Existing I13
is expressly limited to college corrections and does not cover N16. Registered
this constituency-level analogue of LG01 as LG05, with an unapproved proposal
to extend the donor policy narrowly while preserving N16 correction criteria.

All eight N18 first failures follow an N16 correction awarding Sud chiama Nord
a seat in a constituency where it is filed but has zero votes. N16's same-area
route can select its defined zero decimal before considering positive decimals
elsewhere. On the cap path, only positive-entitlement local lists enter the
group denominator. Sud chiama Nord is then alone in one group with one seat
and zero votes. Its quota is zero; the required index would divide zero by zero.
Example 311 has Molise targets Sud chiama Nord 1 / PD 1, and group vote totals
0 / 18,514. Registered LG06 after reviewing C83-bis(1-bis)(a)-(b), with an
unapproved single-college direct-communication proposal; all eight first-failure
constituencies contain one college. This is distinct from an inactive group
with zero seats. Sparse historical support and nationwide filed coverage are
both present in the supplied scenario; zero votes do not mean missing filings.

Verified C83(1)(h)/(i) and C83-bis against the official annotated republication
of 23 October 2026, linked in the gap register. No statutory fallback covering
these cases was identified; current explicit blockers agree with the adopted
contracts. No scrutiny implementation correction was applied.

Verification: streamed the gzip archive, then regenerated all 19 affected IDs
using its exact scenario and installed production snapshot. All 1,254 archived
national list vote counts match, as do every first-failure stage/reason and
394 Camera allocation transfers' remainder evidence/draw contexts. Independent
Python integer arithmetic confirms each N16 surplus is one integer-only seat;
all eight failed N18 groups have zero votes and one seat. Local evidence and
replay/verification scripts are under `data/derived/r26-manual-tests/`, including
`r26-risultati-2-verification.json`. The one-off streaming extractor used Python
`ijson` installed under the OS temporary directory; no project dependency was
added. No runtime code, scenario, generated bundle or input archive was changed;
no workaround, full acceptance, commit or publication was performed.
