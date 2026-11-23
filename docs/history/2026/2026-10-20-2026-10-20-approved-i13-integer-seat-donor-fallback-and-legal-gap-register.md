## 2026-10-20: Approved I13 integer-seat donor fallback and legal-gap register

Luca clarified that the strict C83-bis(1)(b)(3) reading requires
remainder-awarded donor seats and classifies their exhaustion as a legal gap.
This supersedes the preceding investigation's classification of the ordinary
restriction as an implementation defect. The separate cap-group rule expressly
has the same restriction. He authorized a warned loose reading for both when
strict donor exhaustion prevents completion, subject to downstream checks.
Reopened article 1(23) of the controlling GU act; no claim is made that the
workaround represents a binding official interpretation or observed office practice.

Implemented provisional I13-v1 only for Camera N18: ordinary independent list
corrections, the b(3) list rule imported inside cap groups, and cap-group
corrections. Exhaust strict remainder donors first, then rank positive-seat
donors by ascending existing decimal. Repeated integer removals are bounded by
the surplus and remaining seats; tied ranks use the existing seeded I03 draw.
Each fallback removal emits `I13_INTEGER_SEAT_REMOVAL`, severity `warning`,
with simulation/chamber and college/subject context. Applied policy metadata
records I13-v1, and exported list-allocation transfers mark I13. Cap-group
movements retain their detailed matrix trace rather than fictitious candidate
seat IDs. No Senate or Camera constituency donor rule changed.

Integer removals preserve original integer/provisional allocation evidence and
the decimal's exact value and used/unused/consumed state. Thus recovery does
not mistake integer-seat removal for consumption of an unused decimal or a
second remainder donation. Strict donations still consume once. Every operation
reduces an excess by one without negative seats; cap-group recipients stay in
the same college and all communicated list/group totals remain binding.
Recipient exhaustion and numerical failures keep their existing blockers.

Created the [legal-gap register](../../politics-reform-2026-legal-gaps.md), linked from
the transcription, specification, code reading map and active migration plan.
LG01 records ordinary donor exhaustion, LG02 cap-group donor exhaustion, and
LG03 the C3 capacity/upstream constituency-total conflict. LG03 remains blocked
as `constituency-capacity-target-mismatch`; no table resizing or cross-college
cap-group workaround was authorized. The specification and code reading map
now distinguish strict law, the approved workaround and unresolved cases.

Verification:

- Focused constructed/production regressions: **64 passed**. Cover Basilicata's
  exact 42,579 quota, strict-donor priority, repeated integer removals, unchanged
  unused/consumed decimal evidence, cap-group same-college conservation through
  internal list allocation and recovery communications, I13 transfer exports,
  seeded permutation/JSON replay and unchanged LG03/Senate/constituency blockers.
- Production simulation 4 completes **392 Camera + 196 Senate seats**, all 588
  candidate identities unique, with I13 warnings and applied policy metadata.
- `npm run check:data`: pass; `npm run check`: zero errors/warnings.
  `npm test`: **61 files / 872 passed, no skips**. The first full run hit the
  production test's old five-second timeout after adding the second replay;
  set that test's explicit timeout to 30 seconds and the rerun passed.
- `npm run build`: pass. After build completion, `ACCEPTANCE_PREBUILT=1 npm run
  test:e2e`: **21 passed**. No route/component or base-path logic changed.
- Browser production benchmark, 100 simulations / four workers: **passed**,
  25.564 s elapsed, 96 complete / four non-computable. Diagnostics include the
  applied warning: 22 integer removals in 17 simulations. Report:
  `data/derived/r26-i13-review-20261020/browser-100.json`.
  A direct Node replay ran concurrently, so this is browser execution evidence,
  not an isolated latency comparison or replacement acceptance sweep.
- Replayed all **1000 recorded IDs**, unchanged scenario/seed and digest-verified
  production bundle, in four isolated Node workers: **977 complete, 0 partial,
  23 non-computable, 0 exceptions**. The post-I12/pre-I13 replay recorded
  823 complete / 177 non-computable; the net improvement is 154 complete results.
  Recorded **180 I13 removals in 155 simulations**: 130 ordinary independent
  removals and 50 internal cap-list removals. No cap-group fallback was needed
  in this sample; that branch is proved by constructed regressions, including
  downstream internal list allocation and recovery communications.
- Remaining root blockers: Camera N18 has four
  `no-eligible-recipient-remainder` and eight `impossible-residual` results;
  Senate N21 retains ten donor-exhaustion and one recipient-exhaustion result.
  N28 propagates those 23 failures. New reachable recipient blockers do not
  establish a new legal conclusion; no automatic workaround was added for them.
  Exact summary and reproducible runner are under
  `data/derived/r26-i13-review-20261020/` (`replay-summary.json`, `replay.mjs`).

Historical benchmark and post-I12 evidence were preserved. This change adds no
new migration phase, public deployment, commit or publication.
