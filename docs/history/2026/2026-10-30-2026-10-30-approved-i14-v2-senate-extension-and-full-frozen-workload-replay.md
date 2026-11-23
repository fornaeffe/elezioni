## 2026-10-30 — Approved I14-v2 Senate extension and full frozen-workload replay

Luca approved extending I14 to the Senate. `territorial-matrix.ts` now applies
the existing repeated initial ranking to N21 list matrices and N22 internal
lists under S17(5)(d), excluding index/group and regional allocations. It keeps
simultaneous first-pass behavior, freezes reached regional targets per later
whole-matrix pass, preserves college-vote ties without redrawing, and retains
the existing multiply-awarded remainder state. Empty eligible pools and invalid
arithmetic remain explicit failures. I13/I16 and candidate recovery retain their
scope. Warnings and composed metadata record I14-v2; Senate warning text uses
regional targets. Camera's allocation behavior is retained, with the current
I14 version label updated.

Added the frozen Sicilia minimal-matrix regression, N21/N22 repeated-pass,
target-exclusion, permutation, tie, donor-consumption and metadata tests. The
adapter tests exercise N22's imported minority-list operation and I14/I16
transfer/recovery evidence. Replaced the former single-pass failure fixture
with a successful repeated pass, retaining an actual empty-pool matrix failure.
A browser worker reproduces the exact Sicilia result and remainder state.
Updated the specification, core reading map, scoped plan and diagnosis. No new
legal-gap entry was added: this is the approved provisional interpretation.

Both frozen legacy workloads were compared against the pre-extension code at
commit `b3830ec4c127ea45d1409a0eb24bacfc0fb83d6e`, using the identical generated
input per draw and every ID 1–1,000. The no-refresh workload remains 1,000/1,000
complete; the poll-informed workload improves from 999 to 1,000. Every result
has 588 filled seats, 392 Camera/196 Senate, with 588 distinct candidates. Draw
135 alone changes allocation: FI receives Sicilia 1's last seat on pass two;
regional/list targets and its original generated-input hash are unchanged.
Across 2,000 objects, 1,992 are byte-identical and seven change only I14-v1/v2
warning/metadata labels. All original 20 TAA exceptions also pass separately.

Evidence: [paired replay receipt](../../../test/fixtures/benchmarks/senate_i14_replay.json),
its compressed 2,000-row detail ledger, and
[updated TAA/control receipt](../../../test/fixtures/benchmarks/taa_admission_senate_i14_replay.json).
The TAA script now requires completed draw 135 with I14-v2 and writes a new
receipt, preserving the original control/failure records. Original receipt
hashes were checked. Working baseline/implemented bundles, sharded replay
scripts and logs are ignored under
`data/derived/n21-sicilia-implementation-20261030/`.

Verification: **107 unit files / 1,257 tests pass** with `--maxWorkers=2`;
`npm run check:data` and `npm run check` pass with zero diagnostics. Static
builds and both focused browser suites pass at `/` and `/elezioni` (**nine tests
per base path**, including production-worker replay). An initial unrestricted
unit run exceeded two existing five-second deadlines while the sharded replays
were active; reducing test concurrency resolved both without modifying their
deadlines or assertions. `git diff --check` passes.

Marked the scoped migration item complete. These functional Node replays do
not refresh the original browser performance timings or satisfy separate
publication gates. Changes remain uncommitted; no default-generator switch,
push or public deployment was performed.
