# Reform scrutiny core (R26-05/06)

Public API: [`index.ts`](index.ts). Legal requirements and later stage boundaries:
[normative specification](../../../../../docs/politics-reform-2026-specification.md).
This folder is independent of the legacy politics contracts and algorithm registry.

| Task | File |
| --- | --- |
| Construct legal inputs / model metadata | [input.ts](input.ts) |
| Describe results, conditional conclusions, draws and recovery | [result.ts](result.ts) |
| Compute exact fractions, ratios and six-decimal truncation | [exact.ts](exact.ts) |
| Apply successive legal tie criteria and injected seeded draws | [ties.ts](ties.ts) |
| Allocate integer/exact quotas and single-use remainders | [allocation.ts](allocation.ts) |
| Validate unknown input | [validation.ts](validation.ts) |
| Inspect accepted structure and scalar domains | [shape.ts](shape.ts) |
| Check political references and seat accounting | [validation-context.ts](validation-context.ts) |
| Check filed candidatures and coverage | [candidature-validation.ts](candidature-validation.ts) |
| Check vote and preference consistency | [vote-validation.ts](vote-validation.ts) |
| Run independent constructed examples | [contracts.test.ts](contracts.test.ts), [result.test.ts](result.test.ts) |

## Input boundary

`ReformScrutinyInput` uses `schemaVersion: 1` and the reserved algorithm ID
`politiche-reform-2026-v1`. These identify this legal envelope, not an installed
scenario schema or an executable registered algorithm. No old file is migrated.

```ts
import { validateReformScrutinyInput } from '$lib/politics/reform-2026';

const checked = validateReformScrutinyInput(decoded);
if (!checked.valid) {
  // Show checked.issues: code, path, message, optional legal reference.
} else {
  // checked.input is a separately owned, readonly ValidatedReformScrutinyInput.
}
```

The validator rejects unexpected/missing fields, invalid variants, unknown or
duplicate references, invalid calendar dates and non-finite, negative, fractional
or unsafe integer counts. It does not coerce values, attribute votes, rank
candidates or resolve ties. Counts are **ballots/marks/seats**, not percentages;
model reference shares are fractions in `[0, 1]`. Aggregate vote/seat arithmetic
must remain safe. Preference combination margins are checked with exact integers.

One global candidate identity represents a person across all placements and both
chambers. List, coalition, territory and college IDs belong to their chamber;
equal names/IDs across chambers imply no political link. Bonus links record
explicit reciprocal declarations and may be absent. A joint renewal requires
both chamber payloads; a single renewal supplies one and no bonus slates/links.
External candidature declarations remain necessary for foreign/other-chamber
incompatibilities. Legal recognition of a protected minority is asserted upstream
through its region and legal reference, not inferred from its name.

Supply the full domestic college catalog, both seat capacities, source/version
metadata and one vote row per college, with explicit zeros for all contesting
lists/candidates. Missing rows are errors. An entirely zero-vote election is valid
input; undefined calculations become non-computable **stages** later, under I04.
Electorates use age 18 in both chambers; an unknown elector count is `null`.
Known counts bound valid ballots. Candidate ages are evaluated on election day.

## Votes and preferences

| Representation | Fields | Preference ballot budget |
| --- | --- | --- |
| Ordinary `components` | Direct list votes and separate subject bonus-only votes | Direct list votes |
| Ordinary `attributed` | Final list totals only | Final list totals; composition remains unknown |
| Special `components` | Direct list votes, separate candidate-only votes and candidate totals | Supplied separately on any TAA proportional slate |
| Special `attributed` | Final linked-list totals and candidate totals | Supplied separately on any TAA proportional slate |

Mixed representations are rejected to prevent double attribution. Positive
ordinary bonus-only votes require the applicable filed bonus slate and joint
renewal. Special candidate totals must equal their linked-list totals plus any
explicit candidate-only components. Their sum alone counts valid special ballots;
list votes must not be added again. No proportional attribution is performed here.

Every ordinary slate has seven **filed**, ordered slots with alternating genders;
slot zero is the capolista. Subsequent `availability: unavailable` keeps that
filed slot and is accepted for later proclamation/recovery, rather than turning
an initially complete list into malformed input. Bonus slates belong to a whole
subject, with each candidate's list affiliation retained. Validation checks their
size, rounded national gender limit, ordinary territorial placement/prefix,
same-badge and multiple-candidacy constraints, and special incompatibilities.

Camera half-constituency coverage counts declared ordinary and linked special
candidatures; it applies to lists contesting ordinary colleges, with the declared
minority exception. Senate permits a single region. Both require two-thirds
college coverage in each filed ordinary territory. Special-only lists do not
inherit nationwide Camera coverage. TAA proportional slates use the imported
seven-slot/alternating rules; special uninominal links and national gender checks
remain separate (Camera TAA; Senate all special contests).

Preferences are either `missing` with a reason, or complete non-capolista counts,
including zeros. Supplied counts may assert upstream valid-ballot adjudication;
the boundary still checks identities and aggregate budget bounds. Generated
counts require a constructive certificate of category counts for valid subsets:
zero to three distinct eligible candidates, both genders for multiple marks,
an explicit zero-mark category where needed, exact margins and total budget.
That certificate describes valid aggregate inputs, not invalid-ballot adjudication.
Provenance distinguishes supplied from generated data and carries model,
parameter/profile/source versions, assumptions and seed.

Paired chamber metadata preserves resolved geographic scopes, common anchors,
per-chamber reference origins, dates, covariance/profile version and seed. The
envelope simulation ID pairs the chambers. References at different scopes may
coexist; duplicate references to the same chamber list/scope are rejected. The
R26-19 generator must resolve scope/explicit-share precedence before supplying
this metadata; validation does not fit or execute the statistical model.

## Numerical execution (R26-06)

Use `exactFraction()` to construct nonnegative legal quantities from safe integer
numbers, decimal integer strings or bigint. Outputs are reduced `ExactFraction`
string pairs, safe for JSON and workers; bigint is internal. Arithmetic and
comparisons use exact integers, including intermediates beyond JS's safe range.
Undefined fractions/division by zero and malformed scalar inputs throw. Legal
stages must guard undefined denominators under I04; never substitute zero for an
undefined percentage. `truncateToSixDecimals()` returns an exact fraction,
`floor(x * 10^6) / 10^6`, without rounding or conversion to floating point.

`allocateByQuotaAndRemainders(input, execution)` requires an explicit `quotaMode`:
`integer` discards the quota fraction **before** dividing figures; `exact`
retains it for TAA/attribution or normalized active cap indices. Subjects supply
exact `figure` values, stable IDs, and `tieValues` matching the ordered
`tieCriteria`. Callers choose the statutory geographic scope of each criterion;
there is no default national/local electoral-figure tie-break. `remainderEligible:
false` excludes a subject from residual seats only. Every remainder is usable
once; the stage remains responsible for territorial targets and corrections.

Successful results include raw/applied quotas, exact initial-seat total,
integer seats, vote remainders, quotient decimal parts, remainder consumption,
criterion resolutions, draw traces and warnings. Assignments follow input order;
decisions and canonical draw alternatives do not depend on that order. Zero
requested seats returns zero assignments and null calculations without division.
Positive-seat failures return `status: 'non-computable'` with a coded reason and
**no assignments**: no subjects, zero total, zero integer quota, initial
overallocation or impossible residual allocation. Impossible allocations consume
no draws. These outcomes are for the owning stage to propagate, not complete
scrutiny results or an invented fallback allocation.

`resolveTie()` applies exact ascending/descending criteria in order, tracing the
remaining alternatives. List order, birth dates encoded as sortable integers,
electoral figures and exact percentages can therefore have distinct priorities.
Only a final unresolved tie calls `ScrutinyExecutionContext.draw`. Supply the
stage/norm, chamber, unique context ID, statutory reference, terminal rule and
simulation ID. Use `rule: 'I03'` only for the approved provisional fallback:
every such draw returns a simulation-specific warning linked to its trace.
Statutory draws do not emit an I03 warning. Callers must carry **both** `draws`
and `warnings` into the later composed result and record applied policy versions.

`createSeededDrawContext(seed, streamId)` provides an optional implementation:
the explicit seed and stream ID seed a private RNG; alternatives use code-unit
ID order and unbiased integer rejection sampling. Replay requires the same seed,
stream ID and logical draw call sequence. No default/global RNG or legacy random
stream is used. Custom injected resolvers must return the unchanged request,
an eligible chosen ID and a nonempty stream ID; inconsistent traces are rejected.

Focused fixtures: [exact.test.ts](exact.test.ts), [ties.test.ts](ties.test.ts),
[allocation.test.ts](allocation.test.ts). The I05 index fixture proves numerical
truncation can change a seat; cap-stage orchestration and policy metadata remain
R26-13/15. Zero-target groups must remain inactive before constructing their
undefined quotas/indices.

## Output and remaining stages

`ReformScrutinyResult` is a **type contract**, with complete/partial/non-computable
status, stage status, admission/bonus reasons, conditions on candidate conclusions,
source metadata, policy versions, warnings and separate seat categories. Seat
ledgers preserve original entitlement and actual election location. Allocation
transfers and candidate recovery have separate traces; exact fraction strings
and remainder state include consumption. `DrawRequest`/`DrawTrace` identify stage, alternatives,
rule, context, stream and outcome; the injected execution dependency lives outside
the transferable envelope. I03 draws and I07 cross-region recoveries must produce
warnings when implemented. Output types do not themselves enforce those future
execution rules or validate arbitrary result JSON.

Current endowment checks cover domestic totals 392/196, bonus totals 70/35,
special totals, capacity differences and source compatibility. `constructed`
tables support legal fixtures; `provisional-census` records I08, census date,
geography and table versions; `official-decree` records the controlling table.
Real geography, completeness against official catalogs, constitutional regional
minima and population apportionment remain R26-07. Attribution remains R26-08,
special scrutiny R26-10 and initial recovery
R26-17. Scenario/generator, worker and UI integration follow R26-18/19/20.
