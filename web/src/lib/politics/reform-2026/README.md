# Reform scrutiny contracts (R26-05)

Public API: [`index.ts`](index.ts). Legal requirements and later stage boundaries:
[normative specification](../../../../../docs/politics-reform-2026-specification.md).
This folder is independent of the legacy politics contracts and algorithm registry.

| Task | File |
| --- | --- |
| Construct legal inputs / model metadata | [input.ts](input.ts) |
| Describe results, conditional conclusions, draws and recovery | [result.ts](result.ts) |
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
minima and population apportionment remain R26-07. Numerical primitives/draws
remain R26-06, attribution R26-08, special scrutiny R26-10 and initial recovery
R26-17. Scenario/generator, worker and UI integration follow R26-18/19/20.
