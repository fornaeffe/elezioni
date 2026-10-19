# Reform scrutiny core (R26-05 through R26-14)

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
| Validate geography and census coverage | [geography.ts](geography.ts) |
| Generate/validate versioned I08 or supplied decree tables | [endowments.ts](endowments.ts) |
| Reconcile incomplete fine-geography population | [population.ts](population.ts) |
| Read the tracked, offline seat-table artifact | [bundled-data.ts](bundled-data.ts) |
| Attribute ordinary votes / aggregate electoral figures / rank preferences | [attribution.ts](attribution.ts), [figures.ts](figures.ts), [rankings.ts](rankings.ts) |
| Determine preliminary special uninominal winners | [special-winners.ts](special-winners.ts) |
| Determine Camera/Senato admission and local TAA exceptions | [admission.ts](admission.ts) |
| Decide the paired 70/35 governability bonus | [bonus.ts](bonus.ts) |
| Allocate Camera national subjects/lists and apply the 220-seat cap | [camera-national.ts](camera-national.ts) |
| Allocate Camera constituencies, coalition lists and colleges | [camera-territorial.ts](camera-territorial.ts) |
| Inspect shared territorial matrix numerics and correction movements | [territorial-matrix.ts](territorial-matrix.ts) |
| Allocate Senato regional subjects and apply the 113-seat cap | [senate-regional.ts](senate-regional.ts) |
| Allocate Senato regional coalition lists and ordinary/cap colleges | [senate-territorial.ts](senate-territorial.ts) |
| Attribute special candidate-only votes | [special-attribution.ts](special-attribution.ts) |
| Allocate Camera TAA proportional seats and provisional candidates | [taa-allocation.ts](taa-allocation.ts) |
| Reuse special winners and summarize cap entitlements | [special-seats.ts](special-seats.ts) |
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
| Special `components` | Direct list votes, separate candidate-only votes and candidate totals | Territorial direct list votes; counts on the TAA slate |
| Special `attributed` | Final linked-list totals and candidate totals | Territorial final list totals; counts on the TAA slate, composition unknown |

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

## Electoral figures and rankings (R26-08)

`distributeBonusOnlyVotes(input, execution)` receives one ordinary plurinominal
college's validated `OrdinaryVotes`, coalition membership, chamber, simulation ID
and college ID. Component inputs use exact `q = directTotal / onlyVotes` for each
subject, integer additions and descending remainders. Traces preserve quota,
quotients, additions, remainders and I03 draws/warnings. Zero only-votes and a
single linked list avoid division. Multiple linked lists with positive only-votes
but no direct support return `non-computable: zero-direct-support`, without
partial totals or draw consumption. `attributed` inputs return owned final totals
with no allocations or draws; mixed variants are rejected. Attribution uses the
original direct counts, never another subject's additions. Subjects and tied IDs
are canonicalized independently of input-array ordering.

`aggregateElectoralFigures(chamber, ordinaryFigures, specialFigures)` receives
complete attributed list rows for every ordinary and special uninominal college.
Pass successful N04 outputs as `ordinaryFigures`. For `specialFigures`, pass
supplied final special list totals, or successful `attributeSpecialVotes()` outputs.
Do not pass candidate totals or count them
again. Missing/extra colleges and missing/duplicate/non-contesting list rows throw.
These functions require already validated political/geographic references;
they do not replace `validateReformScrutinyInput()` or compose scrutiny stages.

Outputs contain college, constituency/territory, region and national figures,
all-list valid totals and unfiltered coalition totals. `national` includes VdA
and TAA for admission, first place and bonus eligibility; `ordinaryAllocation`
excludes both special territories. There is no admission filter, so excluded
votes remain in denominators and sub-1% members remain in coalition figures.
Camera `taaProportional` contains views of the TAA territorial figures derived
from its uninominal ballots. The envelope's TAA `ordinaryVotes` row is not an
extra ballot contribution: never include it among `ordinaryFigures`. Its slate
still supplies preferences. This avoids counting the same territorial ballots
twice. Senate has no TAA proportional view.

`rankPreferenceCandidates(slate, figures)` receives the college figures (the TAA
constituency view for Camera TAA). It excludes the capolista and orders the other
filed candidates by preference counts, then filed position, without a draw or
availability filtering. Missing preferences return an explicit missing outcome;
explicit zeros retain list order. List and individual percentages use **all valid
votes** in that scope. `calculatePercentages(counts, validVotes)` returns exact
fractions in percentage units, or `undefined: zero-valid-votes` under I04. A
zero-vote election still has complete count aggregates, without defined shares.
Unsafe integer counts or sums throw instead of losing precision.

Own separate injected streams for attribution and downstream legal draws. The
representation-equivalence fixture supplies identical candidate/preferences and
final totals after a tied attribution; figures, rankings and downstream draw
traces match exactly. This proves accounting equivalence, not empirical model
calibration or absence of statistical bias. See [attribution.test.ts](attribution.test.ts),
[figures.test.ts](figures.test.ts) and [rankings.test.ts](rankings.test.ts).

## Admission and preliminary special winners (R26-09)

Use already validated chamber/candidate references and N06 aggregates from the
same simulation. These are pure legal-stage helpers, not a composed scrutiny
entry point. No statistical model or ordinary/special proportional allocation
runs here.

1. Call `determineSpecialWinners({ chamber, candidates, simulationId }, execution)`
   once for each chamber. Both special vote representations already contain the
   adjudicated `candidateTotals`; list attribution is unnecessary for this call.
   Highest candidate votes win regardless of linked-list admission. Camera uses
   the younger candidate on a vote tie; Senato uses the older. Equal dates use
   warned I03 draws. All special colleges/counts/references are preflighted before
   draws, then colleges and alternatives are processed canonically.
2. Call `determineCameraAdmission({ chamber, figures, simulationId,
   specialWinners }, execution)` or `determineSenateAdmission({ chamber, figures,
   simulationId }, execution)`. `figures.national` must include special ballots;
   never substitute `ordinaryAllocation`. Camera requires the complete preliminary
   winner result for the same chamber/simulation. Senate admission does not depend
   on those winners.
3. Retain the preliminary winner ledger for R26-10, later cap accounting and
   proclamations. It preserves college/territory/region, candidate, subject,
   every linked list, separate affiliation, votes, legal reference and tie trace.
   Admission never recomputes it or repeats its warnings/draws. A future
   coordinator must collect each stage's top-level warnings/draws once; nested
   winner tie traces describe those same draws, not additional events.

Admission compares integer counts with bigint products at inclusive 3%, 10% and
20% thresholds; rounded percentages never decide access. Valid-vote denominators
retain excluded lists. A 10% coalition needs a qualifying member. Qualifying
members enter through (2-bis); exactly one otherwise excluded member enters
through (2-ter), without a 1% floor. If none are excluded there is no rescue or
draw. Equal best national figures use warned I03 draws in canonical coalition
order. An all-zero national denominator returns `non-computable:
zero-valid-votes`, with no invented admission/exclusion decisions. Zero-vote
regions cannot qualify a Senate list through 20%.

`coalitions` and `lists` expose decisions, reason codes, legal references and exact
national shares. `qualifyingRegionIds` records the 20% evidence; Camera's
`minorityQualification` also records local shares, college counts, required
`ceil(colleges/4)` wins and actual linked wins. `admittedAs` distinguishes a
standalone list (including independent access after coalition failure) from a
member of an admitted coalition. `admittedSubjects` contains only the admitted
coalitions and standalone lists for later subject allocations/bonus ranking;
coalition members are not counted as additional subjects.

Camera's minority routes require upstream protected-minority recognition,
autonomy and exclusive presentation in the declared region. Only an unlinked
minority can qualify nationally through a quarter of uninominal wins. Those wins
do not qualify a coalition or a (2-bis) member. A minority in a failed coalition
has only the independent national 3% route; `taaLocalExceptions` separately
records I01 access at local 20% under C93-septies(2), preserving its anomalous
(2-bis) reference. R26-10 unions these local standalone lists with the
nationally communicated subjects without duplicating an already admitted list.
Local exceptions do not add nationwide subjects. Senate's 20% route is available
to all lists in any region, including special regions and failed-coalition
members, and permits national access rather than restricting access to that
region. See [admission.test.ts](admission.test.ts) and
[special-winners.test.ts](special-winners.test.ts) for independent worked fixtures
and the validated-envelope integration checks.

## Special territories (R26-10)

Use validated chamber/candidate references and stage results from the same
simulation. These helpers implement domestic **initial** special scrutiny;
they do not dispatch a worker, decide a bonus/cap, resolve multiple elections or
simulate later vacancies and supplementary elections.

1. Compute N09 preliminary winners once, as described above. Independently call
   `attributeSpecialVotes({ chamber, collegeId, simulationId }, execution)` for
   each special uninominal college. Pass its complete results to N06 aggregation,
   alongside complete ordinary N04 results. Positive candidate-only votes with
   multiple linked lists and no direct support block the required figures;
   do not construct partial national admission totals.
2. Determine N07/N08 admission using those aggregates and the retained winners.
   For Camera call `allocateCameraTaaSeats({ chamber, candidates, figures,
   admission, collegeId, simulationId }, execution)` on its TAA proportional
   college. Use its invariant proportional capacity directly: the uninominal
   seats are already separate and must not be subtracted again.
3. Call `proclaimSpecialUninominalWinners({ chamber, candidates, simulationId,
   preliminary })` using the same N09 result. No draw dependency is accepted;
   it checks matching college/candidate totals and political links. Proclamations
   retain winners whose linked lists failed admission. Availability can leave
   an unresolved identity, without deleting the seat entitlement. The owned
   `preliminary` communication preserves candidate affiliation and **all** links;
   coalition seat locations have `listId: null` rather than assigning the seat
   to a member by personal affiliation. Collect N09 warnings/draws once, not
   again from this nested communication.
4. `summarizeSpecialSeats(uninominal, taa?)` returns separate uninominal and TAA
   proportional entitlement totals per subject, suitable for the future cap
   stages. Camera requires the computed TAA entitlements for complete totals;
   omitted/non-computable TAA counts remain `null`. Senate has zero proportional
   special seats. Missing preferences/availability do not erase known
   entitlements. Neither ordinary nor foreign seats enter this summary.

N10 uses exact candidate-only attribution at **each uninominal college** before
any territorial sum. Single linked lists receive their only-votes directly;
zero only-votes require no division; equal residuals use warned I03 draws with
no unprescribed vote secondary criterion. All candidate groups in a college are
preflighted before any draw and processed in canonical order. Senate I02 reads
the S21 link reference together with S20's candidate-only ballot rule. Attributed
inputs are cloned and bypass redistribution; candidate totals never change.
Traces retain per-candidate additions, quotas, remainders, draw IDs and warnings.

N11 uses exact quotas for subjects **and** internal coalition lists, then local
electoral figures and statutory draws on equal remainders/figures. These draws
are not I03 warnings. Subjects are the national C83(1)(e-bis) communication union
I01 local exceptions, deduplicated. Failed-coalition minority access does not
change national admission. Within an admitted coalition, C93-septies(2)(b)
distributes to all locally filed coalition lists: unlike ordinary C83(1)(g), it
does not restrict internal allocation to (2-bis)/(2-ter) lists. Coalition figures
include those local votes. Tests distinguish these two legal routes explicitly.

Results retain subject and coalition allocation traces, list entitlements,
preference ranks and a provisional seat ledger. The capolista precedes the
non-capolista preference order, with filed order on equal preferences. List and
individual percentages use all valid TAA ballots. Preference validation uses
territorial special-list ballots, excluding candidate-only components in raw
inputs; the compatibility TAA `ordinaryVotes` row supplies no additional budget.
That row remains required by input version 1 but never contributes to N06/N11
figures. Raw/attributed equivalence tests isolate attribution and downstream draw
streams; no statistical generation is involved.

An N11 `complete`/`partial` result has `entitlementStatus: complete` even if some
identities are unresolved. Missing preferences still permit the head's seat;
subsequent preference-dependent seats remain unresolved. Explicit zero counts
are ranked normally. Unavailable/exhausted candidates require R26-16/17; do not
silently skip them. The ledger is provisional, including legally permitted
special/ordinary multiple candidacies: R26-16 must apply uninominal precedence
and R26-17 must refill released seats before final whole-chamber conclusions.
A required non-computable quota/admission returns a named failure with no
invented list entitlements. See [special-attribution.test.ts](special-attribution.test.ts),
[taa-allocation.test.ts](taa-allocation.test.ts) and
[special-seats.test.ts](special-seats.test.ts) for worked and validated-envelope
fixtures; age-tie rules remain covered by [special-winners.test.ts](special-winners.test.ts).

## Joint bonus decision (R26-11)

`determineGovernabilityBonus(input, execution)` receives the simulation ID,
declared `renewedChambers`, explicit `bonusLinks` from the legal envelope, and
one `{ chamber, admission, national }` record per available renewed chamber.
Supply the same simulation's N07/N08 admission and N06 `figures.national`, which
includes VdA/TAA ballots once. Do not supply `ordinaryAllocation` or totals
renormalized over admitted subjects. Stage records do not carry an independent
simulation ID; pairing their provenance remains the caller's responsibility.

The function ranks only `admittedSubjects` (coalitions and standalone lists),
checks exact counts against 42% independently in each chamber, and requires
explicit reciprocal links between the two selected first subjects. Equal names
or IDs imply no link; subject kind remains part of identity. A lower linked
subject cannot replace an unlinked first subject. Every unresolved first-place
tie uses an injected, warned I03 draw, with canonical alternatives and fixed
Camera-then-Senato order. Admission draw events are not re-emitted.

Results distinguish `awarded` (the paired 70/35 subject entitlements),
`not-awarded` (structured threshold, linkage or single-renewal reasons), and
`non-computable` (missing joint data, blocked admission, undefined denominator,
empty admission or missing subject figures). Both chambers are preflighted
before drawing. A single renewal follows its statutory no-bonus path without
ranking or drawing; missing data in a declared joint renewal remains a blocker.
Computed joint results retain both first subjects, vote counts, all-valid
denominators, exact percentages, threshold decisions and N12 draw/warning traces.
Coalition awards stay with the coalition. No seat ledger, cap computation,
candidate election or distribution among coalition lists occurs here.
See [bonus.test.ts](bonus.test.ts) for independently constructed divergent
chambers, threshold/tie effects, special votes and validated-envelope composition.

## Camera national allocation (R26-12)

`allocateCameraNationalSeats(input, execution)` consumes the validated Camera
chamber, N06 figures, N07 admission, N12 bonus result, N10 `specialSeats` summary
and simulation ID. Use communications from the same run; N06/N07/N10 totals have
no independent run ID. The bonus ID is checked. Only
`figures.ordinaryAllocation.lists` supplies allocation votes; admission/bonus
totals include special ballots and must not replace them.

The completed result selects `no-bonus`, `bonus-without-cap` or `bonus-with-cap`.
The first allocates all 384 ordinary seats. The other paths first allocate 314
ordinary seats, reserving 70 for the bonus subject. The cap check counts the
winner's ordinary, bonus and linked special entitlements, including unresolved
candidate identities. Exactly 220 remains unchanged; a greater total fixes
majority ordinary seats to `150 - winnerSpecial`, then reapportions
`314 - majorityOrdinary` among the other admitted subjects. The majority quota
is floored for R26-13; it does not recompute the majority's fixed entitlement.
Special totals must be complete before the bonus path starts drawing. Incomplete
special totals do not block a computed no-bonus allocation.

C83(1)(f) includes all linked coalition votes; (g) divides the coalition target
only among its admitted (2-bis)/(2-ter) lists, including the one best excluded
member. All subject, minority and internal-list allocations use integer quotas,
then exact remainders, ordinary national figures and statutory seeded draws.
Zero-seat coalitions need no division. I04 failures return a named stage/reason
and attempted traces without publishing final targets. Upstream draws/warnings
are not repeated.

Outputs retain the provisional allocation, cap check, majority/minority quotas,
minority and coalition allocations, final subject/list ordinary targets, owned
seat-table provenance and stage draw traces. Bonus seats retain subject ownership
and are never distributed among coalition members. The input endowment must
reconcile 392 domestic seats, eight special seats, 384/314 ordinary capacities
and 70 bonus seats; incompatible tables are rejected under I08 rather than
rescaled. Estero has no input into allocation or the cap. The bundled table
remains explicitly provisional census data, with its version and population notes.

See [camera-national.test.ts](camera-national.test.ts) for worked quota/remainder
fixtures, the 220/221 boundary, internal-list filtering, seeded ties, arithmetic
failures and validated single/joint-envelope composition. Ordinary territorial
allocation follows below; bonus proclamations and candidate outcomes remain later stages.

## Camera territorial allocation (R26-13)

Supply `{ chamber, figures, national }` to
`allocateCameraTerritorialSeats(input, execution)`. Use the validated Camera
input, complete N06 electoral figures and N13/N14 national result from the same
run. National simulation ID and seat-table provenance carry through each result;
N06 figures have no independent run ID, so their pairing remains the caller's
responsibility. Only ordinary territories/colleges enter these allocations.
The national path selects the college endowments with or without the bonus.

The scoped coordinator returns three separately inspectable stage results:

| Stage API | Operation |
| --- | --- |
| `allocateCameraConstituencies(input, execution)` | N15 subject matrix under C83(1)(h), or N17 when the cap applies |
| `allocateCameraCapConstituencies(input, execution)` | Explicit N17 entry point requiring the corrected cap path |
| `allocateCameraCoalitionListsInConstituencies(input, constituencies, execution)` | N16 internal coalition-list matrices under C83(1)(i)/(1-ter)(e); standalone targets pass through |
| `allocateCameraColleges(input, lists, execution)` | N18 college groups/lists under C83-bis; requires completed constituency-list targets |

Each complete stage publishes ordinary entitlement targets. A non-computable
stage retains attempted matrices, draw/warning evidence, reason and affected ID,
without publishing final targets. Upstream draw events are not repeated by
downstream stages. The coordinator retains all three stage results; preserve
their metadata when composing the full scrutiny in R26-18.

The internal matrix engine exposes capacities, raw/applied quotas, cap index
divisors, raw/truncated indices, normalized attribution quotients, integer seats,
decimal parts, reached-target exclusions, provisional/final seats and remainder
states. It computes integer parts across all areas before excluding targets
already reached from their parallel remainder allocations. This avoids assigning
an arbitrary constituency/college priority. Each initial remainder is usable
once. Zero-seat divisions are skipped; I04 arithmetic failures are explicit.

Initial constituency subject ties use ordinary national figures; initial
coalition-list ties use constituency figures. N16 corrections import C83(h)'s
national criteria. College list/group residual ties also use constituency figures,
never college votes. Initial final ties use statutory draws. Missing correction
tie-breaks use canonical, warned I03 draws. The cap path truncates majority and
every minority subject index before constituency normalization, retaining
`{ id: 'I05', version: 'I05-v1' }` in the constituency result. College group
indices are both truncated as expressly required by C83-bis(1-bis)(b). A
zero-target group is inactive and its undefined quota/index is not evaluated.

Correction rules remain distinct:

- C83(h), also imported by (i)/(1-ter)(d): largest surplus then national figure;
  used donor decimals ascending, unused recipient decimals descending. Search
  all same-constituency opportunities before the cross-constituency fallback.
  Same-constituency recipient ties use national figures; unspecified fallback
  destination ties use I03. National subject/list targets and total ordinary
  seats are conserved; a fallback can change individual constituency totals.
- C83-bis(1)(b)(3): donor and recipient colleges follow their independent decimal
  rankings. Constituency list targets and the constituency total are conserved;
  individual college totals can change. This rule also applies inside each cap
  group after its corrected college distribution.
- C83-bis(1-bis)(c): cap-group transfers occur within the same college, conserving
  each college capacity and the constituency's group totals.

Every movement records donor/recipient cell IDs, seat counts before transfer,
same/cross-area route, exact remainder transitions and applicable draw context
IDs. Donated remainders become consumed; recipient remainders become used.
These are allocation movements, separate from candidate-recovery seat ledgers.

If an h/i cross-constituency fallback leaves the next constituency's communicated
total incompatible with its fixed college endowment, N18 reports
`constituency-capacity-target-mismatch`, retaining the completed N15/N16 results.
There is no implicit decree-table rescaling or compensating transfer. Composition
must preserve this non-computable state; any further reconciliation requires
reviewed legal support. A correction without an eligible donor or unused
recipient likewise reports its explicit no-progress reason.

See [camera-territorial-matrix.test.ts](camera-territorial-matrix.test.ts) for
hand-worked matrices, correction ordering, changed local totals, truncation
boundaries and changed-seat cases; [camera-territorial.test.ts](camera-territorial.test.ts)
for stage communications, denominators, zero targets and blockers; and
[camera-national.test.ts](camera-national.test.ts) for validated single/joint
envelopes through N18. These functions establish ordinary entitlements only.
Bonus seats remain owned by the national winning subject; candidate proclamations,
multiple elections, exhaustion and application integration
retain their subsequent plan steps.

## Senato regional allocation (R26-14)

`allocateSenateRegions()` consumes validated Senate geography/endowments and the
same run's N06 figures, N08 admission, N10 special entitlements and N12 bonus
decision. The N12 simulation ID is checked; N06/N08/N10 lack independent run IDs,
so their pairing remains the caller's responsibility. Endowments must conserve
196 domestic seats: 189 ordinary plus 7 special, or 154 ordinary plus 35 bonus
plus 7 special. The ordinary reduction must match each region's bonus capacity.
Seat-table provenance remains owned output metadata; census tables retain I08's
provisional label.

N19 allocates each ordinary region independently, using its integer quota,
remainders, regional electoral figures and statutory draws. Coalition figures
include every linked list, including excluded internal members. Special votes
never enter these regional allocations. National subject totals are sums of
regional allocations, with provisional and final totals exposed separately.
For coalition-to-list and college allocation, continue with N21/N22 below.

`applySenateCap()` is independently callable with completed N19 regional
allocations, the bonus subject, its special entitlement count and simulation ID.
It does not re-emit N19 events. Exactly 113 domestic seats need no correction;
above that, the ordinary target is `78 - specialSeats`, retaining all 35 bonus
seats. Cap checking uses entitlements even when candidate identities are missing.
No bonus means no cap check or correction.

The cap result exposes an ascending national ranking of the winner's **raw vote
remainders** that supplied seats. Decimal quotient parts are not comparable
across different regional quotas. Equal values share a rank; canonical array
order only serializes them. I03 resolves their legal order when a removal needs
it, so an unchanged allocation consumes no unnecessary tie draws.

N20 first removes each ranked used remainder at most once. The first percentage
pass excludes those regions and protects regions with only one majority seat.
S16-bis(1-ter)(a)'s sixth sentence repeats only the third and fourth sentences
(percentage ranking and one seat per region), not the fifth sentence's initial
exclusions. It supplies a current-last-seat exclusion instead. Thus further
passes readmit remainder-phase regions while protecting every final seat.
Each pass removes at most one seat per region, ranking the winner's exact
percentage against **all** valid regional votes, including excluded lists. The
one-seat protection belongs to percentage passes: a sole used remainder seat
can be removed in the preceding remainder phase.

Each removal is paired with a reassignment in the same region. A recipient's
largest unused remainder is consumed once; after unused minority remainders
run out, regional votes determine recipients. Missing terminal tie-breaks use
warned I03 draws, including equal removal ranks, equal percentages, equal unused
recipient remainders and equal fallback vote totals. Do not import N19's express
secondary vote criterion into these different correction decisions.

Cap corrections record `{ id: 'I06', version: 'I06-v1' }`, exact ranks,
percentage-pass numbers, donor/recipient seats and remainder states before and
after each movement, tie resolutions, draws and warnings. A donated used
remainder becomes `consumed`; an unused recipient remainder becomes `used`.
Regional endowments are preserved after every paired movement. No eligible
recipient or no lawful removal progress returns an explicit non-computable
result, retaining provisional allocations and attempted movements without final
targets. Missing prerequisite stages block before allocation draws; arithmetic
failures retain the affected region and quota trace.

See [senate-regional.test.ts](senate-regional.test.ts) for hand-worked cap paths,
repeated passes, single-use states, tie criteria, seeded replay/permutations,
failure paths, the bundled endowment and validated single/joint envelopes
through N22. These stages establish subject entitlements; internal list targets
follow below, while bonus candidates and recovery retain their later boundaries.

## Senato list and college allocation (R26-15)

`SenateTerritorialInput` requires the validated Senate chamber, same-run N06
figures, N08 admission and completed N19/N20 regional communication. Regional
targets are final, including any cap correction. Simulation and seat-table
identity are checked where available; N06/N08 pairing remains the caller's duty
because those stages have no run IDs. Every ordinary region must retain its
selected college endowment, and college list figures must sum to regional ones.

| Function | Responsibility |
| --- | --- |
| `allocateSenateRegionalLists(input, execution)` | N21, S17(1)(b),(2): coalition-list integer quotas, remainders, regional votes and statutory draws; standalone lists pass through |
| `allocateSenateColleges(input, lists, execution)` | N21/N22: dispatch ordinary or corrected-cap college allocation from completed regional list targets |
| `allocateSenateCapColleges(input, lists, execution)` | Independently callable N22 boundary; requires the corrected cap path |
| `allocateSenateListsAndColleges(input, execution)` | Scoped coordinator retaining both list and college stage results |

Coalition allocation includes only N08's admitted 2-bis/2-ter members. Its
denominator excludes other coalition members even though N19 counted their
votes at subject level. Targets retain region, subject, list, votes and ordinary
seats. No bonus seats are divided between coalition lists. Source metadata is
owned output and preserves I08's provisional label.

A single ordinary college receives its regional list targets directly, including
zero targets, without an additional quota. In multiple-college regions, only
positive-target lists enter S17(4) denominators. No bonus and bonus without a cap
correction both use integer college quotas. Remainder ties use **college** list
votes, then statutory draws, unlike Camera's constituency figures. Reached
integer targets are excluded from the parallel remainder allocations. Donor
used decimals ascend and recipient unused decimals descend independently;
transfers conserve regional list targets and region totals but can change
individual college totals. Missing correction tie-breaks use warned I03 draws.

After an actual cap correction, S17(5) first divides the region's positive-target
lists into majority and minority groups. Both regional group quotas discard
their fractional part. Both college indices are truncated to six decimals before
normalization by their sum and college capacity. Equal decimals use **college
group** votes, then statutory draws. Group corrections transfer seats within the
same college; S17(4) then allocates lists separately within each corrected group.
Zero-target groups are inactive and their undefined quotas/indices are skipped.
Both truncations are express statutory rules here, requiring no I05 policy.

The shared `territorial-matrix.ts` engine takes an explicit chamber, statutory
references, vote tie criterion and correction route. Camera retains its existing
criteria and draw contexts. Matrix traces expose divisors, raw/truncated indices,
normalized quotients, integer/provisional/final seats, remainder exclusions and
states, movements, resolutions, draws and warnings. Regional coalition quota
traces are kept separately. Downstream results do not re-emit upstream events.

Non-computable prerequisites, I04 arithmetic failures and no-progress corrections
retain attempted traces, reason and affected region without publishing final
targets. There is no invented divisor, equal-share fallback or seat-table
rescaling. Outputs are owned, JSON-safe and deterministic under input permutations.
See [senate-territorial.test.ts](senate-territorial.test.ts) for worked fixtures
and [senate-regional.test.ts](senate-regional.test.ts) for validated single/joint
envelopes through N22. Candidate proclamations, multiple elections, exhaustion,
registration, generation, workers and UI remain in subsequent plan steps.

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

R26-07 adds the versioned real geography and census-based provisional seat tables.
For acquisition, generation, source/estimation limitations and official-table
replacement, use only the [preparation workflow](../../../../../data/README.md).
`validateTerritoryCatalog()` checks municipal and college coverage/populations;
`validateSeatTableBundle()` checks both chambers, constitutional minima, special
structures, 70/35 bonuses and alternative capacities. `getBundledReformSeatTables()`
and `chamberGeography()` return owned data without requiring downloaded archives.
`constructed` inputs remain usable independently for legal fixtures. The additive
optional `SeatTableSource.populationNotes` preserves user-facing reconstruction
warnings in the existing schema version. Scenario/result integration must retain
the referenced immutable artifact and display `seatTableMessages()` in R26-20.
Ordinary attribution, figures and rankings are available through R26-08 above;
admission and preliminary special winners through R26-09; special attribution,
TAA proportional allocation and entitlement summaries through R26-10; the paired
bonus decision through R26-11, Camera national allocation/cap through R26-12,
ordinary Camera territorial allocation through R26-13 and Senate regional
allocation/cap through R26-14 and Senate list/college allocation through R26-15.
Proclamations and multiple-election precedence remain
R26-16 and initial recovery R26-17. Composition, scenario/generator, worker and
UI integration follow R26-18/19/20.
