# Reform candidate generation

Use this guide for bonus composition, supplied people and ordinary reuse.
Vote/preference estimation belongs to the [generation guide](politics-reform-2026-generation.md).
The [legal specification](politics-reform-2026-specification.md) remains the
authority for scrutiny. This policy constructs candidatures; it does not award
legal bonus entitlements to coalition member lists.

## Policy and replay

New generated defaults declare
`candidateGeneration.bonusPolicy: "reference-proportional-v1"`. Its absence
retains the original filed-prefix generator, including its original streams,
template interpretation and restriction on automatic reuse of bonus people.
Parsing, exporting and opening old drafts do not silently change that policy.
The candidature editor's **Aggiorna selezione del premio** action explicitly
opts an old draft into the new policy. Saved executed scenarios stay unchanged.

## Reference allocations

Preparation resolves municipal central support once through the existing
historical correspondences, poll references and manual/local overrides. It
projects fractional reference votes to colleges using census membership weights,
filing restrictions and remaining electoral capacity, without random rounding.
Turnout weights come from the resolved references. Neither simulated election
votes nor `suppliedVotes` enter this reference-date calculation.

For each chamber and constituency, normalize the expected vote totals among
coalition lists with local ordinary filings. Allocate the constituency's bonus
candidate slots by largest remainders. Exact ties use a scenario-seeded stream
with no simulation ID. Standalone lists receive all local slots. No admission
threshold is applied to candidate selection.

The central estimates represent assumed party expectations, not exact means of
the nonlinear election-day generator. No new missing-support correction or
territorial proxy is introduced here. Existing zero/tiny support is consumed as
estimated. A coalition with no positive reference support requires supplied
reference estimates or a fully supplied, affiliated bonus slate; the policy
does not fabricate equal shares. A standalone list needs no coalition weights.

Supplied candidates, qualifying-prefix capacity and national gender balance can
constrain Hamilton's allocation. The planner minimizes total absolute departures
from Hamilton first, then squared departures from fractional quotas. Among
equally good allocations it preserves the earliest feasible traversal prefixes,
then prefers national gender balance. Supplied people
take precedence; unconstrained allocations remain Hamilton allocations. Quotas
and feasible female-count budgets are fixed across all simulation IDs.

`GenerationTrace.bonusSelection` records expected votes, normalized shares,
Hamilton counts (`referenceSlots`), actual counts and gender budgets. Adjusted
counts emit `BONUS_SELECTION_CONSTRAINTS`. Result metadata retains the selection
receipt for export and review. Candidate affiliations remain distinct from the
coalition's legal bonus entitlement and subsequent recovery rules.

## Supplied candidates and generated witnesses

1. Place every ordinary template at its supplied college/list/position. Preserve
   person identity and metadata across templates. Supplied genders constrain the
   alternating seven-position ordinary slate.
2. Place every bonus template at its supplied constituency/subject/position.
   An explicit affiliation must match the person's ordinary badge. A null
   affiliation uses that badge, or, for a bonus-only template, the unpinned
   proportional plan. Specify affiliation explicitly when it matters.
3. An ordinary supplied person is eligible for the bonus only if the same person
   ID has an explicit bonus template. Otherwise that person blocks the qualifying
   prefix at its ordinary position, including subsequent positions in that slate.
   Additional ordinary appearances do not constitute a bonus declaration.
4. For each list, seed a uniform permutation of its locally filed colleges using
   chamber, constituency, list and simulation ID. Reuse that permutation when
   traversing heads, second positions, and later positions. Select distinct
   bonus identities. An already supplied person can witness its bonus candidature
   through an existing qualifying appearance or an additional compatible one.
5. When supplied positions constrain the traversal, choose the earliest feasible
   collection of witness prefixes in that traversal. Preserve supplied positions
   and identities. Do not move or automatically promote an ordinary-only person.
6. Fill remaining bonus positions by interleaving list queues in sorted list-ID
   order, retaining each list's traversal order. Supplied bonus positions remain
   occupied throughout. The slate has exactly the territorial bonus endowment.
7. Reserve all witness placements before ordinary reuse. Reuse includes bonus
   people, with the same badge/gender, no duplicate within one ordinary slate,
   and at most the configured number of ordinary colleges (never above five).
   Only one qualifying local prefix is required. Other ordinary uses can lie
   elsewhere and need not be qualifying prefixes. Special/TAA incompatibilities
   and cross-chamber restrictions remain enforced.

The planner enforces national rounded 60% bonus gender limits per subject and
chamber, not per member list or individual constituency. Impossible supplied
positions, affiliations, prefixes, reuse counts or gender combinations fail
explicitly. They do not cause vote changes or a simulation reroll. The final
legal input validator remains authoritative. Single-chamber renewals generate
no bonus slates.

## Implementation and verification

All runtime modules below are under `web/src/lib/politics/reform-2026/`:

| Task | Module |
| --- | --- |
| Fractional reference support, no parameter correction | `bonus-reference-support.ts` |
| Supplied positions and feasible witness prefixes | `bonus-placement.ts` |
| Fixed constrained proportional allocations and national gender planning | `reference-bonus-plan.ts` |
| Generated identities, shuffled traversal, bonus assembly and ordinary reuse | `reform-candidate-generation.ts` |
| Original policy for replay | `legacy-candidate-generation.ts` |
| Preparation and trace integration | `generation.ts` |
| Local qualifying placement in preview | `editor-preview.ts` |

Focused verification: `bonus-candidates.test.ts`, `generation.test.ts`,
`scenario-editor.test.ts`, and `tests/e2e/reform-editor.spec.ts`.
Production defaults/catalogs are regenerated through
`npm run data:reform:generate -- --publish`, then verified by
`npm run data:reform:check`. Do not edit generated defaults by hand.
