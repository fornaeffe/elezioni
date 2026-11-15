# Other list category

Use this guide when editing or extending **Altre liste** in common politics
or Emilia-Romagna scenarios. It is an approved simulator
aggregation convention, not an additional legal admission rule.

## Scenario editors

Common politics uses **Attiva Altre liste** with explicit **Componenti del residuo**
selection. Available choices exclude components owned in either chamber and
replace pooled raw members with their pool declaration. No automatic reassignment
from another list occurs. Creation initializes every law's eligible filings and
linked Camera/Senato memberships, using the normal validated domain edits.

Identity `__other__`, name **Altre liste**, grey `#9ca3af` and autonomous non-minority
status are fixed. References, geographic scopes and membership controls follow
ordinary list semantics; blank values differ from explicit zero. Candidate pins,
preferences and bonus links are prohibited. Coverage enables votes without
creating candidate slates. **Disattiva Altre liste** removes the subject and its
dependent references/filings under the shared deletion rules. Reactivation starts
without the former references.

The independent Emilia-Romagna editor retains **Includi Altre liste**, its existing
reference/history controls and persistence contract.

## Calculation contract

`kind: "other"` identifies the category in scenario and scrutiny list metadata;
missing `kind` retains ordinary-list behavior. There can be at most one Other
identity per scenario, shared across reform chamber configurations.

Other contributes to valid-list-vote totals, percentages and the existing
valid-vote threshold denominators. Historical mapping, reference reconciliation,
geographic constraints and stochastic generation treat it as a vote category.
It receives no candidate-only or bonus-only votes.

Other cannot have coalition membership, minority protection, candidates,
preferences, special-candidate supporting links, or bonus links. It is excluded
from every admission route regardless of its share, and never enters seat,
bonus, remainder, recovery, replacement or presidential contender pools.
Invalid supplied associations are rejected. With no eligible recipients, the
run fails explicitly or reports the reform's non-computable allocation path.

Implementation boundaries:

| Layer | Entry points |
| --- | --- |
| Shared identity and toggle | `web/src/lib/core/other-list.ts` |
| Legacy generation and scrutiny | `politics/scenario-projection.ts`, `candidate-generation.ts`, `vote-preparation.ts`, `adapter.ts`, `scrutiny.ts` |
| Regional scrutiny | `regional-er/scenario-projection.ts`, `scrutiny.ts` |
| Reform scenario | `reform-2026/generation-scenario.ts`, `scenario-editor.ts` |
| Reform direct inputs | `reform-2026/input.ts`, `other-list.ts`, `validation-context.ts`, `candidature-validation.ts`, `vote-validation.ts` |
| Reform candidate-free counts | `ChamberInput.otherListCoverage`, `generation.ts`, `figures.ts`, `special-attribution.ts` |

In legacy direct inputs, Other's list-vote rows carry `CANDIDATO: null` and
retain their votes without any candidate attribution.

Reform inputs explicitly list candidate-free support in optional
`otherListCoverage: [{ collegeId, listId }]`. Ordinary votes include those list
counts alongside filed lists. Special valid votes equal candidate totals plus
Other's unlinked votes, each counted once. National aggregation uses disjoint
physical ballots and derives the overlapping Camera TAA view from them.

Results retain Other's shares and zero seats. It is absent from candidate
probability and bonus-contender selectors. The reform admission table records
`other-list-excluded`, with `scenario:other-list` provenance.

## Compatibility and verification

Politics uses the strict common scenario/result/archive formats; old politics
files are rejected without conversion. Internal historical scenario/table codecs
remain regression evidence. Regional formats remain independent (v2 with Other,
v1 without it). Common JSON/gzip files preserve the reserved identity, membership,
filings and references, including inactive law values.

`web/src/lib/core/other-list.test.ts` covers persistence, candidate-free
generation, forbidden associations, admission, geography, historical mapping,
vote conservation, both candidate policies, and extreme shares.
`web/tests/e2e/other-list.spec.ts` exercises the regional editor plus creation, persistence, reactivation and real
zero-seat worker results for all three common politics pairs.

From `web/`, run the maintained TypeScript regression tests:

```powershell
npm test -- src/lib/core/other-list.test.ts
```

The historical R parity check and its one-time checker are retired. Its recorded
outcome remains in migration history; frozen scrutiny fixtures continue to
protect the translated algorithms without requiring R.
