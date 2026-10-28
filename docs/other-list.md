# Other list category

Use this guide when editing or extending **Altre liste** in legacy politics,
reform politics, or Emilia-Romagna scenarios. It is an approved simulator
aggregation convention, not an additional legal admission rule.

## Scenario editors

- **Includi Altre liste** adds/removes one category. Identity `__other__`, name
  **Altre liste**, and grey `#9ca3af` are fixed. There is no per-list type selector
  or conversion of ordinary lists into Other.
- Reference shares are optional; geographic references and historical
  correspondences use the same rules as ordinary lists. Blank/inherited values
  remain distinct from explicit zero. Mean-mode references retain uncertainty.
- The reform editor also supports chamber presence and geographic voting
  coverage under **Depositi delle liste**. Enabling Other initially includes it
  in both chambers and all existing coverage rows. Restrict that coverage when
  needed. Coverage enables votes, never candidate slates; ordinary legal filing
  minimums do not apply to this category. Camera TAA remains a view of the same
  special ballots, not an independent ballot population.
- Switching off removes Other's shares, geographic overrides, correspondences,
  and coverage. The reform editor previews dependent removals through its usual
  confirmation panel. Switching on again creates a fresh category.

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
retain their votes without any candidate attribution. R direct inputs use
`NA` for that reference and the same `kind` metadata.

Reform inputs explicitly list candidate-free support in optional
`otherListCoverage: [{ collegeId, listId }]`. Ordinary votes include those list
counts alongside filed lists. Special valid votes equal candidate totals plus
Other's unlinked votes, each counted once. National aggregation uses disjoint
physical ballots and derives the overlapping Camera TAA view from them.

Results retain Other's shares and zero seats. It is absent from candidate
probability and bonus-contender selectors. The reform admission table records
`other-list-excluded`, with `scenario:other-list` provenance.

## Compatibility and verification

Legacy politics exports use scenario envelope v8 when Other is present and v7
otherwise; regional exports use v2 and v1 respectively. Both parsers retain older
supported scenarios. Reform fields are additive optional extensions of the
strict scenario/direct-input schemas; older inputs, fingerprints and candidate
generation policies remain usable. Updated archives preserve the category
through JSON and compressed export/import. Use the updated application to
replay scenarios containing Other.

`web/src/lib/core/other-list.test.ts` covers persistence, candidate-free
generation, forbidden associations, admission, geography, historical mapping,
vote conservation, both candidate policies, and extreme shares.
`web/tests/e2e/other-list.spec.ts` exercises all three editors and workers.

For R/TypeScript parity, from the repository root run:

```powershell
$env:LC_ALL = 'English_United States.utf8' # Windows R UTF-8 locale
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' scripts/check_other_lists.R "$env:TEMP/elezioni-other-r.json"
```

Then from `web/`:

```powershell
npx tsx --tsconfig tsconfig.json scripts/check-other-list-parity.ts "$env:TEMP/elezioni-other-r.json"
```

The R checker requires the generated politics golden fixture. It reuses the
first Camera/Senato prepared inputs and constructs regional cases at 1%, 30%
and 80%; it never rewrites fixtures or source data. Regenerate required
fixtures with their existing exporters when absent.
