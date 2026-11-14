# Common politics editor (CP-06)

Use this guide for editor components, form edits and focused verification. See
[contract.md](contract.md) for the scenario grammar, [files.md](files.md) for file
and draft APIs, and [execution.md](execution.md) for execution adapters.

## Entry points and integration boundary

The review route is `/politics/editor`, linked from `/politics`, with the
configured project base applied. It restores and saves only
`elezioni:politics:common-draft:v1`. The existing simulator remains available at
`/politics` until CP-07 connects the common editor to its execution/session owner.
This avoids replacing a working simulator with an editor-only page during CP-06.

`CommonPoliticsEditor.svelte` receives an immutable `scenario`, an `onchange`
callback and an optional `ExecutionControls` port. It owns cancellable editor-data
loading, staged scenario imports and transient form state. The host owns draft
persistence. The review host reports storage failures while retaining the edited
in-memory draft; it never reports an unsuccessful write as saved.

CP-07 must supply the execution port: run, explicit cancellation, visible task
state/progress, fitting and checkpoint callbacks. Without that port the preview
disables execution/fitting/checkpoint actions and explains their availability.
Automatic editor loading verifies the selected model's pinned parameters; it
does **not** claim that a statistical fit was computed. Dependency-based fit
reuse, immutable executed scenarios, durable result sessions and worker counts
belong to CP-07. Deferred D14 inputs remain in the scenario without extra editors.

## Component map

All implementation paths below are relative to `web/src/lib/politics/editor/`.

| Task | Entry point |
| --- | --- |
| Scenario bar, law/model/renewal selection, staged JSON/gzip loading, reset and error reporting | `CommonPoliticsEditor.svelte` |
| Shared names/colors/affiliation/minority, creation/deletion | `SubjectsEditor.svelte` |
| Reciprocal links, eligibility filtering, retained reasons and replacement disclosure | `BonusLinksEditor.svelte`; uses the domain resolver and edits |
| National/chamber references and SDs/multipliers | `ReferencesEditor.svelte`, `ReferenceFields.svelte` |
| Local overrides, descendant badges, paging/search and subtree deletion | `ReferenceTree.svelte` |
| Law-specific tri-state filings and lazy candidate trees | `FilingTree.svelte`, `CandidateSlots.svelte` |
| Shared candidate biography/availability, explicit gender, assumed birth date | `CandidateForm.svelte` |
| Future chamber locks, source/option memberships, M01 compositions | `MembershipsEditor.svelte` |
| Whole-component pooling, fallback/component references, read-only electorate | `AdvancedEditor.svelte` |
| Run settings, separate visible seed buttons, candidate-generation policies and execution port | `RunControls.svelte` |
| Verified editor assets, labels and tree indexes | `assets.ts` |
| Detached validated form edits and candidate-option explanations | `model.ts` |
| Mounting only expanded content; shared responsive/focus styles | `LazySection.svelte`, `editor.css` |

## Editing invariants

- Domain edits clone and validate before replacement. Invalid edits report an
  error without partially modifying chamber sets, links or pins. Reference scope
  identity does not depend on JSON property order.
- The generator select changes only the active law's saved choice. Law switching
  restores the destination choice, including reform M01. All inactive law/model
  settings, memberships, fallback assumptions and candidate pins remain stored.
- Empty references inherit; zero remains explicit. Default SDs are concrete and
  retain their origin; explicit SDs survive mean edits. M01 hides SD/multiplier
  controls without discarding their values. The visible non-valid input uses
  electors; its advanced section contains SD/chamber/local controls. Full
  model-family feasibility remains with the execution adapter's validation.
- New lists initialize both laws' eligible filings, both chamber sets and a
  stable owned fallback component. The latter is activated by absent historical
  members, rather than inserted as a fictitious historical option.
- Unfiling or excluding a chamber retains candidate pins. Removing a pin leaves
  its identity and other positions intact. Identity deletion uses the domain's
  cleanup across inactive laws and external declarations. Candidate-option checks
  explain age, availability, chamber, list, multiplicity and bonus restrictions;
  they supplement, and never replace, the authoritative legal validators.
- Pooling never invents or splits observations. Removing a pool used in future
  memberships or references requires removing those declarations explicitly
  first; unidentified fitting bases remain errors in the statistical pipeline.
- Exact asset failures cannot trigger latest/default substitution. Imports stage
  decoding, pin verification and contextual validation before replacing the
  draft. A subsequent edit/import or disposal prevents a delayed import from
  replacing newer state. Existing politics draft keys remain untouched.

## Verification

Run npm commands from `web/`:

```text
npm run check
npm test
npm run build
npx playwright test tests/e2e/common-politics-editor.spec.ts --workers=2
```

Build and Playwright must remain sequential. Repeat focused browser checks with
`BASE_PATH=/elezioni`; use `ACCEPTANCE_PREBUILT=1` after the matching build.
`model.test.ts` covers pure edit invariants; the browser suite covers real pinned
assets, switching/reload, both scenario encodings, imports/reset, tri-state
filings, candidate pin retention, reciprocal links, advanced forms and
keyboard/mobile layout. Missing production bundles cause an explicit development
skip; CP-08's production gate must reject absent bundles/skips.
