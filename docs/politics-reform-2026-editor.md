# Reform scenario editor

Use this guide for the structured scenario editor. For generation semantics and
data preparation use [generation](politics-reform-2026-generation.md); for result
storage, execution and exports use [application](politics-reform-2026-application.md).

## User workflow

- **Comune ai rami** is the initial editing scope. List names, membership,
  historical mappings and share/abstention references apply to both chambers.
  Enable **modifiche avanzate per Camera/Senato** to expose chamber buttons.
  Explicit chamber share overrides retain precedence after common edits; clear
  their fields in that chamber to inherit again. Labels/colors are shared
  presentation metadata. The advanced subject section can explicitly copy lists
  and correspondences from the other chamber, without inventing new filings.
- Blank percentage fields inherit; zero is explicit. Common shares are model
  anchors, not necessarily identical chamber percentages. Geographic references
  apply in national, region, province, municipality order. Mean mode retains
  election-day uncertainty. Profile descriptions explain the versioned sensitivity
  assumptions; parameters are not edited or fitted in the browser.
- New lists start unfiled. **Depositi** filters by geography and college kind;
  common bulk actions affect both chambers. Individual filings are editable in
  the explicit chamber scope. Absent lists remain distinct from filed zero votes.
- Coalition/filing/list changes may remove dependent mappings, references,
  templates or bonus links. A preview requires an explicit apply action before
  these removals. Supplied vote/preference data blocks structural changes until
  reconciled through JSON. Reciprocal bonus links are explicit declarations.
- **Candidature** edits stable person identities and ordinary/bonus/special
  placements. Editing an existing identity updates its templates together.
  Availability retains filed positions. Bonus previews follow the existing
  planner's ordinary prefixes; compatible templates pin people without changing
  bonus composition/order. The preview is conditional on simulation 1 and the
  current seed; it is not a scrutiny result or a probability estimate.
- **Verifica e anteprima** loads the checksum-verified runtime snapshot in a
  separate worker and checks one generated input. It shows resolved share
  references, weighted abstention and normalization warnings. Editing invalidates
  the preview. Later simulations may still encounter legal non-computability.
- Supplied votes/preferences remain in **Scenario completo e dati forniti
  (JSON)**. Preserve provenance, stable candidate identities and validity evidence.
  Camera TAA special ballots are supplied once; its proportional view is derived.
  A stale JSON draft cannot overwrite newer structured edits.

Only valid scenario drafts replace the locally saved scenario. Execution captures
its own scenario; later edits do not change result exports. File imports retain
the law selector's existing routing. No legacy conversion wizard is provided.

## Persistence and compatibility

Reform scenario v2 adds `presentation`: scenario `name`, list colors keyed by
stable list IDs, and coalition names/colors keyed by coalition IDs. Formatting
metadata never enters legal inputs, parameter fitting or RNG streams. Missing
v1 labels/colors have deterministic identity-based defaults.

The parser/export path preserves v1 reform scenarios verbatim. Opening a v1
scenario for editing explicitly upgrades only the draft to v2. Saved result
sessions and their executed scenarios retain their original version; comparing
the draft to an old result ignores this presentation-only upgrade. Unknown
versions remain errors. This is not conversion from the pre-reform law.

The existing `elezioni:politics-reform-2026:scenario:v1` local-storage key is
retained to discover saved v1 drafts; the stored envelope declares its version.
Generated defaults/catalogs come from `data:reform:generate -- --publish`, never
manual edits.

## Implementation map

All reform paths below are under `web/src/lib/politics/reform-2026/`.

| Task | Entry point |
| --- | --- |
| Workspace execution/import/export | `ReformWorkspace.svelte` |
| Draft coordination, validation, advanced scopes and JSON | `ReformScenarioEditor.svelte` |
| Subject, reference, history, filing, link and candidate UI | Corresponding `Reform*Editor.svelte` files |
| Atomic editing operations and dependency summaries | `scenario-editor.ts` |
| Presentation defaults, proxy-safe cloning and explicit draft upgrade | `presentation-metadata.ts` |
| Compact catalog projection | `editor-catalog.ts`; generated pointer in `data/generation-catalog.generated.json` |
| Preview calculation | `editor-preview.ts`; `web/src/lib/workers/reform-editor.worker.ts` |
| Shared percentage input and bounded location/person search | `web/src/lib/scenario/PercentageInput.svelte`, `SearchSelect.svelte` |

The editor catalog includes geographic labels/IDs and historical list metadata,
not municipal vote matrices. The preparation pipeline fingerprints it and
`data:reform:check` compares it to the runtime snapshot. Lists, searches, filing
tables, templates and preview tables render bounded slices; advanced panels mount
on opening. The earlier politics editor reuses the percentage input and retains
its parser, generator and result UI.

For verification run `npm run acceptance:reform` from `web/`. Focused tests are
`scenario-editor.test.ts` and `tests/e2e/reform-editor.spec.ts`. Full acceptance
also exercises existing result archive replay, both laws, missing production
data, and root/project-base routing. Results/plot alignment remains a separate
item in `MIGRATION_PLAN.md`.
