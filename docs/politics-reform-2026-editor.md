# Reform scenario editor

Use this guide for the structured scenario editor. For generation semantics and
data preparation use [generation](politics-reform-2026-generation.md); for result
storage, execution and exports use [application](politics-reform-2026-application.md).

## User workflow

- **Includi Altre liste** toggles one fixed grey, candidate-free vote category.
  It retains reference, history, geography and chamber-presence controls but
  cannot join coalitions or receive seats. See the shared
  [Other category contract](other-list.md) for accounting and compatibility.

- **Ripristina scenario predefinito** restores the bundled scenario after
  confirmation, including its locally saved draft. It discards pending editor
  changes and previews, preserves calculated results and their executed scenario,
  and is unavailable during loading, execution, import/export or preview checks.
- Lists and coalitions have row-local **Settings** icon buttons. Normally hidden
  details declare Camera/Senato presence; list details also contain protected
  minority controls. A badge marks limited chamber presence or list protection.
  Names, colors and the main membership control edit existing list occurrences
  without restoring an absent chamber. At least one chamber must retain a list;
  use the trash button to remove it entirely.
- Coalition presence declarations are persisted in optional v2 presentation
  `coalitions[].chambers`; older files infer presence from member lists. Turning
  off a chamber unassigns its coalition members. Turning it back on restores
  matching member identities already present there; absent lists are not invented.
  A populated coalition declared in both chambers is automatically linked.
  Empty declarations are retained but cannot form legal links until populated.
- **Ramo riferimenti** defaults to common and affects only shares/abstention.
  Historical mappings have an independent **Ramo corrispondenze** selector.
  Candidate placement already selects its own chamber. There is no global mode.
- Blank percentage fields inherit; zero is explicit. Common shares are model
  anchors, not necessarily identical chamber percentages. Geographic references
  apply in national, region, province, municipality order. Mean mode retains
  election-day uncertainty. Profile descriptions explain the versioned sensitivity
  assumptions; parameters are not edited or fitted in the browser.
- Partial share references constrain only the entered lists. Other lists fill
  the remainder using their local historical proportions and filing support;
  their national shares may adjust differently across disconnected territories.
  Local turnout stays fixed unless separately overridden. References that exceed
  all valid votes in a list's filed geography report the maximum available share.
- New lists start unfiled. **Depositi** filters by geography and college kind;
  a chamber filter joins the region/type filters. Each row edits its own chamber,
  and bulk actions affect only filtered colleges in which the list is present. Absent lists remain distinct from filed zero votes.
- Coalition/filing/list changes may remove dependent mappings, references,
  templates or bonus links. A preview requires an explicit apply action before
  these removals. Supplied vote/preference data blocks structural changes until
  reconciled through JSON. Reciprocal bonus links are explicit declarations.
- **Candidature** edits stable person identities and ordinary/bonus/special
  placements. Editing an existing identity updates its templates together.
  Availability retains filed positions. The [candidate policy](politics-reform-2026-candidates.md)
  places supplied people first and fills remaining bonus slots from frozen
  proportional reference allocations and ordinary prefixes. An ordinary supplied
  person remains outside the bonus unless its identity also has a bonus template.
  **Aggiorna selezione del premio** explicitly upgrades an old policy; parsing or
  editing presentation alone retains archive behaviour. The preview is conditional on simulation 1 and the
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
| Draft coordination, validation, local scopes and JSON | `ReformScenarioEditor.svelte` |
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
