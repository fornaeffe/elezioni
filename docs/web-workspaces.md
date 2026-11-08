# Web workspaces

Use this guide for workspace routing, scenario defaults, runtime loading and
shared UI. Use [data preparation](../data/README.md) for acquiring/importing data
and the [unified M01/M02 guide](vote-generation-unified-workflow.md) for reform
generation, fitting and model semantics.

## Repository boundary

Keep the app and its Node toolchain in `web/`, even after R retirement. The root
also owns shared source data, legal/model documentation, frozen regression
evidence and independent research. Moving the app would require coordinated
changes to preparation paths, fixture URLs, research replay bindings and docs
without removing those separate responsibilities. Run npm in `web/`, or use
`npm --prefix web run <script>` from the root. Reconsider a move only as a
separate repository-layout change with its own path and base-URL verification.

## Task map

| Task | Entry point under `web/` |
| --- | --- |
| Supported election/law/model combinations and limits | `src/lib/workspaces/registry.ts` |
| Common-politics domain and execution (CP-02/03; editor/session integration remains later) | `src/lib/politics/scenario/index.ts`, `src/lib/politics/execution/index.ts`; [contract](common-politics-scenario/contract.md), [execution](common-politics-scenario/execution.md) |
| Route composition and law selection | `src/routes/politics/+page.svelte`, `src/routes/emilia-romagna/+page.svelte` |
| Previous-law politics and ER editors | `src/lib/workspaces/LegacyPoliticsWorkspace.svelte`, `RegionalErWorkspace.svelte` |
| Reform adapter and specialized fitting/storage | `src/lib/politics/reform-2026/ReformWorkspace.svelte`, `vote-share/FastReformWorkspace.svelte` |
| Shared navigation, controls, messages and tables | `src/lib/workspaces/WorkspaceShell.svelte`, `RunControls.svelte`, `WorkspaceMessages.svelte`, `ResultTable.svelte` |
| Owned worker lifecycle and draft persistence | `src/lib/workspaces/controller.svelte.ts`, `draft.svelte.ts` |
| Metadata routing and one-shot cross-route imports | `src/lib/workspaces/import-routing.ts`, `navigation.ts`, `handoff.ts` |
| Snapshot resolution and schema/checksum checks | `src/lib/workspaces/data.ts`, `snapshots.generated.ts` |
| Memory/stored result pages | `src/lib/workspaces/result-access.ts`; exports remain in `src/lib/core/result-export.ts` |
| Shared-default compiler and descriptor generation | `scripts/workspaces/defaults.ts`, `generate.ts` |
| Standalone list tickets at the previous-law boundary | `src/lib/scenario/politics-tickets.ts` |

The installed workflows are previous-law politics, reform politics with M01/M02,
and Emilia-Romagna regional elections. Municipal simulation remains unavailable;
its type does not register a runnable workspace. The default politics law remains
the previous law. Registry validation also runs at the worker boundary.

## Defaults and compatibility

[`data/fast-share-future-scenario.json`](../data/fast-share-future-scenario.json)
is the single authored politics choice set, retaining its established filename
and identity. Both default compilers consume it. Do not edit generated defaults.

The previous-law compiler uses the promoted source-membership policy and reviewed
legacy comparison weights. Unspecified internal splits use explicitly labelled
equal-member assumptions; unmapped votes keep the legacy abstention semantics.
Historical lists retain projected means; reviewed named polls initialize editable
shares, automatically enabling national overrides for lists without compatible
historical membership. Accepted reform Camera/common reference means supply
special-territory/provincial anchors in the older shared chamber field. This does
not give reference SDs a new legacy meaning or convert M02 fitting to the old model.
The generated scenario carries these assumptions in `defaultSource.notes`.

Scenario schema 9 adds optional per-chamber constituency filing and minority
region declarations. Older scenarios keep unrestricted filing and their existing
interpretation. Camera and Senate use their own geographic namespaces; restricted
Camera filing becomes whole corresponding Senate regions, following the authored
policy. The editor retains the two political coalitions and `null` coalition
membership for autonomous lists. Standalone lists use singleton tickets only
inside the legacy adapter. This is
an electoral representation, not a claim of political coalition membership.

New filing conditions the shared generated field separately per chamber while
preserving each unit's non-valid fraction, and filters candidate slots/reuse.
Its list/simulation/unit fields use typed arrays with a 256 MiB numerical
workspace bound; worker chunks remain 50 runs. Historical scenarios retain the
original object-based generator and seeded traversal unchanged.
Minority declarations reach legal assembly without changing scrutiny rules.
Reference means remain stochastic mean-mode inputs; structural conditioning and
more-specific overrides can change aggregate references, with existing overlap
warnings retained. Missing local history remains a separate model limitation.

Existing draft keys and result formats remain readable. Import metadata selects
the workflow; cross-election imports navigate with an in-memory one-shot handoff.
Changing law opens a separate draft. Reset requires confirmation and preserves
calculated results with their executed scenario. Changed drafts are marked stale.
Navigation terminates legacy workers and disposes reform-owned services.

Previous-law scrutiny exceptions retain their R behavior. Execution errors now
identify the failing simulation/chamber and real elapsed time. The pre-preparation-migration
default's 1,000-run benchmark exposed the Camera flipper dead end at simulation 584; this
is an open legal-review blocker in [the plan](../MIGRATION_PLAN.md#plan), not an
accepted 1,000-run campaign or a corrected scrutiny rule.
Its frozen input remains a regression fixture; the revised dataset need not
reproduce that same failing seed/simulation.

## Regeneration and checks

Use the [complete Node preparation pipeline](data-preparation.md). From `web/`:

```powershell
npm run data:prepare -- --download
npm run data:check
npm run check:data
npm run check
npm test -- --maxWorkers=2
npm run build
npm run test:e2e -- --workers=2
```

The complete command invokes all snapshot/default/catalog compilers in dependency
order. `data:workspaces:generate` remains available for focused compiler changes.
Runtime descriptors record exact snapshot bytes, schema and
SHA-256. Missing, changed or incompatible production assets fail; there is no debug
fallback. `acceptance:reform` includes the complete `data:check` gate.

Repeat build/browser checks with `BASE_PATH=/elezioni`, always sequentially.
Direct loads, refreshes, navigation, workers and required data must work at both
bases. Development tests may clearly skip absent generated fixtures; release
acceptance requires all data and rejects skips. The R implementation and temporary
runtime bridges are retired; frozen JSON regressions remain test-only evidence.
Public deployment remains a separate migration step.
