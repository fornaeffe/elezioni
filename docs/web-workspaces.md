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
| Common politics pairs and resource bounds | `src/lib/politics/scenario/registry.ts`, `src/lib/politics/execution/resources.ts` |
| Election routing and regional workspace contract | `src/lib/workspaces/registry.ts` |
| Common-politics domain and execution | `src/lib/politics/scenario/index.ts`, `src/lib/politics/execution/index.ts`; [contract](common-politics-scenario/contract.md), [execution](common-politics-scenario/execution.md) |
| Common politics editor, lifecycle and durable sessions | `src/lib/politics/CommonPoliticsWorkspace.svelte`, `workspace-controller.ts`; [editor](common-politics-scenario/editor.md), [lifecycle](common-politics-scenario/lifecycle.md) |
| Route composition and law selection | `src/routes/politics/+page.svelte`, `src/routes/emilia-romagna/+page.svelte` |
| ER editor | `src/lib/workspaces/RegionalErWorkspace.svelte` |
| Law adapters and statistical preparation | `src/lib/politics/execution/prepare.ts`, `rosatellum.ts`, `reform.ts` |
| Shared navigation, controls, messages and tables | `src/lib/workspaces/WorkspaceShell.svelte`, `RunControls.svelte`, `WorkspaceMessages.svelte`, `ResultTable.svelte` |
| Regional worker lifecycle and draft persistence | `src/lib/workspaces/controller.svelte.ts`, `draft.svelte.ts` |
| Metadata routing and one-shot cross-route imports | `src/lib/workspaces/import-routing.ts`, `navigation.ts`, `handoff.ts` |
| Snapshot resolution and schema/checksum checks | `src/lib/workspaces/data.ts`, `snapshots.generated.ts` |
| Memory/stored result pages | `src/lib/workspaces/result-access.ts`; exports remain in `src/lib/core/result-export.ts` |
| Authored defaults, exact dependency packaging and descriptors | [CP-04 defaults guide](common-politics-scenario/defaults.md) |
| Standalone list tickets at the previous-law boundary | `src/lib/scenario/politics-tickets.ts` |

The installed workflows are previous-law politics, reform politics with M01/M02,
and Emilia-Romagna regional elections. Municipal simulation remains unavailable;
its type does not register a runnable workspace. Fresh politics startup selects reform/M02 from the authored scenario.
Registry validation also runs at the worker boundary.

## Defaults and compatibility

[`data/politics-default-scenario.json`](../data/politics-default-scenario.json)
is the single installed politics default. It includes complete common declarations,
per-law filings, model settings, run preferences and exact dependencies. See the
[CP-04 defaults guide](common-politics-scenario/defaults.md) for editing,
regeneration and retained reference inputs. Do not edit generated defaults.

Both politics routes (`/politics` and `/politics/editor`) use one common editor,
draft/controller and durable execution service for Rosatellum/M01,
reform/M01 and reform/M02. Selection is saved per law. Preparation loads/reuses
automatically, running locks edits, and both seeds change only through ordinary
field edits or their individual visible buttons. See the
[lifecycle guide](common-politics-scenario/lifecycle.md) for resource estimates,
compact session bounds, cancellation, quota recovery and exact checkpoint reuse.

Politics files use the [strict common formats](common-politics-scenario/files.md).
Old politics files are rejected and former draft keys are ignored and untouched.
Browsing/importing results never replaces the draft; only explicit scenario
loading does. Reset requires confirmation and preserves results with their
executed identity. Changed drafts are marked stale. Regional formats, persistence
and one-shot navigation handoffs remain independent.

Superseded projected politics workspaces, M00 control paths and old internal
schema helpers are not mounted by the politics routes. They remain for CP-08's
separate retirement and isolated legal/regression checks. Historical schema-9
filing conversion and old 50-run worker batching are not the common runtime
contract. Use the common domain/execution guides for current filings and
bounded pull-driven worker transport.

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
