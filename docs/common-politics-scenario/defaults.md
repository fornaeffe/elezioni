# Authored politics default (CP-04)

Use this guide to change or package the installed politics default. For domain
fields use [the contract](contract.md); for generation and legal assembly use
[the execution guide](execution.md).

## Entry points

| Task | File |
| --- | --- |
| Edit scenario choices, palette, filings, history, references, model/law settings or run preferences | `data/politics-default-scenario.json` |
| Browser default and detached reset copies | `web/src/lib/politics/scenario/defaults.ts` |
| Read/validate the authored file; deterministic transitional projections | `web/scripts/workspaces/politics-default.ts` |
| Prepare and verify exact source/geography/electorate/seat/model/catalogue bundles | `web/scripts/workspaces/politics-assets.ts` |
| Package M02 fitted state and the transitional reform scenario | `web/scripts/data-preparation/fast-share-release.ts` |
| Regenerate snapshot descriptors and the transitional previous-law scenario | `web/scripts/workspaces/generate.ts` |
| Verify the approved values and projections | `web/scripts/workspaces/politics-default.test.ts`, `web/tests/e2e/politics-default.spec.ts` |

The authored JSON is a complete schema-1 common scenario, with no observation
tables or fitted arrays. It selects reform/M02/joint renewal and saves
Rosatellum/M01 separately. Each law has explicit filings for all 24 lists,
including statutory uni-only Rosatellum ballots. Colors are stored once per
subject; packaging duplicates them only where a transitional format requires it.

The baseline retains separate Camera/Senato references and explicit SDs, including
zero, selected/excluded polls, reviewed M01 compositions and supplemental polls,
whole-component M02 pooling, and labelled fallback/electorate assumptions.
Source aliases are explicit membership declarations over unchanged original
observations. Reference-only support uses the common contract's owned `new:<list>`
components. M01's retained parameter date is `2026-10-16`, distinct from the shared
`2026-10-19` evidence cutoff. The M02 implicit multipliers and conditional forecast
reference are explicit. Actual worker counts remain execution provenance.

## Regeneration and dependency changes

Run the [complete production pipeline](../data-preparation.md) from `web/`:

```powershell
npm run data:prepare
npm run data:check
```

The pipeline prepares original data, packages the authored common dependencies,
regenerates workspace descriptors before calculating execution implementation
identities, prepares/verifies the fitted release, and checks every required asset.
Common decoded assets live under ignored `web/static/data/politics/`, with
content-addressed filenames and exact version/schema/byte-count/SHA-256 pins.
They are included in the production receipt and static build. Rosatellum uses
the definitive 2022 geography and its own census reconstruction, without the
reform Bolzano overlay; reconstruction conserves municipal legal population.

`INCOMPATIBLE_AUTHORED_PIN` means that the prepared dependency no longer matches
the reviewed authored pin. Review the source/model/geography change and the
descriptors returned by `buildPoliticsAssets`; update the authored pins only as
part of that reviewed change, then rerun the complete pipeline. Preparation never
rewrites pins or substitutes a latest bundle. Standalone descriptor generation
also validates dependencies for all three supported pairs. Missing production
assets fail release; dependent development tests skip with a preparation message.

Do not edit generated defaults, descriptors or catalogues. The former authored
choice/policy JSON and retired compiler helpers are retained only as historical
comparison evidence in tests; production no longer reads them.

## Transitional consumers and regional isolation

CP-04 supplies the common authored input and changes fresh browser startup to
reform/M02. The existing reform editor receives a deterministic release projection;
the previous-law editor receives a deterministic M00 compatibility projection
until CP-06/08 replace that editor/execution wiring. That old format cannot express
the full common contract: its local reference field uses Camera/common means;
its editable share previews project the authored M01 supplemental polls, and its
non-valid preview comes from the pinned paired Camera observation unless an
explicit authored national reference replaces it;
the common input retains both chambers' complete values. No old-format reader or
converter is introduced here. CP-05 owns new files/drafts; CP-06/07 own the common
editor and execution-session integration.

`data/workspace-defaults.json` still supplies the independent ER default and the
historical reference scaffold inside the old static snapshot. The older reform
model-preparation defaults likewise remain reference inputs for retained tests
and the retired editor. Neither chooses the installed common politics default.
The historical snapshot scenario is now test-only at
`web/tests/fixtures/legacy-snapshot-scenario.ts`. Its former production exporter
and generated module are removed. ER keeps its own exporter and unchanged
default behavior; no regional consumer depended on that removed exporter.
