# Authored politics default (CP-04)

Use this guide to change or package the installed politics default. For domain
fields use [the contract](contract.md); for generation and legal assembly use
[the execution guide](execution.md).

## Entry points

| Task | File |
| --- | --- |
| Edit scenario choices, palette, filings, history, references, model/law settings or run preferences | `data/politics-default-scenario.json` |
| Browser default and detached reset copies | `web/src/lib/politics/scenario/defaults.ts` |
| Read/validate the authored file; package statistical release inputs | `web/scripts/workspaces/politics-default.ts` |
| Prepare and verify exact source/geography/electorate/seat/model/catalogue bundles | `web/scripts/workspaces/politics-assets.ts` |
| Package M02 fitted state and its internal release scenario | `web/scripts/data-preparation/fast-share-release.ts` |
| Regenerate and validate snapshot descriptors | `web/scripts/workspaces/generate.ts` |
| Verify the approved values and projections | `web/scripts/workspaces/politics-default.test.ts`, `web/tests/e2e/politics-default.spec.ts` |

The authored JSON is a complete schema-1 common scenario, with no observation
tables or fitted arrays. It selects reform/M02/joint renewal and saves
Rosatellum/M01 separately. Each law has explicit filings for all 24 lists,
including statutory uni-only Rosatellum ballots. Colors are stored once per
subject; internal numerical release packaging retains its required projections.

The baseline retains separate Camera/Senato references and explicit SDs, including
zero, selected/excluded polls, reviewed M01 compositions and supplemental polls,
whole-component M02 pooling, and labelled fallback/electorate assumptions.
Source aliases are explicit membership declarations over unchanged original
observations. Reference-only support uses the common contract's owned `new:<list>`
components. M01's retained parameter date is `2026-10-16`, distinct from the shared
`2026-11-15` evidence cutoff. The M02 implicit multipliers and conditional forecast
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
also validates dependencies for all four supported pairs. Missing production
assets fail release; dependent development tests skip with a preparation message.

Do not edit generated defaults, descriptors or catalogues. The former authored
choice/policy JSON and retired compiler helpers are retained only as historical
comparison evidence in tests; production no longer reads them.

## Runtime and regional isolation

The common authored scenario is the browser default for all politics pairs.
The M00 compatibility default compiler and its generated application module are
removed. Their last captured scenario is isolated at
`web/tests/fixtures/retired-politics/default.ts`; it is test evidence, not an
input to preparation or runtime. The fitted M01/M02 release keeps its internal
statistical projection; it does not own a second user scenario or draft.

`data/workspace-defaults.json` still supplies the independent ER default and the
historical reference scaffold inside the static snapshot. Neither chooses the
installed politics default. The historical snapshot scenario remains test-only
at `web/tests/fixtures/legacy-snapshot-scenario.ts`. ER retains its own exporter
and unchanged default behavior.
