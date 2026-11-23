## CP-04 — Replace authored-default construction (2026-11-08)

Added the complete schema-1 `data/politics-default-scenario.json` as the installed
politics choice set. It materializes D03/D04/D07: reform/M02/joint startup,
Rosatellum/M01 saved selection, the approved ID/name/dates/seeds, ten simulations,
explicit model-aware automatic workers, all list/coalition colors, and explicit
filings under both laws. Complete M01/M02 blocks retain reviewed compositions,
partial named polls, selected/excluded observations, pooling, separate chamber
references and explicit SDs (including zero), candidate policies, and empty manual
candidate/supplied-input declarations. M01 retains its distinct 16 October
parameter date. Common reference-only components use owned `new:<list>` identities;
reviewed aliases are explicit source memberships over unchanged original counts.
These identity/encoding changes do not revise a probability law or legal rule.

Added scenario-independent common source, catalogue, reference-geography,
conditional 18+ electorate, geography, seat and model assets with exact pins.
Rosatellum uses definitive 2022 boundaries and legal-census physical cells from
its existing population reconstruction, without the reform overlay. Validation
checks every baseline pair, statutory ballots, physical population conservation,
model/schema/version compatibility and checksums. Preparation refuses incompatible
authored pins and never replaces them with a latest bundle. The ten decoded
common assets are ignored regenerable outputs, included in the production receipt
and static site. Workspace descriptors and catalogues are generated before
release implementation hashes; final checks are read-only.

Replaced independent politics-default compilers with deterministic projections
from the authored contract. Release fitting uses its exact selection, pooling,
seeds and settings; browser startup and reset consume that release and its run
preferences. Transitional M00 share previews project the authored M01 named polls
and the pinned paired Camera observation; local preview fields retain their
Camera/common limitation, while both complete chamber inputs stay in the common
scenario. The existing common execution API remains available for all three
pairs. Actual common editor/session wiring stays in CP-06/07, and new file/draft
isolation stays in CP-05.

Removed the obsolete snapshot exporter/shared compiler/unified default augmenter.
Moved the historical snapshot scenario and retained compiler regression helpers
to test-only fixtures. Traced regional preparation: ER keeps its independent
exporter and unchanged default, and still needs the shared historical snapshot
preparation. `workspace-defaults.json` and the old reform preparation sidecars
remain historical/reference scaffolds, not installed common politics choices.
Old authored choice/policy JSON is now comparison evidence only. No frozen legal
fixture, checksum manifest, CP-01 baseline or legal expectation changed.

Browser validation exposed an extra common fitting-RNG field leaking into the
strict transitional reform schema. The packaging boundary now selects only that
format's supported fields and calls its parser before writing. The startup/reset
browser check loads the model before editing run counts, matching the current
editor's disabled-until-loaded controls. Existing previous-law browser tests now
select that law explicitly instead of relying on its former startup default.

Verification on Windows x64/Node 24.20.0, from `web/`:

- Complete cold-cache preparation with separate `--output` and `--cache` passed
  in 133.53 s; all 20 required artifacts were present. Final regenerated pipeline
  passed in 84.39 s; read-only `data:check` passed in 23.37 s with matching receipt
  SHA-256 `9c686c08fc244424a0d6fc00416676212cd7e8cf1edc3e7e346829680057e5d1`.
  The release check filled all 588 domestic seats without a new default fit.
- `check:data` passed; `check` reported zero errors/warnings. Svelte MCP found no
  component issues; suggestions concerned existing lifecycle/local-map code.
- Final `npm test -- --maxWorkers=2`: 109 files, **1,287 tests passed**, no skips,
  56.69 s. This includes the new authored/pin/projection tests and unchanged legal,
  checksum and CP-01 regression evidence.
- Static builds passed at `/` (5.14 s) and `/elezioni` (5.03 s), with the existing
  plugin-timing/chunk-size notices. Matching prebuilt Playwright runs passed
  **51/51 at `/`** (55.5 s) and **51/51 at `/elezioni`** (53.0 s), no retries/skips.
  The added browser case checks fresh law/model/name/seed/count/palette, loads
  the real prefit and verifies edited run-count/seed reset and reload.
- Documentation file links, frozen-evidence diff audit and `git diff --check`
  passed. Local acceptance logs are retained under
  `data/derived/cp04-acceptance-20261108/` rather than the working tree.

Marked CP-04 and the separately approved palette task complete. CP-05–08, the
parent merger, final resource/1,000-run acceptance and public deployment remain
open. No commit, publication or manual user acceptance is claimed.
