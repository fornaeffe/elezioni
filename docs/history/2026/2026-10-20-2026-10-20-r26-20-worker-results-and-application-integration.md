## 2026-10-20: R26-20 worker, results and application integration

Implemented only R26-20. The common registry now exposes both algorithms to the
production worker and selector. Legacy politics remains the default; reform
scenarios have separate storage and are never inferred from an old saved file.
The [application guide](../../politics-reform-2026-application.md) is the focused entry
point for UI/worker/export work; legal and generation guides retain their scopes.

- Reform generation prepares references once and sends each globally numbered
  paired Camera/Senato simulation through the existing common scrutiny entry
  point, retaining 50-run chunk boundaries. Old-law generation keeps its random
  streams; both legacy chambers now share the same global envelope ID. Optional
  wrapper instrumentation preserves measured per-chamber diagnostic times
  without adding nondeterministic fields to replayable legal results.
- Added typed presentation tables for bonus eligibility/reasons, national and
  territorial entitlements, cap effects, special seats, candidate outcomes,
  entitlement/election territories, admission, draws, recovery and I11 exclusions.
  Conditions, model/profile/source/conversion provenance, geographic reference
  resolution, census/decree messages and applied interpretations remain
  inspectable. Repeated metadata/warnings are deduplicated with occurrence counts.
  Chart structures derive from the table contract and display political names.
- Complete, partial, domain-blocked and execution-failed runs remain in the
  requested denominator. N18 cap-capacity and ordinary no-progress failures
  produce explicit errors/stage rows; there is no reroll, legal rescaling or
  successful-run normalization. Candidate/profile conclusions are explicitly
  conditional; dependence-profile sensitivity is separate from calibrated
  uncertainty and does not add cross-law comparison UI.
- The new reform viewer renders one selected simulation/chamber plot, one
  selected detail table with at most 50 rows and at most ten warnings. Existing
  legacy/regional plot and table visibility is preserved. Reform has no arbitrary
  run cap. Legacy retains 1,000 because its simultaneous scatter plots create
  one SVG element per subject/run; exceeding it now fails instead of silently
  truncating. Regional aggregate charts have no equivalent structural cap.
  Exported/stored reform detail still grows with run count; R26-21 must measure
  large-run time and memory rather than infer performance acceptance here.
- Generic schema-v2 columnar exports preserve the executed scenario, worker
  result, algorithm/model/data identities and seed, including after draft edits.
  Reform imports check algorithm/seed/data agreement; old exports without an
  algorithm explicitly resolve to the legacy ID. Warning chamber/interpretation/
  seat/draw context survives roundtrip. Imports route to the appropriate law;
  full reform scenarios expose supplied input and explicit conversion provenance.
- `BASE_PATH` controls navigation, worker and data URLs. Framework base resolution
  stays in routes and crosses the worker request boundary: importing `$app/paths`
  inside the worker initially failed to bundle unsupported SvelteKit client
  dependencies. The shared runtime loader verifies pinned gzip/decoded hashes.
  Vite preview serves `.json.gz` as HTTP `Content-Encoding: gzip`, so Fetch has
  already decoded it; that path verifies the pinned decoded digest directly.
  Raw gzip assets verify both compressed and decoded digests. Missing production
  inputs fail; the old politics debug fallback was removed. Defaults/models
  follow the regenerated catalog rather than hard-coded immutable filenames.

Verification on Windows x64 / Node 24.20.0, from `web/`, with the existing required
production artifacts available (no source/data regeneration or publication):

- `npm run check:data` and `npm run check`: zero diagnostics.
- `npm run test`: **832 tests passed in 54 files**, no skips, **12.15 s**; subsequent
  focused presentation/export/registry checks also passed after final refinements.
- Root static builds passed; build and browser execution ran sequentially.
- Full Chromium regression: **13 passed**, no skips, **34.3 s**, covering old-law,
  regional, reform production UI/replay/imports, explicit missing input failure,
  and a constructed-bundle compilation of the actual production worker across
  simulation IDs 1/50/51. The constructed worker test needs no ignored artifacts.
- Project-base `/elezioni/` regression: **5 passed**, no skips; both politics laws,
  regional simulation, navigation and import/replay/export were exercised.
- Svelte MCP checked the modified components; the new component has no reported
  issues. Existing large pages retain advisory unkeyed-loop/style suggestions;
  no unrelated rendering refactor was performed. Visually inspected the new
  workspace screenshot; changed chart labels from internal IDs to political names.
- `git diff --check` passed. R26-20 is checked in both plans; R26-21 and the final
  whole-migration publication gate remain open. No commit or deployment performed.
