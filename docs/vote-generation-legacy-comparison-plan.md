# Installed legacy comparison: execution guide

Use [the results and limitations](vote-generation-legacy-comparison-results.md)
for findings. This guide concerns the actual installed R-derived generator,
not the earlier research comparator named `legacy-global-beta-v1`.

## Frozen scope

- Historical Camera 2013/2018/2022, source cutoffs 730/365/90/15 days,
  512 draws per case and variant, plus the full 2018/2022 short-horizon domains.
- Requested 24-list future ballot, exact Camera filings and whole corresponding
  Senate regions; 1,000 paired generation-and-scrutiny draws per variant.
- **No general poll refresh:** historical mappings plus explicit latest reviewed
  positive poll references for otherwise unsupported new lists.
- **Poll refresh:** the same mappings, latest reviewed publication for established
  lists, new-list references, and the production poll-reference measurement path.
- Preserve the legacy transition scales, seeded streams, count apportionment,
  reference clocks, filing, turnout and residual semantics. No fitted candidate
  parameters or municipal profiles enter legacy generation.

Reviewed inputs are in `data/legacy-comparison-policy.json` and the separately
reviewed `data/legacy-comparison-future-polls.json`. Keep source corrections and
technical replay fixes separate from predictive model tuning. Reference dates
are information cutoffs; unchanged effective inputs reuse draws and seeds.

Historical snapshots use original Camera list counts, the existing membership
policy, conditional target electorates/filing, and a labelled regional historical
fallback for missing/invalid municipality records. The selected model's compact
source geography supplies the same census crosswalk for scoring. The historical
filing containers support vote generation only, not historical legal scrutiny;
TAA is excluded from ordinary poll refresh, as in production.

## Reproduce

Run from `web/`, with the original pinned data and the generated production and
candidate bundles present. Missing bundles are failures. Historical profile
folders are the same frozen inputs used by the selected-candidate study; regenerate
missing folders with the existing party-drift export workflow described in
[the research guide](vote-generation-feasible-research.md).

```powershell
node --max-old-space-size=8192 --import tsx scripts/acceptance/legacy-comparison.ts reduced
node --max-old-space-size=8192 --import tsx scripts/acceptance/legacy-comparison.ts full
node --import tsx scripts/acceptance/legacy-comparison-future.ts
node --import tsx scripts/acceptance/legacy-comparison-diagnose.ts polls 23
node --import tsx scripts/acceptance/legacy-comparison-diagnose.ts polls 135
node --import tsx scripts/acceptance/taa-admission-replay.ts
npx playwright test --config playwright.legacy-comparison.config.ts
node --import tsx scripts/acceptance/legacy-comparison-report.ts
npm test -- scripts/acceptance/legacy-comparison-input.test.ts src/lib/politics/reform-2026/generation.test.ts src/lib/politics/reform-2026/generation-production.test.ts
npm run check:data
```

`LEGACY_DRAWS` and `LEGACY_BROWSER_DRAWS` select smaller pilots; curated evidence
requires 512 historical and 1,000 browser draws. `LEGACY_TARGET`,
`LEGACY_HORIZON`, `LEGACY_VARIANTS` and `LEGACY_REPORT` isolate diagnostic runs.
A default reduced invocation deduplicates effective inputs across its cutoffs.
Do not compare an eight-draw pilot with the frozen predictive results.

Generated inputs, detailed scores and raw browser ledgers live under
`data/derived/legacy-comparison/`. The report command retains compact reviewed
JSON and compressed group/college evidence in `test/fixtures/benchmarks/`.
Browser tests deliberately fail if a draw throws, scrutiny is non-computable,
seats remain unfilled, or a variant exceeds the current 60-minute budget. A
completed diagnostic run with a failed gate remains evidence; do not drop it or
change its seed. The frozen selected-candidate browser comparator has 512 draws
and its earlier 30-minute target, so its elapsed time is not an equal-work timing.

Public defaults, deployment and research archival are separate migration tasks.

`taa-admission-replay.ts` reads the 20 exception IDs from the original curated
benchmark and checks the frozen variant input hashes before generation. It writes
the separate `test/fixtures/benchmarks/taa_admission_senate_i14_replay.json` receipt, preserving
the original browser/diagnostic records. Every exception must return 588 filled
seats with unique identities and consistent TAA admission; draw 135 is a separate
control expected to complete with Senate I14-v2. The previous
`taa_admission_replay.json` retains the pre-extension control result. This targeted Node replay does
not replace full browser/resource acceptance.
