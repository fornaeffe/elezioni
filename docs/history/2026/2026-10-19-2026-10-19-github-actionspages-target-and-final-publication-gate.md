## 2026-10-19: GitHub Actions/Pages target and final publication gate

Luca selected GitHub Actions plus GitHub Pages on GitHub Free as the desired
hosting target for the whole migration, with reassessment and conditional
publication as its last step, before post-migration additions. Luca retains the
merge into `main`; the current checkout is named `migrazione` (the migration
branch), and no branch rename or merge was performed.

### Decision and targeted documentation changes

- `MIGRATION_PLAN.md` now sets the hosting target and ends with a gate covering
  current free-tier eligibility/limits, complete cold/warm-cache Linux runner
  measurements, production transfer sizes, all migrated election kinds and
  project-base browser checks. Passing feasibility leads to publication from
  `main` after Luca's merge and deployed smoke tests. A failed assessment keeps
  the task open and triggers a review of free alternatives.
- `AGENTS.md` retains only stable hosting/base-path and release-data rules.
  `data/README.md` owns the shared non-interactive Node preparation contract:
  clean-clone regeneration, versioned sources/models/bundles/defaults, optional
  caches, checksum failures and production-only payloads. Its existing
  `--publish` command publishes a local source artifact, not a website.
- R26-18 keeps the composed legal core browser-worker compatible. R26-19
  delivers its required historical import/calibration through shared Node
  commands; R26-20 handles configurable project-base URLs, versioned payloads
  and explicit missing-data failures. R26-21 includes preparation typechecking
  and production-data/base-path acceptance. Its application acceptance remains
  distinct from final all-election hosting acceptance and publication.
- Existing model/legal decisions, R26 ordering and completion markers remain
  unchanged. The final gate contains no fixed provider quotas that could become
  stale; recheck the official [Pages limits](https://docs.github.com/en/pages/getting-started-with-github-pages/github-pages-limits),
  [Actions limits](https://docs.github.com/en/actions/reference/limits),
  [runner resources](https://docs.github.com/en/actions/reference/runners/github-hosted-runners)
  and [billing allowances](https://docs.github.com/en/billing/concepts/product-billing/github-actions)
  when executing it.

### Assessment evidence and verification scope

- The preceding hosting assessment verified a public repository and measured
  145,471,137 bytes of pinned source downloads, cached geography/census generation
  in 1.67 s, and a successful static build in 4.71 s (28 files / 116,030,390 bytes).
  The politics snapshot was 75,921,415 bytes, or 4,034,519 bytes with gzip;
  Emilia-Romagna was 5,102,281 bytes, or 325,628 bytes with gzip. These were local
  Windows measurements, excluding cold downloads, dependency installation,
  future import/calibration and actual deployed content encoding. They do not
  establish free-runner or publication acceptance.
- Documentation-only verification: `git diff --check` passed; all **100 local
  links/anchors** in the five edited documents resolved. A structural check
  confirmed the publication gate is the last migration task before the
  post-migration section. Application tests were not rerun for these prose edits.
  No application, workflow, source manifest, generated data or legal/model
  implementation changed; no R26 step or publication was completed.
