## 2026-11-18 — Focused acceptance profiles and Firefox startup deadline

- Luca supplied the hosted evidence for
  `069fe40e87165d7ba3a1938c9be2c5d2b7604fcb` and approved all proposed
  reductions after the timing review. Its cold preparation, data/type checks,
  1,380 units, 73 general checks, three editor checks and four preflight checks
  passed. Firefox Rosatellum/M02 then exceeded the remaining two-minute initial
  preparation wait. The trace shows covariance solving advancing from 18/180
  at 51 seconds to 150/180 at 120 seconds. The observed 63.5-minute failed cold
  run and earlier 129.8-minute local application campaign motivated the cuts;
  these are earlier measurements, not timings of this implementation.
- Real initial/refit readiness now has one five-minute cap with bounded detailed
  progress on failure. Production workflows allow ten minutes overall; the
  multi-refit residual M02 case retains fifteen. Controlled UI readiness is
  bounded at 20 seconds. Zero retries, active-status readiness and failure traces
  remain. The retained Firefox production workflow runs early, once.
- Added explicit `focused-browser-v2` contracts: development requires 66 general
  cases plus one controlled editor cancellation (67); release requires 70 general,
  one editor cancellation, two representative production archive workflows and
  one root smoke (74). The optional extended profile requires all eight archive
  law/model/browser combinations plus general/editor checks at both bases (158).
  Every profile still requires production-data validation, both type checks and
  every unit. Wrong selected counts and partial/skipped/flaky reports fail.
- Removed the duplicate diagnostic preflight and default full root replay. Root
  release smoke uses the existing constructed scenario/real worker, nested
  compute, pinned test data, direct routes, refreshes and navigation. Extracted
  that unchanged constructed setup for reuse with the four existing law/model
  lifecycle cases. Staged cold acceptance still requires all six real production
  route/worker/export checks, including all four national law/model pairs.
- Pure editor/default/draft/legacy-import and synthetic presentation specs now
  install a test-only controlled worker preparation boundary. Actual pinned
  editor assets, import validation, forms, serialization, persistence and result
  queries remain real; election execution and checkpoint creation are explicitly
  rejected by the boundary. It cannot supply numeric production acceptance.
  Cancellation holds a controlled refit until cancel, then permits retry; real
  execution cancellation/prefix preservation remains in the constructed suite.
  Worker-count/warning UI checks moved to this boundary.
- Removed three redundant v1–v4 rejection browser cases, retaining one and
  enumerating retired versions in unit coverage. Removed the separate
  alignment/placeholder case and the production cosmetic/checkpoint editor
  case; one mobile/keyboard smoke, constructed checkpoint/lifecycle checks and
  file/execution units remain. Legal outputs, frozen regression manifests,
  conservation, deterministic seeds, explicit zero and failed/partial membership
  expectations are unchanged.
- Pushes now prepare required assets and run `acceptance:dev` with Chromium.
  Hosted cold/warm reassessment is an explicit manual workflow (`publish=false`,
  normally `extended=false`). Cold runs release application/staged acceptance;
  warm runs preparation, data validation and project build, requiring matching
  artifact membership/bytes/SHA-256 and exact staged payload identity. Warm names
  its reused cold acceptance honestly and requires the same revision/policy.
  Publication still requires explicit dispatch, `main`, the publication variable
  and successful cold/warm reassessment. No new repository variables are needed.
- Added compact policy tests for profile/count enforcement and warm artifact
  equality. Updated active plan and operational guides around the new contracts.
  Static TypeScript parsing, package/workflow parsing, source enumeration
  (70 general / 4 production-tagged / 66 development) and `git diff --check`
  succeeded. Static evidence: `data/derived/acceptance-rationalization/static-review.json`.
  No type checks, builds, preparation, unit/browser tests or acceptance campaigns
  were executed: Luca explicitly retained execution. Runtime/type verification
  and post-change timings remain outstanding. Start with the commands in the
  common-politics acceptance guide, then manually dispatch the new committed
  revision's hosted release gate. No commit, push, merge or publication occurred.
