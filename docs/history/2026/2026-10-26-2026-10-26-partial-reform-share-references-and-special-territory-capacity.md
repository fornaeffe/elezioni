## 2026-10-26 — Partial reform share references and special-territory capacity

Fixed the approved reform editor reference-share bug in
`paired-vote-generation.ts`. The former remainder calculation promoted every
unspecified list to a hard national target. Entering a common FdI anchor of
26.7% in the bundled scenario implicitly demanded about 54,743 Camera votes
from Valle d'Aosta ballot groups, despite their fixed 53,746 valid votes and
exclusive territorial support; 1,000 reconciliation passes could not satisfy
those contradictory targets.

Reconciliation now constrains only entered shares and chamber shares derived
from common anchors. Unspecified lists keep their local relative proportions
while their aggregate shares adjust to geographic capacity. Preserved local
turnout, filing zeros, explicit zero references, scope/chamber precedence and
fully supplied normalization. Exactly exhausted supplied mass removes the free
remainder directly; a reference exactly at its filed capacity removes competing
local mass. These feasible boundary references do not converge only
asymptotically. References exceeding their filed valid-vote capacity fail with
the requested and maximum possible shares; other infeasible combinations still
fail rather than receiving votes outside their filings. Poll-reference setup
continues to supply its existing complete target map.

Added nine constructed regressions for disconnected ballots, common and
chamber-specific 26.7% inputs, higher/lower anchors, turnout and structural zeros,
infeasible references, geographic capacity boundaries and 0%/100% endpoints.
Three required-bundle regressions also prepare each production FdI 26.7% editor
variant and generate a validated
simulation input. These tests clearly skip if the production bundle is absent;
the release acceptance gate continues to require it. Updated the editor and
generation guides and marked the corrective task complete in the active plan.

Verification: all 1,037 tests in 78 files passed with no skips; `npm run check`
reported zero errors/warnings; the static production build and `git diff --check`
passed. The build reported plugin timing and chunk-size warnings. No browser
acceptance was run for this TypeScript reconciliation change. No source data,
generated defaults, legal scrutiny rules or saved archives were rewritten.
Re-running scenarios with partial references uses the corrected reconciliation;
existing exported results retain their stored outcomes. No commit or publication
was performed.
