## 2026-10-28 — FV-01 production contracts and preserved research evidence

Completed FV-01 only. Extracted the approved numerical model's 768-line
national/forecast/municipal definition unchanged into
[the maintained model contract](../../vote-generation-fast-model.md). Separate
[scenario/data](../../vote-generation-fast-contracts.md),
[reference/chamber](../../vote-generation-fast-references.md) and
[validation](../../vote-generation-fast-validation.md) contracts keep numerical,
preparation, editor and acceptance tasks independently readable. Updated the
main FV-01 checklist, production/archive reading maps, research status notice and
data entry point; FV-02 through FV-09 remain open.

The prototype exposed a real reference-policy difference: changing one political
reference can change non-valid support through joint elector-weight normalization.
Luca explicitly selected the installed workflow's separate turnout rule:
**list-reference edits preserve turnout; changing turnout is a separate input**.
Recorded `fixed-turnout-reference-v1`, with worked elector/valid-share examples
and a distinct availability-removal operation that still renormalizes non-valid
support as approved. This decision precedes runtime implementation.

Reserved scenario v4 and separate statistical, numerical, representation,
reference, chamber, RNG, fitted-state and asset identities without changing the
installed parser/constants/catalog. Defined original grouped compact observations,
daily availability/cutoff/source-range semantics, strict membership/Other/new-unit
validation, semantic versus provenance identities and fit/profile/composition/run
invalidation. Specified a calibrated joint logistic-normal reference family for
multiple uncertain shares, exact constraints, common/chamber precedence and local
override examples; its numerical calibration/acceptance remains FV-04. The C/E
fit is not a Senate fit. Paired adaptation preserves marginal variance budgets,
with the existing dependence profiles explicitly treated as assumptions.

Froze independent matrix/reference fixtures, seed/draw budgets, diagnostics,
numerical/Monte Carlo tolerances, predictive non-deterioration comparisons,
resource targets and input-envelope probes. Distinguished the research report's
observed 0.151/0.356 pp errors from the original harness's 0.5/0.8 pp absolute
ceilings. Neither convergence nor the small empirical replay establishes
universal calibration; unsupported moments and material regressions must be
reported instead of silently altering inputs or tolerances.

Created the [research evidence index](../../../research/vote-generation/README.md),
exact hash/disposition/consumer inventory and read-only default-domain snapshot.
The capture records base HEAD `3e062924c8adcc6c8c0a21a3f6108e66724959a7` plus exact
working-tree hashes/modified source paths, not a new commit. Preserved **61**
evidence records, **8,307,728 bytes** (7.92 MiB), including failed/corrected runs,
selection/source pins and older poll/endpoint/drift/Stan diagnostics. Large
municipal score rows are explicit JSON omissions with original byte/hash pins;
retained aggregates/diagnostics are checked against the originals.

The inventory covers **1,433** files, including mixed poll outputs and
research-owned Python bytecode. Mixed active poll artifacts remain keep/review
items and every record has `deletionAuthorized: false`. The domain snapshot
covers all **7,904** installed-target municipalities, **86** physical colleges
and the separate derived Camera TAA view. It inventories **47** proposed component
identities; provisional memberships and opaque special groups still require
FV-03/04 review/admissibility, not automatic acceptance as new defaults.

Verification passed: Node syntax check; `freeze.mjs --check`; `--check-sources`
rehashing all 1,433 originals and checking compact extracts; unchanged capture
replay; arithmetic/grouped/Other/turnout/common/local examples and four independent
reduced matrix targets. An isolated copy with no original ignored data passed;
corrupted retained evidence was rejected and the restored copy passed. Markdown
local links/anchors, formula extraction and diff whitespace were checked.
Pinned frozen JSON/JSONL bytes with local Git attributes and verified a temporary
Git-index checkout using `core.autocrlf=true`; snapshot hashes still pass without
original ignored data. Documentation checks covered 11 Markdown files, 161 local
links/anchors and 253 unchanged research formula spans. No files were staged in
the project repository by that isolated checkout test.
No production engine, parser, scenario/default, bundle, source observation or
legal behavior changed. No fit, forecast/backtest, app suite/build or browser
acceptance ran. Research originals/code were neither moved nor deleted, and no
commit or publication was made.
