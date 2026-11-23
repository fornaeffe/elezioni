## Migration checklist review and cleanup — 2 November 2026

Reviewed the two open items at Luca's request before pruning completed tasks.
The **Fast vote-share model in production: 2026 reform only** parent is complete:
FV-01–FV-09 and its follow-up tasks are delivered, the runtime registry selects
M01/M02 2.0.0 with M02 as default, and the current release/archive verification
is recorded in the preceding restoration and FV-09 entries. Closed the parent
and removed it with the other completed checklist entries. This closes the
implementation scope; it does not establish predictive calibration or a fresh
1,000-draw resource campaign for 2.0.0. The earlier acceptance receipts retain
their original model/version scope. Public deployment remains the final gate.

**Local support for future filed lists without local history remains open.**
The [generation diagnosis](../../politics-reform-2026-generation.md#filed-lists-without-local-history),
current M01 reference preparation and restored 2.0.0 definitions still expose the
missing-support limitation: a national reference/numerical seed does not identify
a meaningful local profile. The superseded 1 November inference corrections do
not close this task after the restoration. Moved its complete directive to
**After completion of the web migration**, retaining Sud chiama Nord in
Molise/Basilicata, the estimate/reference decision, I17's limited legal scope
and the rejected sole-college bypass.

Removed **27 completed top-level entries**, including the newly closed fast-model
parent and all their completed children. Existing history already records the
foundations/parity/benchmarks, politics and Emilia-Romagna delivery, R26-01–21,
legal-gap investigations and approved resolutions, editor/results/messages,
poll preparation/calibration research, Other category, reference-based bonus
candidates, FV-01–09/model follow-ups and UI/table/spinogram refinements. Kept
all five remaining migration directives and every post-migration task. Replaced
the deferred poll-editor task's obsolete FV-07 checklist pointer with its
maintained workspace guide, and corrected the production plan's stale statement
that implementation acceptance was still open.

Verification: reviewed the current registry, restored model/reference code,
generation diagnosis, dated history and FV-09 verification receipt. Ran
`cd web; npm test -- src/lib/politics/reform-2026/vote-share/correspondence-policy-audit.test.ts src/lib/politics/reform-2026/vote-share/generation.test.ts`:
**2 files / 22 tests passed**, no skips. Checklist comparison confirms that all
previously open tasks survive unchanged except the completed fast-model parent,
with local support moved intact; no checked entries remain. Changed-document
local links and `git diff --check` pass. Only documentation changed; no new full
build, browser, predictive/resource or publication acceptance is claimed.
