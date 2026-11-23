## 2026-10-22 — Row-local presence and editor-local chamber controls

Replaced the global common/Camera/Senato switch following Luca's revised UI
request. Coalition and list rows now have Settings2 icon buttons with titles,
expanded-state semantics, hidden details and badges. Coalition details declare
chamber presence; list details combine presence and protected-minority controls.
Name/membership edits affect existing occurrences without resurrecting absent
lists. Removing presence previews dependent removals; restored lists remain
unfiled. The last list occurrence is removed through the trash action.

Coalition declarations persist in optional v2 presentation `chambers`; old files
infer them from member lists, preserving archive/default compatibility. Populated
coalitions present in both chambers acquire reciprocal automatic links; empty
coalition declarations remain editable until membership makes a legal link
possible. Disabling coalition presence unassigns local members. Re-enabling it
restores matching member identities already present in that chamber. Automatic
same-coalition links are identified in the link editor instead of offering an
inconsistent manual unlink action.

References have their own common/Camera/Senato selector; historical mappings have
an independent selector. Coverage is always chamber-specific, with an optional
chamber filter alongside region and college kind, row checkboxes and filtered
bulk actions. Candidate placement already has its own chamber selector. Shared
row sizing and trash controls remain in place.

Added unit regressions for presence restoration, non-resurrection on name edits,
empty declarations, automatic links and serialization, plus browser coverage for
row toggles/badges, protection and independent reference/history/coverage scopes.
Updated the editor guide/application/plan for the revised workflow. A Windows
text-write encoding issue was caught by the production build and corrected to
UTF-8; line endings were normalized after diff checking. Final verification is
recorded below.

Final verification: `npm run acceptance:reform` passed with 911 unit tests and
26 browser tests at each of `/` and `/elezioni`, zero skips/retries; production
checksum/vertical-slice and preparation/application type checks passed (zero
Svelte errors/warnings). Static builds passed and the root build was restored.
`git diff --check` passed. No commit or deployment was performed.
