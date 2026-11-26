# Candidate slot combobox

Each [candidate slot](../../../web/src/lib/politics/editor/CandidateSlots.svelte)
uses the shared [SearchCombobox](../../../web/src/lib/ui/SearchCombobox.svelte).
The wrapper owns search, keyboard navigation, and dropdown positioning. The
editor supplies eligibility reasons and assignment behavior. The field restores
the saved label when a search is dismissed, and **Genera automaticamente** clears
the assignment.

**Crea candidato** opens a native modal and checks eligibility for the originating
slot before saving the identity and assignment in one scenario edit. A rejected
edit displays its reason in the modal. Save and cancellation return focus to the
picker. Bonus pickers offer existing candidates because a bonus assignment
requires an eligible ordinary candidacy in the constituency.

`npm run check` reported zero errors and warnings. The editor-model and scenario
unit suites passed 48 tests. Four focused
[Chromium tests](../../../web/tests/e2e/common-politics-editor.spec.ts) covered
keyboard search and selection, disabled choices, retained assignments, automatic
generation, the bonus picker, creation and cancellation, focus return, and popup
placement at 375 px.
