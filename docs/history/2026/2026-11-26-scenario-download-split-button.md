# Scenario download split button

The [common national scenario editor](../../../web/src/lib/politics/editor/CommonPoliticsEditor.svelte)
downloads compressed JSON from the main button. The adjacent arrow reveals the
plain JSON option. The option closes after selection, when focus leaves the
control, or when Escape returns focus to the arrow.

The reusable component owns the layout, styling, disclosure and focus behavior.
Its callers supply the labels and actions. The editor supplies the two scenario
formats and handles serialization and file downloads. See [SplitButton.svelte](../../../web/src/lib/ui/SplitButton.svelte).

`npm run check` passed with zero errors and warnings. Five focused Chromium
tests passed. The [editor test](../../../web/tests/e2e/common-politics-editor.spec.ts)
verified keyboard navigation, dismissal and round trips in both formats. The
[lifecycle tests](../../../web/tests/e2e/common-politics-lifecycle.spec.ts)
verified that the default download contains the current draft with archived
results present, for both election laws and both models.
