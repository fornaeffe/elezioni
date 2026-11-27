# Shared politics editor dialog

[Dialog.svelte](../../../web/src/lib/ui/Dialog.svelte) provides the native modal,
accessible heading and optional description, backdrop, viewport bounds, and
optional actions row. List and coalition removal, *Altre liste* deactivation,
and filing confirmations use the default width. Candidate creation uses a wider
panel and retains its form controls for reuse in the inline editor.

Callers handle confirmation, validation, and focus restoration. Confirmed
deletion can move focus to a surviving control. Candidate creation and slot
assignment still use one edit, with validation feedback inside the open form.
The [task list](../../../TODO.md) points the planned activation and pool forms
to the shared component.

`npm run check` reported zero errors and warnings. Eight focused Chromium
[editor tests](../../../web/tests/e2e/common-politics-editor.spec.ts) passed,
covering keyboard and button cancellation, focus restoration, filing changes,
removal, and candidate validation and creation. The candidate dialog fit a
375 × 320 viewport without horizontal overflow. Three filing assertions were
updated to match the existing accessible descriptions.
