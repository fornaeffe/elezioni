## Common politics editor non-valid references E2E (2026-11-15)

Updated the editor E2E coverage for the non-valid reference controls' move into
scenario Advanced settings. The zero/default-uncertainty check now edits the
national Camera reference using its current label, and the responsive reference
test confirms that this control appears after opening Advanced settings and its
reference section.

Verification: `git diff --check` passed. The focused Playwright test was not
run.
