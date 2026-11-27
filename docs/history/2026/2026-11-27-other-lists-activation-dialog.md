# Other lists activation dialog

Replaced the inline setup form in the common-politics editor with the shared native `Dialog`. While the category is off, its card contains only the activation button. The dialog retains searchable selection of free components, requires at least one valid selection, and clears pending selection on cancel or close. Updated the existing editor browser scenarios for the dialog flow and marked the task complete in [TODO](../../TODO.md).

Verification: the official Svelte autofixer reported no issues for `SubjectsEditor.svelte`. Automated checks were not run in this turn.
