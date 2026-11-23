## Fixed application status box (2026-11-15)

Moved status presentation to a viewport-fixed, collapsible application box with ready, working and error indicators. Political and regional workspaces report their operations and errors independently, with applicable cancellation and retry actions; the collapsed mobile control uses only an icon. Removed duplicated simulation status and cancellation controls. The separate import/export performance and parallelization assessment remains open.

Verification: `npm run check` passed; a production build and targeted Chromium tests covered mobile fixed positioning, political and regional import errors, political lifecycle, cancellation, and draft isolation. The initial browser run reused a preview server that stopped during testing; rerunning against a fresh build passed the targeted checks.


### 2026-11-15 — Common politics editor subject rows

- Aligned coalition and list controls under shared headers, with responsive compact labels and row-local icon controls for advanced options. List national references remain in the basic row; advanced reference controls now live in the list options panel.
- Reference percentage fields use `% auto` as their empty placeholder across lists, components, abstention and territorial and chamber references. This is presentation only; explicit zero, inheritance and scenario data are unchanged.

### 2026-11-15 — Animate working status icon

- Added a continuous linear rotation to the application status box's working icon. The ready and error icons remain static; users who request reduced motion see a static working icon.
- Verification: `git diff --check` passed. No browser animation review was performed.

### 2026-11-15 — Move reference date into advanced scenario settings

- Moved the reference date input from the basic scenario row into the existing collapsed advanced scenario section. Scenario editing and validation behavior are unchanged.
- Updated the common politics editor E2E coverage to check that the control is hidden until advanced settings are opened, edits persist in the draft, and the selected date survives reload.
- Verification: not run in this change.

### 2026-11-15 — Collapsible advanced simulation settings

- Grouped fitting and simulation seeds, worker selection, candidate-generation parameters, and M02 fitting/checkpoint controls inside a collapsed advanced simulation panel. Kept the simulation count and run action in the primary section.
- Moved requested/recommended/maximum worker counts, per-worker memory estimate, and the conditional worker memory guidance into that panel. Replaced the warning text with the requested retry guidance.
- Verification: Svelte autofixer reported no issues. No test suite or browser review was run.
