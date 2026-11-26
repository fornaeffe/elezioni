# Shared light theme and UI styling (2026-11-26)

[Theme tokens](../../../web/src/lib/ui/token.css) now define the shared light
palette, typography, spacing, and shape. The root layout loads the tokens and
the [native element baseline](../../../web/src/lib/ui/base.css) and owns the
shared page frame. Both simulation workspaces use the same primary action and
focus styles. Party colors still come from scenario and result data. Canvas and
SVG axes use the shared chart-axis color.

The politics editor owns its responsive subject grid, field layout, and lazy
disclosures in the components that render them. Shared help fields use the
existing native-popover toggletip. The editor and diagnostics stylesheets were
retired after their rules moved to their owners. The unused reform editor
stylesheet was removed. Notices in both workspaces share presentation by
severity while callers choose whether to announce them. A stale politics
session is a warning and the recommendation after a successful run is
informational. Shared result components own disclosure and table scrolling.

The [scenario split button](../../../web/src/lib/ui/SplitButton.svelte) keeps
the native JSON.gz action and uses Bits UI for the alternate JSON download. The
menu closes on outside interaction, and its trigger and primary action are
disabled during execution. The enlarged spinogram uses a native modal dialog
with Escape, backdrop, and close-button dismissal and returns focus to its
trigger.

## Verification

- `npm run check` passed with zero errors and warnings. The static builds for
  the root and `/elezioni` succeeded.
- The focused Chromium suite passed 39 tests across the shared frame, editor,
  results, and regional workspace. The scenario download test passed again
  with keyboard Tab entry before opening the menu. Four focused Firefox tests
  passed for downloads, toggletips, chart enlargement, and the narrow page frame.
  The `/elezioni` smoke test passed for route refreshes, navigation, and a
  constructed worker run.
- Checked contrast for 11 theme color pairs. Sampled text pairs reached at
  least 5.80:1 and tested control borders reached at least 3.39:1. Reviewed
  captures of the mobile politics editor and menu and the desktop regional editor.
- The lockfile audit reported the same 11 advisories before and after adding
  Bits UI. Reports, logs, and screenshots are under
  `data/derived/ui-theming-2026-11-26/`.

The [candidate picker](../../../TODO.md) remains a separate UI task.
