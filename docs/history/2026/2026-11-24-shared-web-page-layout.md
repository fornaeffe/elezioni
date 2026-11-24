# Shared web page layout (2026-11-24)

The landing page previously reserved a full viewport for its own content,
pushing the footer below the fold. The header, political editor and results,
and regional navigation and content also calculated their horizontal edges
independently.

The [root layout](../../../web/src/routes/+layout.svelte) now defines a shared
`90rem` content width cap and responsive gutters for the header, main content,
and footer interior. It provides the main landmark on each route and fills the
viewport when content fits. Tall content scrolls in normal document flow. The
landing selector centers vertically in its available space and has no empty
header above it.

[Playwright coverage](../../../web/tests/e2e/workflow-selector.spec.ts) checks
the footer, page edges, main landmark, and Home navigation at 390, 1024, and
1600 px. The short-viewport case checks that the footer can be reached by
scrolling. Existing political lifecycle and statistics tests now use the
reworded scenario notice and the rendered spinograms for their assertions.

Verification passed for 41 focused Chromium tests at `/`, seven at `/elezioni`
including one production Rosatellum M01 simulation, and a final five-test repeat
of the layout spec at `/`. `npm run check` reported zero errors and warnings.
Local reports and logs are in `data/derived/web-layout-2026-11-24/`.
