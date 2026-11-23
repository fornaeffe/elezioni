## Mobile application-status indicator (2026-11-15)

Restricted the mobile hiding rule to the status label so the working spinner
remains visible. The existing accessible name and reduced-motion behavior remain
in place. A Playwright regression holds editor data requests pending and checks
the working icon, label, reduced motion and desktop layout, as well as the mobile
ready and error icons.

Verification from `web/`: `npm run check` passed with no errors or warnings;
`npx playwright test tests/e2e/app-status.spec.ts --reporter=line` passed all
three Chromium tests. The build emitted a large-chunk warning.
