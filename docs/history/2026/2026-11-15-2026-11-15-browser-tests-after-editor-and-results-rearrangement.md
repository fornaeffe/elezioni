## 2026-11-15: Browser tests after editor and results rearrangement

- Updated browser tests to open the current simulation settings, reference,
  result transfer, chart and message disclosures; use the current accessible
  control names; and inspect import errors in the shared application status.
  Retained assertions for scenario persistence, rejection without replacement,
  result content, chart selections and exports.
- The mobile status test formerly selected two spans when the working spinner
  was present. Its text assertion now targets the label. Inspection found a
  separate product issue: at widths up to 650px the rule hiding toggle spans
  also hides the working spinner, leaving the busy toggle visually empty.
  The issue remains open; no application behavior was changed in this work.
- Verification: the production build succeeded and the full default Chromium
  Playwright suite passed 70/70 at `/` with four workers and no retries. This
  does not constitute the separate release acceptance or base-path matrix.
