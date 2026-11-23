## Conditional college election probabilities (2026-11-15)

The reform election-position table now offers the existing all-simulations metric
and two subsets according to whether the ordinary slate's capolista appears in
any bonus slate of the same chamber. The classification uses candidate identity,
independent of bonus award and final election. The cumulative final-local-winner
event and default requested-run denominator are unchanged. Each conditional row
shows its selected denominator and the number of simulations without a position
observation; unknown outcomes within the group retain the existing lower-bound
convention. A zero denominator is unavailable, not zero probability.

The position observation table is now v2 and records the capolista condition.
No v1 archive compatibility was added, as requested; old position data, including
the last-position chart, requires a new run. Unit and browser coverage includes
same-chamber identity matching, missing observations and outcomes, empty groups,
the selector, and imported disk-backed data beyond 1,000 simulations.

Verification from `web/`: `npm run check` (zero errors/warnings),
`npm run check:data`, `npm test -- --maxWorkers=4 --reporter=dot` (120 files,
1,334 tests), and `npm run build` passed. The broad Chromium Playwright run with
four workers passed 70 of 71 tests; an unrelated editor import-status check timed
out while model preparation was active and passed alone. Focused position UI and
disk statistics tests passed alone, including the 1,201-run conditional scan.
The build emitted the existing large-chunk warning. The first focused browser
attempt also timed out during concurrent full unit testing and passed on rerun.
