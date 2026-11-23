## 2026-06-03 Checkpoint 26

Completed in the scrutiny-algorithm registry pass:

- Added optional `scrutinyAlgorithmId` to `SimulationRequest` and
  `SimulationBenchmark`.
- Added `web/src/lib/politics/scrutiny-algorithms.ts`, registering
  `politiche-r-parity-v1` as the default politics scrutiny algorithm.
- Rewired the generated politics worker to resolve the algorithm through the
  registry instead of importing `runPoliticsScrutiny()` directly.
- Recorded the selected algorithm in benchmark metadata and added a fallback
  warning for unknown requested algorithm IDs.
- Added registry tests proving the default algorithm is registered, unknown IDs
  fall back explicitly, and the default registry path calls the same
  implementation as the direct R-parity scrutiny function.
- Kept the UI selector deferred: there is still only one real politics
  algorithm, so exposing a visible choice would add noise without user value.

Verification:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 125 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
