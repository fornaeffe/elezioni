import { defineConfig, devices } from '@playwright/test';

/** Explicit national resource campaign, excluded from routine functional tests. */
export default defineConfig({
  testDir: 'tests/benchmarks', testMatch: 'common-politics-resources.spec.ts',
  fullyParallel: false, workers: 1, retries: 0, reporter: 'list', timeout: 60 * 60 * 1000,
  use: { baseURL: 'http://127.0.0.1:4173', trace: 'off' },
  webServer: { command: 'npm run build && npm run preview -- --host 127.0.0.1', url: `http://127.0.0.1:4173${process.env.BASE_PATH ?? ''}/`,
    reuseExistingServer: false, timeout: 120_000 },
  projects: [{ name: 'chrome', use: { ...devices['Desktop Chrome'], channel: 'chrome' } }],
});
