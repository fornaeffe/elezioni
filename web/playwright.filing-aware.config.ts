import { defineConfig, devices } from '@playwright/test';
export default defineConfig({
  testDir: 'tests/e2e',
  testMatch: 'filing-aware.spec.ts',
  workers: 1,
  retries: 0,
  timeout: 1850000,
  reporter: 'line',
  use: { baseURL: 'http://127.0.0.1:4174', trace: 'retain-on-failure' },
  webServer: {
    command:
      (process.env.ACCEPTANCE_PREBUILT ? '' : 'npm run build && ') +
      'node --import tsx tests/fixtures/serve-static-release.ts',
    env: { REFORM_FAST_CANDIDATE: '1' },
    url: 'http://127.0.0.1:4174/',
    reuseExistingServer: false,
    timeout: 120000,
  },
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
});
