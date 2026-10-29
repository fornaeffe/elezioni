import { defineConfig, devices } from '@playwright/test';
export default defineConfig({
  testDir: 'tests/e2e',
  testMatch: 'fast-share-ui.spec.ts',
  workers: 1,
  reporter: 'line',
  timeout: 120000,
  use: {
    baseURL: 'http://127.0.0.1:4174',
    trace: 'retain-on-failure',
    actionTimeout: 15000,
  },
  webServer: {
    command:
      (process.env.ACCEPTANCE_PREBUILT ? '' : 'npm run build && ') +
      'node --import tsx tests/fixtures/serve-static-release.ts',
    env: { REFORM_FAST_CANDIDATE: '1' },
    url: 'http://127.0.0.1:4174' + (process.env.BASE_PATH ?? '') + '/',
    reuseExistingServer: false,
    timeout: 120000,
  },
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
});
