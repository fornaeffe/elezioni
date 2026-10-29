import { defineConfig, devices } from '@playwright/test';
export default defineConfig({
  testDir: 'tests/e2e',
  testMatch: ['fast-share-release.spec.ts','fast-share-fit.spec.ts'],
  workers: 1,
  reporter: 'line',
  use: { baseURL: 'http://127.0.0.1:4174', trace: 'retain-on-failure' },
  webServer: {
    command:
      (process.env.ACCEPTANCE_PREBUILT ? '' : 'npm run build && ') +
      'node --import tsx tests/fixtures/serve-static-release.ts',
    url: 'http://127.0.0.1:4174' + (process.env.BASE_PATH ?? '') + '/',
    reuseExistingServer: false,
    timeout: 120_000,
  },
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
});
