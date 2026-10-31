import { defineConfig, devices } from '@playwright/test';
const origin = 'http://127.0.0.1:' + (process.env.ACCEPTANCE_SERVER_PORT ?? '4174');
export default defineConfig({
  testDir: 'tests/e2e',
  testMatch: 'unified-share.spec.ts',
  outputDir: process.env.FV08_FULL === '1' ? '../data/derived/fv08-unified/full-test-artifacts' : 'test-results',
  workers: process.env.FV08_FULL === '1' ? Number(process.env.FV08_CAMPAIGN_WORKERS ?? 1) : 1,
  fullyParallel: process.env.FV08_FULL === '1' && Number(process.env.FV08_CAMPAIGN_WORKERS ?? 1) > 1,
  retries: 0,
  reporter: 'line',
  timeout: 70 * 60 * 1000,
  use: {
    baseURL: origin,
    trace: 'retain-on-failure',
    actionTimeout: 20000,
  },
  webServer: {
    command:
      (process.env.ACCEPTANCE_PREBUILT ? '' : 'npm run build && ') +
      'node --import tsx tests/fixtures/serve-static-release.ts',
    env: { REFORM_FAST_CANDIDATE: '1' },
    url: origin + (process.env.BASE_PATH ?? '') + '/',
    reuseExistingServer: process.env.ACCEPTANCE_REUSE_SERVER === '1',
    timeout: 120000,
  },
  projects:
    process.env.FV08_FULL === '1'
      ? [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }]
      : [
          { name: 'chromium', use: { ...devices['Desktop Chrome'] } },
          { name: 'firefox', use: { ...devices['Desktop Firefox'] } },
        ],
});
