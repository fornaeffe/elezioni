import { defineConfig, devices } from '@playwright/test';
const origin = 'http://127.0.0.1:' + (process.env.ACCEPTANCE_SERVER_PORT ?? '4174');
export default defineConfig({
  testDir: 'tests/e2e',
  testMatch: 'unified-share.spec.ts',
  outputDir: 'test-results',
  workers: 1,
  fullyParallel: false,
  retries: 0,
  reporter: 'line',
  timeout: 10 * 60 * 1000,
  use: {
    baseURL: origin,
    trace: 'retain-on-failure',
    actionTimeout: 20000,
  },
  webServer: {
    command:
      (process.env.ACCEPTANCE_PREBUILT ? '' : 'npm run build && ') +
      'node --import tsx tests/fixtures/serve-static-release.ts',
    url: origin + (process.env.BASE_PATH ?? '') + '/',
    reuseExistingServer: process.env.ACCEPTANCE_REUSE_SERVER === '1',
    timeout: 120000,
  },
  projects: [
    { name: 'firefox', use: { ...devices['Desktop Firefox'] },
      ...(process.env.ACCEPTANCE_FULL_MATRIX === '1' ? {} : { grep: /politiche-r-parity-v1\/M02 unified browser workflow/ }) },
    { name: 'chromium', use: { ...devices['Desktop Chrome'] },
      ...(process.env.ACCEPTANCE_FULL_MATRIX === '1' ? {} : { grep: /politiche-reform-2026-v1\/M02 unified browser workflow/ }) },
  ],
});
