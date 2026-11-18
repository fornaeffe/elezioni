import { defineConfig, devices } from '@playwright/test';
const publicUrl = process.env.HOSTING_URL;
const base = process.env.BASE_PATH ?? '/elezioni';
process.env.BASE_PATH = base;
const origin = publicUrl ? new URL(publicUrl).origin : 'http://127.0.0.1:4176';
export default defineConfig({
  testDir: 'tests/e2e', testMatch: 'hosting.spec.ts',
  outputDir: process.env.HOSTING_BROWSER_OUTPUT ?? '../data/derived/hosting/browser-output',
  workers: 1, fullyParallel: false, retries: 0, timeout: 10 * 60_000,
  reporter: 'line',
  use: { baseURL: origin, trace: 'retain-on-failure', serviceWorkers: 'block' },
  webServer: publicUrl ? undefined : {
    command: 'node --import tsx tests/fixtures/serve-static-release.ts',
    env: { BASE_PATH: base, ACCEPTANCE_SERVER_PORT: '4176', ACCEPTANCE_GZIP_ENCODING: '0' },
    url: origin + base + '/', reuseExistingServer: false,
  },
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
});
