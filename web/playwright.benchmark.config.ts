import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: 'tests/benchmarks',
  fullyParallel: false,
  retries: 0,
  reporter: 'list',
  timeout: 180_000,
  use: {
    baseURL: 'http://127.0.0.1:4173',
    trace: 'off'
  },
  webServer: {
    command: 'npm run build && npm run preview -- --host 127.0.0.1',
    url: 'http://127.0.0.1:4173',
    reuseExistingServer: false,
    timeout: 120_000
  },
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] }
    }
  ]
});
