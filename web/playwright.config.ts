import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: 'tests/e2e',
  // Dedicated release/research checks have their own serving and budget configuration.
  testIgnore: ['base-smoke.spec.ts','hosting.spec.ts','fast-share-release.spec.ts','fast-share-fit.spec.ts','fast-share-ui.spec.ts','unified-share.spec.ts','unified-resources.spec.ts'],
  fullyParallel: true,
  workers: process.env.CI ? 1 : undefined,
  retries: 0,
  reporter: 'html',
  use: {
    baseURL: 'http://127.0.0.1:4173',
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure',
    actionTimeout: 30_000,
  },
  webServer: {
    // Current unified workspace; old scenario imports must be rejected.
    command: (process.env.ACCEPTANCE_PREBUILT ? '' : 'npm run build && ') +
      'node --import tsx tests/fixtures/serve-static-release.ts',
    env: { ACCEPTANCE_SERVER_PORT: '4173' },
    url: `http://127.0.0.1:4173${process.env.BASE_PATH ?? ''}/`,
    reuseExistingServer: !process.env.CI && !process.env.ACCEPTANCE_PREBUILT,
    timeout: 120_000
  },
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] }
    }
  ]
});
