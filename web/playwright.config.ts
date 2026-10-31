import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: 'tests/e2e',
  // Dedicated release/research checks have their own serving and budget configuration.
  testIgnore: ['fast-share-release.spec.ts','fast-share-fit.spec.ts','fast-share-ui.spec.ts','legacy-comparison.spec.ts','unified-share.spec.ts','unified-resources.spec.ts'],
  fullyParallel: true,
  retries: process.env.CI ? 2 : 0,
  reporter: 'html',
  use: {
    baseURL: 'http://127.0.0.1:4173',
    trace: 'on-first-retry'
  },
  webServer: {
    // These retained editor/archive regressions exercise the original v1-v3
    // workflow. Unified ordinary defaults have their own required suites.
    env: { REFORM_FAST_CANDIDATE: '0' },
    command: process.env.ACCEPTANCE_PREBUILT ? 'npm run preview -- --host 127.0.0.1' : 'npm run build && npm run preview -- --host 127.0.0.1',
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
