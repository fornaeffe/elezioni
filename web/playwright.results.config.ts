import { defineConfig } from '@playwright/test';
import base from './playwright.config';

export default defineConfig({ ...base, testDir: 'tests/acceptance', workers: 1, fullyParallel: false,
  timeout: 45 * 60 * 1000, reporter: 'line', retries: 0,
  use: { ...base.use, trace: 'off' },
  webServer: { ...base.webServer, reuseExistingServer: false }
});
