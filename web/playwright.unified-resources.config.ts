import { defineConfig, devices } from '@playwright/test';
import base from './playwright.unified-share.config';
export default defineConfig({
  ...base,
  testMatch: 'unified-resources.spec.ts',
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
});
