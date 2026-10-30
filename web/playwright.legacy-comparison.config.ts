import { defineConfig, devices } from "@playwright/test";
export default defineConfig({
  testDir: "tests/e2e",
  testMatch: "legacy-comparison.spec.ts",
  workers: 1,
  retries: 0,
  timeout: 3660000,
  reporter: "line",
  projects: [{ name: "chromium", use: { ...devices["Desktop Chrome"] } }],
});
