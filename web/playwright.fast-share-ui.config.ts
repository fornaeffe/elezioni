import { defineConfig, devices } from "@playwright/test";
export default defineConfig({
  testDir: "tests/e2e",
  testMatch: "fast-share-ui.spec.ts",
  workers: 1,
  reporter: [
    ["line"],
    [
      "json",
      {
        outputFile:
          (process.env.FILING_RESEARCH_COMPARISON === "1"
            ? "../data/derived/filing-aware-research/browser/ui-"
            : "../data/derived/fv08-acceptance/browser/ui-") +
          (process.env.BASE_PATH ? "nested" : "root") +
          ".json",
      },
    ],
  ],
  timeout: 120000,
  use: {
    baseURL: "http://127.0.0.1:4174",
    trace: "retain-on-failure",
    actionTimeout: 15000,
  },
  webServer: {
    command:
      (process.env.ACCEPTANCE_PREBUILT ? "" : "npm run build && ") +
      "node --import tsx tests/fixtures/serve-static-release.ts",
    env: { REFORM_FAST_CANDIDATE: "1" },
    url: "http://127.0.0.1:4174" + (process.env.BASE_PATH ?? "") + "/",
    reuseExistingServer: process.env.ACCEPTANCE_REUSE_SERVER === "1",
    timeout: 120000,
  },
  projects: [
    { name: "chromium", use: { ...devices["Desktop Chrome"] } },
    { name: "firefox", use: { ...devices["Desktop Firefox"] } },
    { name: "webkit", use: { ...devices["Desktop Safari"] } },
  ],
});
