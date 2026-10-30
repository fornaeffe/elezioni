import { test, expect } from "@playwright/test";
import { build } from "vite";
import { fileURLToPath } from "node:url";
import { mkdir, writeFile, readFile } from "node:fs/promises";
import { implementationKey } from "../../scripts/data-preparation/fast-share-release-io";
import { resolve } from "node:path";
let worker = "";
test.beforeAll(async () => {
  const result = await build({
    configFile: false,
    logLevel: "error",
    build: {
      write: false,
      minify: false,
      lib: {
        entry: fileURLToPath(
          new URL("../fixtures/filing-aware.worker.ts", import.meta.url),
        ),
        formats: ["es"],
        fileName: "filing-aware-worker",
      },
    },
  });
  for (const bundle of Array.isArray(result) ? result : [result])
    if ("output" in bundle)
      for (const chunk of bundle.output)
        if (chunk.type === "chunk" && chunk.isEntry) worker = chunk.code;
  expect(worker.length).toBeGreaterThan(0);
});
test(
  process.env.FILING_PRODUCTION
    ? "complete requested default refit and paired generation/legal browser budget"
    : process.env.FILING_REPLAY_SOURCE
      ? "complete historical forecast in browser"
      : "reduced browser fit/profile and feasible kernels before full computation",
  async ({ page, browser }) => {
    await page.route("**/filing-aware.worker.js", (route) =>
      route.fulfill({ contentType: "text/javascript", body: worker }),
    );
    await page.goto("/");
    let request: object = { action: "synthetic" };
    if (process.env.FILING_PRODUCTION) {
      request = {
        action: "production",
        model: process.env.FILING_MODEL ?? "bounded-pair-field-v1",
        scale: Number(process.env.FILING_SCALE ?? 1.5),
        draws: Number(process.env.FILING_DRAWS ?? 512),
        implementations: {
          generation: await implementationKey(
            resolve(".."),
            "web/src/lib/politics/reform-2026/vote-share/generation.ts",
          ),
          fitting: await implementationKey(
            resolve(".."),
            "web/src/lib/politics/reform-2026/vote-share/information.ts",
          ),
          profiles: await implementationKey(
            resolve(".."),
            "web/src/lib/politics/reform-2026/vote-share/senate-adapter.ts",
          ),
        },
      };
    }
    if (process.env.FILING_REPLAY_SOURCE) {
      const bytes = await readFile(process.env.FILING_REPLAY_SOURCE);
      await page.route("**/filing-source.json", (route) =>
        route.fulfill({ contentType: "application/json", body: bytes }),
      );
      request = {
        action: "replay",
        url: "/filing-source.json",
        seed: process.env.FILING_REPLAY_SEED ?? "filing-browser-full",
        model: process.env.FILING_MODEL ?? "bounded-pair-field-v1",
        scale: Number(process.env.FILING_SCALE ?? 1),
        draws: Number(process.env.FILING_DRAWS ?? 512),
      };
    }
    page.on("console", (message) => {
      if (message.text().startsWith("FILING_PROGRESS "))
        console.log(message.text());
    });
    const result = await page.evaluate(async (request) => {
      const w = new Worker("/filing-aware.worker.js", { type: "module" });
      try {
        return await new Promise<any>((resolve, reject) => {
          w.onmessage = (e) => {
            if (e.data.progress)
              console.log("FILING_PROGRESS " + JSON.stringify(e.data));
            else resolve(e.data);
          };
          w.onerror = (e) => reject(new Error(e.message));
          w.postMessage(request);
        });
      } finally {
        w.terminate();
      }
    }, request);
    await mkdir("../data/derived/filing-aware-research/browser", {
      recursive: true,
    });
    await writeFile(
      "../data/derived/filing-aware-research/browser/" +
        (process.env.FILING_PRODUCTION
          ? "production-" + (process.env.FILING_DRAWS ?? 512) + "-"
          : process.env.FILING_REPLAY_SOURCE
            ? "full-"
            : "reduced-") +
        (process.env.FILING_MODEL ?? "all") +
        ".json",
      JSON.stringify({ browser: browser.version(), request, result }, null, 2) +
        "\n",
    );
    expect(result.ok, result.error).toBe(true);
    expect(result.milliseconds).toBeLessThan(1800000);
    if (result.action === "production") {
      expect(result.completed).toBe(result.requested);
      expect(result.failed).toBe(0);
      expect(result.nonComputable).toBe(0);
      expect(
        result.ledger.every(
          (row: any) => row.seats === 588 && row.filled === 588,
        ),
      ).toBe(true);
      expect(result.lists).toBe(24);
    } else if (result.action !== "replay") {
      expect(result.valid).toBe(true);
      expect(result.politicalDiagnostics.passed).toBe(true);
      expect(result.participationDiagnostics.passed).toBe(true);
    }
  },
);
