import { test, expect } from "@playwright/test";
import { build } from "vite";
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { resolve } from "node:path";
import { createHash } from "node:crypto";
let worker = "";
test.beforeAll(async () => {
  const result = await build({
    configFile: false,
    logLevel: "error",
    build: {
      write: false,
      minify: false,
      lib: {
        entry: resolve("tests/fixtures/legacy-comparison.worker.ts"),
        formats: ["es"],
        fileName: "legacy-comparison",
      },
    },
  });
  for (const bundle of Array.isArray(result) ? result : [result])
    if ("output" in bundle)
      for (const chunk of bundle.output)
        if (chunk.type === "chunk" && chunk.isEntry) worker = chunk.code;
  expect(worker.length).toBeGreaterThan(0);
});
for (const variant of ["no-polls", "polls"])
  test(
    "requested 24-list production legacy " + variant,
    async ({ page, browser }) => {
      const directory = resolve("../data/derived/legacy-comparison"),
        body = await readFile(
          resolve(directory, "future-" + variant + ".json"),
        );
      await page.route("http://legacy.test/**", (route) => {
        const p = new URL(route.request().url()).pathname;
        return route.fulfill({
          contentType: p.endsWith(".js")
            ? "text/javascript"
            : p.endsWith(".json")
              ? "application/json"
              : "text/html",
          body: p.endsWith(".js")
            ? worker
            : p.endsWith(".json")
              ? body
              : "<!doctype html><title>Legacy comparison</title>",
        });
      });
      await page.goto("http://legacy.test/");
      page.on("console", (m) => {
        if (m.text().startsWith("LEGACY_PROGRESS "))
          console.log(variant + " " + m.text());
      });
      const draws = Number(process.env.LEGACY_BROWSER_DRAWS ?? 1000);
      const result = await page.evaluate(async (draws) => {
        const w = new Worker("/legacy.worker.js", { type: "module" });
        try {
          return await new Promise<any>((resolve, reject) => {
            w.onerror = (e) => reject(Error(e.message));
            w.onmessage = (e) => {
              if (e.data.progress)
                console.log("LEGACY_PROGRESS " + JSON.stringify(e.data));
              else resolve(e.data);
            };
            w.postMessage({ url: "/input.json", draws });
          });
        } finally {
          w.terminate();
        }
      }, draws);
      await mkdir(directory, { recursive: true });
      await writeFile(
        resolve(directory, "browser-" + variant + ".json"),
        JSON.stringify(
          {
            browser: browser.version(),
            variant,
            draws,
            inputSha256: createHash("sha256").update(body).digest("hex"),
            result,
          },
          null,
          2,
        ),
      );
      expect(result.ok, result.error).toBe(true);
      expect(result.milliseconds).toBeLessThan(3600000);
      expect(result.completed).toBe(draws);
      expect(result.lists).toBe(24);
      expect(
        result.ledger.every((r: any) => r.seats === 588 && r.filled === 588),
      ).toBe(true);
    },
  );
