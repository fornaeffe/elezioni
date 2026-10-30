import { existsSync } from "node:fs";
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { resolve } from "node:path";
import { build } from "vite";
import { test, expect, type Page } from "@playwright/test";
import {
  implementationKey,
  readBytes,
} from "../../scripts/data-preparation/fast-share-release-io";
import { runFitTask } from "../../src/lib/politics/reform-2026/vote-share/fit-service";
import { prepareFastGeneration } from "../../src/lib/politics/reform-2026/vote-share/generation";
import { byteHash } from "../../src/lib/politics/reform-2026/vote-share/source-codec";
import type { FitConfiguration } from "../../src/lib/politics/reform-2026/vote-share/fit-types";
import type { SourcePoll } from "../../src/lib/politics/reform-2026/vote-share/source-types";
const root = fileURLToPath(new URL("../../../", import.meta.url)),
  dir = resolve(root, "web/static/data/reform-2026/fast-share"),
  base = process.env.BASE_PATH ?? "";
test.skip(
  !existsSync(resolve(dir, "candidate.json")),
  "Prepare required candidate assets with data:reform:shares:release. This development skip is not release acceptance.",
);
test.setTimeout(120000);
const bundles = new Map<string, string>();
let configuration: FitConfiguration,
  custom: SourcePoll,
  expected: {
    fit: string;
    profile: string;
    input: string;
    defaultInput: string;
  },
  sourcePath = "",
  membershipIds: string[] = [];
test.beforeAll(async () => {
  const implementations = {
    generation: await implementationKey(
      root,
      "web/src/lib/politics/reform-2026/vote-share/generation.ts",
    ),
    fitting: await implementationKey(
      root,
      "web/src/lib/politics/reform-2026/vote-share/information.ts",
    ),
    profiles: await implementationKey(
      root,
      "web/src/lib/politics/reform-2026/vote-share/senate-adapter.ts",
    ),
  };
  configuration = {
    base,
    implementations,
    pointer: JSON.parse(await readFile(resolve(dir, "candidate.json"), "utf8")),
  };
  for (const [entry, name] of [
    ["fast-fit-harness.ts", "fv06-harness.js"],
    ["fast-fit-consumer.worker.ts", "fv06-consumer.js"],
  ]) {
    const result = await build({
      configFile: false,
      logLevel: "error",
      base: base + "/",
      worker: { format: "es" },
      build: {
        write: false,
        minify: false,
        lib: {
          entry: fileURLToPath(
            new URL("../fixtures/" + entry, import.meta.url),
          ),
          formats: ["es"],
          fileName: () => name,
        },
      },
    });
    for (const b of Array.isArray(result) ? result : [result])
      if ("output" in b)
        for (const out of b.output)
          bundles.set(
            base + "/" + out.fileName,
            out.type === "chunk"
              ? out.code
              : typeof out.source === "string"
                ? out.source
                : new TextDecoder().decode(out.source),
          );
  }
  const loaded = (
      await runFitTask({ action: "load", configuration }, readBytes(dir))
    ).accepted!,
    ids = loaded.assets.fit.componentIds,
    // Synthetic support follows the actual fitted basis, including expanded
    // future choices; no ten-coordinate assumption belongs to this worker test.
    weights = ids.map((_, i) => i + 1),
    total = weights.reduce((a, b) => a + b, 0),
    shares = weights.map((w) => w / total);
  custom = {
    id: "fv06-manual",
    surveyId: "fv06-survey",
    context: "camera",
    date: "2026-10-18",
    fieldworkStart: "2026-10-17",
    fieldworkEnd: "2026-10-18",
    availableDate: "2026-10-19",
    citation:
      "Explicit synthetic browser acceptance poll, not actual survey evidence",
    sampleSize: 2000,
    questionSampleSize: null,
    decidedFraction: 0.8,
    effectiveSample: 800,
    designEffect: 2,
    effectiveSamplePolicy: "fast-share-effective-sample-v1",
    errorGroup: "2026-H2-camera",
    roundingWidth: 0.001,
    otherDefinition: "offered-complement",
    status: "supported",
    reasons: [],
    method: "FV06 synthetic acceptance observation",
    original: ids.map((label, i) => ({
      label,
      percentage: shares[i] * 100,
      kind: "list",
    })),
    normalized: ids.map((id, i) => ({
      label: id,
      members: [id],
      share: shares[i],
      other: false,
    })),
  };
  sourcePath = loaded.sourceCatalog.assets.source.path;
  const edited = structuredClone(loaded.scenario);
  edited.polls.added = [custom];
  edited.polls.selected.push(custom.id);
  const node = (
    await runFitTask(
      { action: "fit", configuration, previous: loaded, scenario: edited },
      readBytes(dir),
    )
  ).accepted!;
  expected = {
    fit: node.assets.fittedStateId,
    profile: node.assets.profileStateId,
    input: await byteHash(
      new TextEncoder().encode(
        JSON.stringify(
          prepareFastGeneration(node.scenario, node.assets, node.context).run(1)
            .input,
        ),
      ),
    ),
    defaultInput: await byteHash(
      new TextEncoder().encode(
        JSON.stringify(
          prepareFastGeneration(
            loaded.scenario,
            loaded.assets,
            loaded.context,
          ).run(1).input,
        ),
      ),
    ),
  };
  const members = loaded.scenario.sourceMemberships;
  membershipIds = ["fdi", "lega"].map(
    (id) =>
      Object.keys(members).find(
        (k) =>
          k.startsWith("camera-2022:") &&
          members[k].members.length === 1 &&
          members[k].members[0] === id,
      ) ?? "",
  );
});
test.beforeEach(async ({ context, page }) => {
  await context.route("**/*", (r) => {
    const code = bundles.get(new URL(r.request().url()).pathname);
    return code === undefined
      ? r.continue()
      : r.fulfill({ contentType: "text/javascript", body: code });
  });
  await page.goto(base + "/");
  await page.evaluate(
    async ({ base, configuration }) => {
      const w = window as any,
        h = await import(base + "/fv06-harness.js");
      w.fitHarness = h;
      w.fitProgress = [];
      w.heartbeat = 0;
      w.timer = setInterval(() => w.heartbeat++, 20);
      w.client = new h.ReformFitClient(configuration, {
        onChange: (s: any) => {
          w.fitProgress.push(s);
          if (w.cancelSampling && s.progress?.phase === "sampling") {
            w.cancelSampling = false;
            w.cancelStarted = performance.now();
            w.client.cancel();
            w.cancelElapsed = performance.now() - w.cancelStarted;
          }
        },
      });
      w.defaultAudit = (await w.client.load()).audit;
    },
    { base, configuration },
  );
});
test.afterEach(async ({ page }) => {
  await page.evaluate(() => {
    const w = window as any;
    w.client?.dispose();
    clearInterval(w.timer);
  });
});
async function record(name: string, value: unknown) {
  await mkdir("../data/derived/fv06-acceptance", { recursive: true });
  await writeFile(
    "../data/derived/fv06-acceptance/browser-" +
      (base ? "project-base" : "root") +
      "-" +
      name +
      ".json",
    JSON.stringify(value, null, 2) + "\n",
  );
}

test("default startup performs zero fits and an added manual poll is fitted once with Node-identical output and preserved profiles", async ({
  page,
  browser,
}) => {
  const requested: string[] = [];
  page
    .context()
    .on("request", (r) => requested.push(new URL(r.url()).pathname));
  const result = await page.evaluate(
    async ({ custom, base }) => {
      const w = window as any,
        c = w.client,
        s = c.copyDraft();
      s.polls.added = [custom];
      s.polls.selected.push(custom.id);
      c.setDraft(s);
      const validation = await c.validate(),
        fit = await c.fit(),
        handoff = c.reserveGenerationCopies(1, {
          deviceMemory: 8,
          hardwareConcurrency: 4,
        }),
        lease = handoff.leases[0];
      try {
        return {
          defaultAudit: w.defaultAudit,
          validation: validation.plan,
          fit: fit.audit,
          plan: fit.plan,
          scenario: c.copyDraft(),
          heartbeat: w.heartbeat,
          numericFit: lease.packet.assets.fittedStateId,
          profile: lease.packet.assets.profileStateId,
          output: await w.fitHarness.runConsumer(
            base + "/fv06-consumer.js",
            lease.packet,
            lease.transfer,
            [1],
          ),
          canRun: c.canRun,
        };
      } finally {
        lease.release();
      }
    },
    { custom, base },
  );
  expect(result.defaultAudit).toMatchObject({
    nationalFits: 0,
    profileFits: 0,
    sourceLoaded: false,
  });
  expect(result.validation).toMatchObject({
    national: true,
    profiles: false,
    paired: false,
  });
  expect(result.fit).toMatchObject({
    nationalFits: 1,
    profileFits: 0,
    pairedFits: 0,
  });
  expect(result.numericFit).toBe(expected.fit);
  expect(result.profile).toBe(expected.profile);
  expect(result.output[0].input).toBe(expected.input);
  expect(result.heartbeat).toBeGreaterThan(0);
  expect(result.canRun).toBe(true);
  expect(requested.some((p) => p.endsWith(sourcePath))).toBe(true);
  await record("manual", {
    base,
    browser: browser.version(),
    ...result,
    scenario: undefined,
  });
});
test("reference date and historical correspondence edits invalidate both fits and profiles in the real worker", async ({
  page,
}) => {
  expect(membershipIds.every(Boolean)).toBe(true);
  const result = await page.evaluate(async (ids) => {
    const c = (window as any).client,
      s = c.copyDraft();
    s.referenceDate = "2026-10-20";
    c.setDraft(s);
    const date = await c.fit();
    c.restoreAccepted();
    const h = c.copyDraft(),
      [a, b] = ids;
    [h.sourceMemberships[a].members, h.sourceMemberships[b].members] = [
      h.sourceMemberships[b].members,
      h.sourceMemberships[a].members,
    ];
    c.setDraft(h);
    const membership = await c.fit();
    return { date: date.audit, membership: membership.audit, canRun: c.canRun };
  }, membershipIds);
  expect(result.date).toMatchObject({
    nationalFits: 1,
    profileFits: 1,
    pairedFits: 1,
  });
  expect(result.membership).toMatchObject({
    nationalFits: 1,
    profileFits: 1,
    pairedFits: 1,
  });
  expect(result.canRun).toBe(true);
  await record("evidence", result);
});
test("failure and cancellation retain the accepted revision, permit restore and retry, and report cancellation within one second", async ({
  context,
  page,
}) => {
  await context.route("**/" + sourcePath, (r) =>
    r.fulfill({ status: 404, body: "missing source" }),
  );
  const failed = await page.evaluate(async (custom) => {
    const c = (window as any).client,
      s = c.copyDraft();
    s.polls.added = [custom];
    s.polls.selected.push(custom.id);
    c.setDraft(s);
    try {
      await c.fit();
      return { error: "unexpected success" };
    } catch (e) {
      return { error: String(e), phase: c.state.phase, canRun: c.canRun };
    }
  }, custom);
  expect(failed.error).toContain("HTTP 404");
  expect(failed.canRun).toBe(false);
  await context.unroute("**/" + sourcePath);
  const result = await page.evaluate(async () => {
    const w = window as any,
      c = w.client,
      retryAudit = (await c.fit()).audit;
    c.restoreAccepted();
    const restored = c.canRun,
      s = c.copyDraft();
    s.fitSeed += "cancelled";
    c.setDraft(s);
    w.cancelSampling = true;
    let error = "";
    try {
      await c.fit();
    } catch (e) {
      error = (e as Error).name;
    }
    const stopped = c.state.phase;
    c.restoreAccepted();
    const original = c.copyDraft().fitSeed;
    const retry = c.copyDraft();
    retry.seed += "retry";
    c.setDraft(retry);
    const accepted = await c.validate();
    return {
      retryAudit,
      restored,
      error,
      stopped,
      cancelMilliseconds: w.cancelElapsed,
      fitSeed: original,
      ready: c.canRun,
      audit: accepted.audit,
    };
  });
  expect(result).toMatchObject({
    restored: true,
    error: "AbortError",
    stopped: "cancelled",
    ready: true,
  });
  expect(result.cancelMilliseconds).toBeLessThan(1000);
  expect(result.retryAudit).toMatchObject({
    nationalFits: 1,
    profileFits: 0,
    pairedFits: 0,
  });
  expect(result.audit).toMatchObject({ nationalFits: 0, sourceLoaded: false });
  await record("cancellation", result);
});
test("reserved consumers reproduce serial inputs and scrutiny while respecting the parallel memory limit", async ({
  page,
}) => {
  const result = await page.evaluate(async (base) => {
    const w = window as any,
      c = w.client,
      serial = c.reserveGenerationCopies(1, {
        deviceMemory: 8,
        hardwareConcurrency: 4,
      });
    let one;
    try {
      const lease = serial.leases[0];
      one = await w.fitHarness.runConsumer(
        base + "/fv06-consumer.js",
        lease.packet,
        lease.transfer,
        [1, 2],
      );
    } finally {
      serial.leases[0].release();
    }
    const hints = {
        deviceMemory: 8,
        hardwareConcurrency: 4,
      },
      capacity = c.generationCapacity(hints),
      limit = Math.min(2, capacity.maximum);
    if (limit < 1) throw new Error("No safe generation consumer");
    let rejected = "";
    if (limit < 2) {
      try {
        const unsafe = c.reserveGenerationCopies(2, hints);
        for (const lease of unsafe.leases) lease.release();
      } catch (error) {
        rejected = String(error);
      }
    }
    const many = [];
    for (let start = 1; start <= 2; start += limit) {
      const reservation = c.reserveGenerationCopies(
        Math.min(limit, 3 - start),
        hints,
      );
      try {
        many.push(
          ...(
            await Promise.all(
              reservation.leases.map((lease: any, i: number) =>
                w.fitHarness.runConsumer(
                  base + "/fv06-consumer.js",
                  lease.packet,
                  lease.transfer,
                  [start + i],
                ),
              ),
            )
          ).flat(),
        );
      } finally {
        for (const lease of reservation.leases) lease.release();
      }
    }
    return { serial: one, parallel: many, capacity, rejected, ready: c.canRun };
  }, base);
  expect(result.serial).toEqual(result.parallel);
  expect(result.serial[0].input).toBe(expected.defaultInput);
  expect(result.ready).toBe(true);
  if (result.capacity.maximum < 2)
    expect(result.rejected).toContain("GENERATION_AGGREGATE_MEMORY_LIMIT");
  await record("parallel", result);
});
