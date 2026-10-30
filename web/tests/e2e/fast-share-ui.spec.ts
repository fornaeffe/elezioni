import { readFile, writeFile, mkdir } from "node:fs/promises";
import { resolve } from "node:path";
import { gunzipSync } from "fflate";
import { test, expect, type Page, type Download } from "@playwright/test";
import { decodeState } from "../../src/lib/politics/reform-2026/vote-share/state-codec";
import { readBytes } from "../../scripts/data-preparation/fast-share-release-io";
import { watchMemory } from "../fixtures/fast-share-memory";
import { parseSimulationResultExport } from "../../src/lib/core/result-export";
import { loadCandidateSource } from "../../src/lib/politics/reform-2026/vote-share/release";
import type {
  CandidatePointer,
  ReleaseCatalog,
  ScenarioState,
} from "../../src/lib/politics/reform-2026/vote-share/release";
const base = process.env.BASE_PATH ?? "",
  dir = resolve("static/data/reform-2026/fast-share");
test.afterEach(async ({ page }, info) => {
  if (info.status !== info.expectedStatus)
    console.error(
      "Workspace messages:",
      await page.locator('[role="alert"]').allTextContents(),
    );
});
let pointer: CandidatePointer,
  catalog: ReleaseCatalog,
  defaults: ScenarioState["scenario"];
let offeredRawIds: string[] = [];
let installedDefaults: unknown;
test.beforeAll(async () => {
  const installedCatalog = JSON.parse(
    await readFile(
      resolve(
        "src/lib/politics/reform-2026/data/generation-catalog.generated.json",
      ),
      "utf8",
    ),
  );
  installedDefaults = JSON.parse(
    await readFile(
      resolve(
        "src/lib/politics/reform-2026/data",
        installedCatalog.defaultsPath,
      ),
      "utf8",
    ),
  );
  // No accepted skip: this suite requires a completely prepared candidate build.
  pointer = JSON.parse(await readFile(resolve(dir, "candidate.json"), "utf8"));
  catalog = (
    await decodeState<ReleaseCatalog>(
      pointer.catalog,
      readBytes(dir),
      "fast-share-catalog",
    )
  ).state;
  defaults = (
    await decodeState<ScenarioState>(
      catalog.assets.scenario,
      readBytes(dir),
      "fast-share-scenario",
    )
  ).state.scenario;
  const source = await loadCandidateSource(catalog, readBytes(dir)),
    first = new Set(
      defaults.components
        .filter((c) => c.kind === "fitted")
        .slice(0, 5)
        .map((c) => c.id),
    );
  offeredRawIds = [
    ...new Set([
      ...source.payload.choices.flatMap((c) => c.members),
      ...source.payload.polls.flatMap((p) =>
        p.normalized.flatMap((a) => a.members),
      ),
    ]),
  ].filter((id) => first.has(defaults.componentMap[id] ?? id));
});
async function open(page: Page) {
  await page.goto(base + "/politics");
  await page
    .getByLabel("Algoritmo di scrutinio")
    .selectOption("politiche-reform-2026-v1");
  await expect(page.getByTestId("fast-reform-workspace")).toBeVisible();
  await expect(page.getByTestId("fast-scenario-editor")).toBeVisible();
}
async function load(page: Page) {
  await page
    .getByRole("button", { name: "Carica modello predefinito", exact: true })
    .click();
  await expect(page.getByTestId("fit-status")).toHaveText("Modello pronto", {
    timeout: 60000,
  });
}
async function bytes(d: Download) {
  const chunks: Buffer[] = [];
  for await (const chunk of (await d.createReadStream())!)
    chunks.push(Buffer.from(chunk));
  return Buffer.concat(chunks);
}
async function session(page: Page) {
  const promise = page.waitForEvent("download");
  await page
    .getByRole("button", { name: "Scarica scenario con stima", exact: true })
    .click();
  return JSON.parse((await bytes(await promise)).toString());
}
test("on-demand data, future-only edits, frozen run, exports and scenario replay", async ({
  page,
  browser,
}) => {
  const memory = await watchMemory(browser, page);
  const paths: string[] = [];
  page.on("request", (r) => paths.push(new URL(r.url()).pathname));
  const errors: string[] = [];
  page.on("pageerror", (e) => errors.push(e.message));
  await open(page);
  expect(paths.some((p) => p.endsWith(catalog.assets.fit.path))).toBe(false);
  await page
    .getByLabel("Seme delle simulazioni", { exact: true })
    .fill("fv07-before-load");
  await page.getByLabel("Seme delle simulazioni", { exact: true }).press("Tab");
  await load(page);
  expect(paths.some((p) => p.endsWith(catalog.assets.source.path))).toBe(false);
  await page
    .getByText("Quote di riferimento e incertezza", { exact: true })
    .click();
  await page
    .getByRole("combobox", { name: "Soggetto", exact: true })
    .selectOption("list:" + defaults.lists.camera[0].id);
  await page.getByLabel("Quota di riferimento %", { exact: true }).fill("15");
  await page
    .getByRole("button", { name: "Applica riferimento", exact: true })
    .click();
  await expect(page.getByTestId("fit-status")).toHaveText("Modello pronto");
  const saved = await session(page);
  expect(saved.scenario.seed).toBe("fv07-before-load");
  expect(saved.scenario.nonValidReferences).toEqual(
    defaults.nonValidReferences,
  );
  expect(
    saved.scenario.references.some(
      (r: { subject: { id: string }; mean: number }) =>
        r.subject.id === defaults.lists.camera[0].id && r.mean === 0.15,
    ),
  ).toBe(true);
  expect(paths.some((p) => p.endsWith(catalog.assets.source.path))).toBe(false);
  await page.getByLabel("Simulazioni", { exact: true }).fill("1");
  await page
    .getByRole("button", { name: "Esegui riforma", exact: true })
    .click();
  await expect(page.getByTestId("reform-phase")).not.toHaveText("Pronto");
  await page
    .getByLabel("Seme delle simulazioni", { exact: true })
    .fill("fv07-edited-during-run");
  await page.getByLabel("Seme delle simulazioni", { exact: true }).press("Tab");
  await expect(page.getByTestId("reform-phase")).toHaveText("completed", {
    timeout: 100000,
  });
  await expect(page.getByTestId("stored-result-status")).toContainText("1/1");
  await expect(
    page.getByText(
      "Scenario modificato. Risultati ed export conservano lo scenario e la stima eseguiti.",
      { exact: true },
    ),
  ).toBeVisible();
  const downloaded = page.waitForEvent("download");
  await page
    .getByRole("button", { name: "Scarica risultati JSON.gz", exact: true })
    .click();
  const archive = await bytes(await downloaded),
    payload = JSON.parse(new TextDecoder().decode(gunzipSync(archive)));
  // Validate the actual exported metadata before replay. A stale stored result
  // must not make a failed archive import appear to have succeeded.
  expect(
    parseSimulationResultExport(JSON.stringify(payload)).schema_version,
  ).toBe(3);
  expect(payload.schema_version).toBe(3);
  expect(payload.scenario.seed).toBe("fv07-before-load");
  expect(payload.fitFile.scenario).toEqual(payload.scenario);
  expect(payload.result.benchmark.dataVersion).toBe(
    defaults.data.sourceBundleId,
  );
  expect(
    payload.result.tables.some(
      (t: { name: string; rows: unknown[] }) =>
        t.name === "Seat entitlement and election locations" &&
        t.rows.length > 0,
    ),
  ).toBe(true);
  await page.getByTestId("reform-file-input").setInputFiles({
    name: "replay.json.gz",
    mimeType: "application/gzip",
    buffer: archive,
  });
  await expect(page.getByTestId("stored-result-status")).toContainText("1/1");
  await page
    .getByRole("button", {
      name: "Ripristina scenario dei risultati",
      exact: true,
    })
    .click();
  await expect(page.getByTestId("fit-status")).toHaveText("Modello pronto", {
    timeout: 60000,
  });
  await expect(
    page.getByLabel("Seme delle simulazioni", { exact: true }),
  ).toHaveValue("fv07-before-load");
  const file = await session(page);
  expect(file.scenario.data.fittedStateId).toBe(
    saved.scenario.data.fittedStateId,
  );
  const csvDownload = page.waitForEvent("download");
  await page
    .getByRole("button", { name: "Scarica risultati CSV", exact: true })
    .click();
  expect((await bytes(await csvDownload)).length).toBeGreaterThan(1000);
  expect(errors).toEqual([]);
  await expect(page.getByText(/RESULT_SERVICE_ERROR:/)).toHaveCount(0);
  const memoryReport = await memory.stop();
  const report = resolve(
    process.env.FILING_RESEARCH_COMPARISON === "1"
      ? "../data/derived/filing-aware-research/browser"
      : "../data/derived/fv08-acceptance/browser",
  );
  await mkdir(report, { recursive: true });
  await writeFile(
    resolve(
      report,
      browser.browserType().name() + (base ? "-nested.json" : "-root.json"),
    ),
    JSON.stringify(
      {
        base,
        browser: browser.browserType().name(),
        browserVersion: browser.version(),
        catalogId: pointer.catalogId,
        implementation: catalog.generationImplementationId,
        scenario: file.scenario.data,
        result: payload.result.benchmark,
        originalHistoryFetched: paths.some((p) =>
          catalog.sourceChunks.some((c) => p.endsWith(c.path)),
        ),
        pageErrors: errors,
        memory: memoryReport,
      },
      null,
      2,
    ),
  );
  // The authorized comparison freezes a 30-minute compute target. Preserve the
  // older memory target as measured evidence; ordinary FV-08 acceptance still
  // enforces it by default.
  if (
    "peakBytes" in memoryReport &&
    process.env.FILING_RESEARCH_COMPARISON !== "1"
  )
    expect(memoryReport.peakBytes).toBeLessThanOrEqual(512 * 1024 * 1024);
});
test("advanced poll editing triggers explicit refit, cancellation, retry and portable refit state", async ({
  page,
}) => {
  await open(page);
  await load(page);
  await page
    .getByText("Dati avanzati: sondaggi, corrispondenze e data", {
      exact: true,
    })
    .click();
  await expect(
    page.getByText("Caricamento catalogo osservazioni…", { exact: true }),
  ).not.toBeVisible({ timeout: 60000 });
  await page
    .getByText("Aggiungi / modifica sondaggio", { exact: true })
    .click();
  await page.getByLabel("ID (vuoto: nuovo)", { exact: true }).fill("fv07-poll");
  await page.getByLabel("ID indagine", { exact: true }).fill("fv07-survey");
  await page
    .getByLabel("Fonte / citazione", { exact: true })
    .fill("Sondaggio sintetico di accettazione, non dati reali");
  await page
    .getByLabel("Inizio rilevazione", { exact: true })
    .fill("2026-10-17");
  await page.getByLabel("Fine rilevazione", { exact: true }).fill("2026-10-18");
  await page.getByLabel("Pubblicazione", { exact: true }).fill("2026-10-19");
  await page
    .getByLabel("Data osservazione (vuoto: fine rilevazione)", { exact: true })
    .fill("2026-10-17");
  await page
    .getByLabel("Sottocampione della domanda (facoltativo)", { exact: true })
    .fill("800");
  await page
    .getByRole("button", { name: "Aggiungi risposta", exact: true })
    .click();
  let answer = page.getByRole("group", { name: "Risposta 1", exact: true });
  await answer.getByLabel("Etichetta", { exact: true }).fill("Insieme offerto");
  await answer.getByLabel("Quota %", { exact: true }).fill("60");
  await answer
    .getByRole("listbox", { name: "Componenti originali", exact: true })
    .selectOption(offeredRawIds);
  await page
    .getByRole("button", { name: "Aggiungi risposta", exact: true })
    .click();
  answer = page.getByRole("group", { name: "Risposta 2", exact: true });
  await answer.getByLabel("Etichetta", { exact: true }).fill("Altri");
  await answer.getByLabel("Quota %", { exact: true }).fill("40");
  await answer
    .getByLabel("Altri: complemento offerto", { exact: true })
    .check();
  await page
    .getByRole("button", { name: "Salva sondaggio", exact: true })
    .click();
  await expect(page.getByTestId("fit-status")).toContainText(
    "Stima da aggiornare",
  );
  await expect(
    page.getByRole("button", { name: "Esegui riforma", exact: true }),
  ).toBeDisabled();
  await page
    .getByRole("button", { name: "Ricalcola parametri", exact: true })
    .click();
  await expect(page.getByTestId("fit-status")).toContainText("Stima in corso");
  await page
    .getByRole("button", { name: "Annulla operazione modello", exact: true })
    .click();
  await expect(page.getByTestId("fit-status")).toHaveText(
    "Operazione annullata",
  );
  await page
    .getByRole("button", { name: "Ricalcola parametri", exact: true })
    .click();
  await expect(page.getByTestId("fit-status")).toHaveText("Modello pronto", {
    timeout: 100000,
  });
  const saved = await session(page);
  expect(saved.scenario.polls.selected).toContain("fv07-poll");
  expect(
    saved.scenario.polls.added.find(
      (p: { id: string }) => p.id === "fv07-poll",
    ),
  ).toMatchObject({
    date: "2026-10-17",
    questionSampleSize: 800,
    effectiveSample: 800,
  });
  expect(saved.scenario.data.fittedStateId).not.toBe(
    defaults.data.fittedStateId,
  );
  expect(saved.scenario.data.profileStateId).toBe(defaults.data.profileStateId);
  await page.getByTestId("reform-file-input").setInputFiles({
    name: "edited-session.json",
    mimeType: "application/json",
    buffer: Buffer.from(JSON.stringify(saved)),
  });
  await expect(page.getByTestId("fit-status")).toHaveText("Modello pronto", {
    timeout: 60000,
  });
});
test("metadata retry, mobile keyboard forms and reset preserve result-independent drafts", async ({
  page,
}) => {
  await page.setViewportSize({ width: 390, height: 844 });
  let fail = true;
  await page.route("**/fast-share/candidate.json", (route) =>
    fail
      ? route.fulfill({ status: 503, body: "Unavailable" })
      : route.continue(),
  );
  await page.goto(base + "/politics");
  await page
    .getByLabel("Algoritmo di scrutinio")
    .selectOption("politiche-reform-2026-v1");
  await expect(
    page.getByRole("button", {
      name: "Riprova caricamento scenario",
      exact: true,
    }),
  ).toBeVisible();
  fail = false;
  await page
    .getByRole("button", { name: "Riprova caricamento scenario", exact: true })
    .click();
  await expect(page.getByTestId("fast-scenario-editor")).toBeVisible();
  const seed = page.getByLabel("Seme delle simulazioni", { exact: true });
  await seed.focus();
  await seed.fill("mobile-edit");
  await seed.press("Tab");
  await page.reload();
  await page
    .getByLabel("Algoritmo di scrutinio")
    .selectOption("politiche-reform-2026-v1");
  await expect(seed).toHaveValue("mobile-edit");
  page.once("dialog", (d) => d.accept());
  await page
    .getByRole("button", {
      name: "Ripristina scenario predefinito",
      exact: true,
    })
    .click();
  await expect(seed).toHaveValue(defaults.seed);
  expect(
    await page.evaluate(
      () => document.documentElement.scrollWidth <= window.innerWidth,
    ),
  ).toBe(true);
});

test("new reference-supported lists, explicit filing and strict JSON drafts", async ({
  page,
}) => {
  await open(page);
  await load(page);
  const group = page.getByRole("group", {
    name: "Nuova lista / lista congiunta",
    exact: true,
  });
  await group
    .getByLabel("Nome nuova lista", { exact: true })
    .fill("Lista del browser");
  await group.getByLabel("Senato", { exact: true }).uncheck();
  await group
    .getByLabel("Nuovo componente senza dati storici", { exact: true })
    .check();
  await group.getByLabel("Quota iniziale %", { exact: true }).fill("1");
  await group
    .getByRole("button", { name: "Aggiungi lista", exact: true })
    .click();
  await page.getByText("Depositi nei collegi", { exact: true }).click();
  await page
    .getByRole("combobox", { name: "Lista da depositare", exact: true })
    .selectOption({ label: "Lista del browser" });
  await page
    .getByRole("button", { name: "Deposita nei collegi filtrati", exact: true })
    .click();
  await expect(page.getByTestId("fit-status")).toHaveText("Modello pronto", {
    timeout: 15000,
  });
  const saved = await session(page),
    list = saved.scenario.lists.camera.find(
      (l: { name: string }) => l.name === "Lista del browser",
    );
  expect(
    saved.scenario.lists.senato.some((l: { id: string }) => l.id === list.id),
  ).toBe(false);
  expect(
    saved.scenario.coverage
      .filter((c: { chamber: string }) => c.chamber === "camera")
      .every((c: { listIds: string[] }) => c.listIds.includes(list.id)),
  ).toBe(true);
  expect(saved.scenario.data.fittedStateId).toBe(defaults.data.fittedStateId);
  await page
    .getByText("JSON completo: ipotesi, raggruppamenti e input forniti", {
      exact: true,
    })
    .click();
  await page
    .getByRole("button", {
      name: "Carica JSON corrente / scarta bozza",
      exact: true,
    })
    .click();
  const input = page.getByLabel("Scenario JSON", { exact: true }),
    raw = JSON.parse(await input.inputValue());
  raw.unknownField = true;
  await input.fill(JSON.stringify(raw));
  await expect(
    page.getByRole("button", { name: "Esegui riforma", exact: true }),
  ).toBeDisabled();
  await page.getByRole("button", { name: "Applica JSON", exact: true }).click();
  await expect(
    page.getByRole("alert").filter({ hasText: "unknownField" }),
  ).toBeVisible();
  await page
    .getByRole("button", {
      name: "Carica JSON corrente / scarta bozza",
      exact: true,
    })
    .click();
  await expect(
    page.getByRole("button", { name: "Esegui riforma", exact: true }),
  ).toBeEnabled();
  await page.getByTestId("reform-file-input").setInputFiles({
    name: "earlier-results.json",
    mimeType: "application/json",
    buffer: Buffer.from(JSON.stringify(installedDefaults)),
  });
  await expect(page.getByTestId("fast-reform-workspace")).toHaveCount(0);
  await expect(
    page.getByRole("button", { name: "Scarica scenario", exact: true }),
  ).toBeVisible();
  const legacyDownload = page.waitForEvent("download");
  await page
    .getByRole("button", { name: "Scarica scenario", exact: true })
    .click();
  const legacy = JSON.parse((await bytes(await legacyDownload)).toString());
  expect(legacy.models).toEqual(
    (installedDefaults as { models: unknown }).models,
  );
  await page.getByTestId("reform-file-input").setInputFiles({
    name: "fast-session.json",
    mimeType: "application/json",
    buffer: Buffer.from(JSON.stringify(saved)),
  });
  await expect(page.getByTestId("fast-reform-workspace")).toBeVisible();
  await expect(page.getByTestId("fit-status")).toHaveText("Modello pronto", {
    timeout: 60000,
  });
});
