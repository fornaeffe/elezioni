/** Freeze compact, source-pinned comparison evidence after all requested runs. */
import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { gzipSync, gunzipSync } from "node:zlib";
import { digest } from "./legacy-comparison-input";
import { JointTailScores } from "./filing-aware-monte-carlo";
import policy from "../../../data/legacy-comparison-policy.json" with { type: "json" };
import futurePolls from "../../../data/legacy-comparison-future-polls.json" with { type: "json" };
const root = resolve(".."),
  dir = resolve(root, "data/derived/legacy-comparison"),
  json = async (p: string) => JSON.parse(await readFile(p, "utf8"));
const sha = (b: Uint8Array | string) =>
  createHash("sha256").update(b).digest("hex");
const candidate = await json(
  resolve(root, "test/fixtures/benchmarks/vote_share_feasible_research.json"),
);
const candidateDetails = JSON.parse(
  gunzipSync(
    await readFile(
      resolve(
        root,
        "test/fixtures/benchmarks/vote_share_feasible_details.json.gz",
      ),
    ),
  ).toString(),
);
const selected = "bounded-mass-pair-field-v2-scale-1.5-rho-0.25",
  rows: any[] = [],
  details: any[] = [],
  inputs: any[] = [],
  sources: any[] = [];
const historicalOnly = process.argv.includes("--historical-only");
for (const mode of ["reduced", "full"]) {
  const bytes = await readFile(resolve(dir, mode + "-report.json")),
    report = JSON.parse(bytes.toString());
  sources.push({ path: mode + "-report.json", sha256: sha(bytes) });
  const expected = mode === "full" ? 4 : 24;
  if (report.rows.length !== expected || report.rows.some((r: any) => r.error))
    throw new Error("Incomplete " + mode + " report");
  for (const r of report.rows) {
    if (report.draws !== 512 || r.maximumResidual !== 0)
      throw new Error("Invalid draw count/conservation");
    const prior = candidate.reports
      .filter((p: any) =>
        mode === "full"
          ? p.mode === "regional-full"
          : p.mode === "regional-development" ||
            p.mode === "regional-validation",
      )
      .flatMap((p: any) => p.report.rows)
      .find(
        (p: any) =>
          p.target === r.target &&
          p.horizon === r.horizon &&
          p.candidate === selected,
      );
    if (!prior) throw new Error("Missing selected candidate match");
    const profile = await readFile(
      resolve(
        root,
        "data/derived/party-drift",
        prior.sourceDirectory,
        "profiles.json",
      ),
    );
    if (
      prior.sourceHash !== sha(profile) ||
      (r.sourceHash !== digest(profile.toString()) &&
        r.sourceHash !== sha(profile))
    )
      throw new Error("Mismatched scoring source");
    const raw = JSON.parse(
      gunzipSync(
        await readFile(
          resolve(
            dir,
            `${r.target}-${r.horizon}-${r.variant}-${mode}-scores.json.gz`,
          ),
        ),
      ).toString(),
    );
    const joint: any = {};
    for (const [scope, key] of [
      ["college", "college"],
      ["constituency", "constituency"],
      ["sampledArea", "national"],
    ]) {
      const x = new JointTailScores(512);
      for (const row of raw[key]) x.add(row.area, row.values, row.truth);
      joint[scope] = x.finish();
    }
    const previous = candidateDetails.details.find(
      (p: any) =>
        p.target === r.target &&
        p.horizon === r.horizon &&
        p.candidate === selected &&
        (mode === "full"
          ? p.mode === "regional-full"
          : p.mode !== "regional-full"),
    );
    const differences: any = {};
    for (const scope of ["college", "constituency", "sampledArea"]) {
      differences[scope] = {};
      for (const key of ["error30", "error15", "extreme80"]) {
        const a = joint[scope][key],
          b = previous.mc[scope === "sampledArea" ? "national" : scope][key],
          delta = a.mean - b.mean,
          se = Math.hypot(a.mcse, b.mcse);
        differences[scope][key] = {
          legacyMinusSelected: delta,
          mcse: se,
          normal95: [delta - 1.96 * se, delta + 1.96 * se],
        };
      }
    }
    const bias: any[] = [];
    for (const [scope, key] of [
      ["municipality", "municipality"],
      ["college", "college"],
      ["constituency", "constituency"],
      ["sampledArea", "national"],
    ]) {
      const groups = new Map<string, any[]>();
      for (const row of raw[key]) {
        const key = JSON.stringify([...row.members].sort()),
          g = groups.get(key) ?? [];
        g.push(row);
        groups.set(key, g);
      }
      for (const [members, g] of groups)
        bias.push({
          scope,
          members: JSON.parse(members),
          units: g.length,
          truth: g.reduce((n, r) => n + r.truth, 0) / g.length,
          expected: g.reduce((n, r) => n + r.score.mean, 0) / g.length,
          signedError: g.reduce((n, r) => n + r.score.meanBias, 0) / g.length,
          absoluteError: g.reduce((n, r) => n + r.score.meanMae, 0) / g.length,
        });
    }
    const inputPath = resolve(
        dir,
        `${r.target}-${r.horizon}-${r.variant}${mode === "full" ? "-full" : ""}-inputs.json`,
      ),
      prepared = await json(inputPath);
    inputs.push({
      mode,
      target: r.target,
      horizon: r.horizon,
      variant: r.variant,
      ...prepared,
    });
    details.push({
      mode,
      target: r.target,
      horizon: r.horizon,
      variant: r.variant,
      joint,
      bias,
      college: raw.college.map(({ values, ...r }: any) => r),
      constituency: raw.constituency.map(({ values, ...r }: any) => r),
      national: raw.national.map(({ values, ...r }: any) => r),
    });
    rows.push({
      ...r,
      mode,
      sourceHash: sha(profile),
      selected: prior.scores,
      tailDifferences: differences,
      unmappedOrUnfiledValid: prepared.audit.unmappedOrUnfiledValid ?? null,
      originalValid: prepared.audit.originalValid ?? null,
    });
  }
}
const future: any[] = [];
for (const variant of historicalOnly ? [] : ["no-polls", "polls"]) {
  const inputBytes = await readFile(
      resolve(dir, "future-" + variant + ".json"),
    ),
    input = JSON.parse(inputBytes.toString()),
    browser = await json(resolve(dir, "browser-" + variant + ".json"));
  if (browser.draws !== 1000 || browser.result.completed !== 1000)
    throw new Error("Incomplete browser run " + variant);
  if (browser.inputSha256 && browser.inputSha256 !== sha(inputBytes))
    throw new Error("Browser input changed after run " + variant);
  future.push({
    variant,
    inputSha256: sha(inputBytes),
    snapshotVersion: input.snapshot.version,
    modelVersion: input.snapshot.models.version,
    historicalLists: input.snapshot.historicalLists,
    sourceVersions: input.snapshot.sourceVersions,
    scenario: input.scenario,
    audit: input.audit,
    ...browser,
  });
}
const codeHashes: any = {};
for (const file of [
  "web/scripts/acceptance/legacy-comparison.ts",
  "web/scripts/acceptance/legacy-comparison-input.ts",
  "web/scripts/acceptance/legacy-comparison-future.ts",
  "web/scripts/acceptance/legacy-comparison-diagnose.ts",
  "web/tests/fixtures/legacy-comparison.worker.ts",
  "web/src/lib/politics/reform-2026/paired-vote-generation.ts",
  "web/src/lib/politics/reform-2026/generation.ts",
  "web/src/lib/politics/reform-2026/recovery.ts",
  "web/src/lib/politics/reform-2026/recovery-communications.ts",
  "web/src/lib/politics/reform-2026/taa-allocation.ts",
])
  codeHashes[file] = sha(await readFile(resolve(root, file)));
const diagnostic = await json(resolve(dir, "diagnostic-polls-23.json"));
const failureDiagnostics = {
  ...diagnostic,
  stack: diagnostic.stack?.replaceAll(root, "<repository>"),
  method:
    "Replay of the recorded simulation with unmodified production preparation, generation and scrutiny modules.",
  nonComputableReplay: await json(resolve(dir, "diagnostic-polls-135.json")),
};
if (!historicalOnly) {
  await writeFile(
    resolve(root, "test/fixtures/benchmarks/vote_share_legacy_comparison.json"),
    JSON.stringify(
      {
        schemaVersion: 1,
        scope:
          "Installed R-derived production generator; historical Camera accuracy and requested future paired browser checks. Research, not public release acceptance.",
        qualification:
          "Reused elections; source collection and manual mixed-wave point references differ from the selected fitted procedure. Monte Carlo intervals use whole draws and independent model streams, conditional on these fixed cases, not uncertainty about future elections. Municipal summary bias cancels compositionally; inspect group bias.",
        policy,
        futurePolls,
        codeHashes,
        baseRevision: execFileSync("git", ["rev-parse", "HEAD"], {
          cwd: root,
          encoding: "utf8",
        }).trim(),
        environment: {
          node: process.version,
          platform: process.platform,
          architecture: process.arch,
        },
        sources,
        rows,
        future,
        failureDiagnostics,
      },
      null,
      2,
    ) + "\n",
  );
  await writeFile(
    resolve(root, "test/fixtures/benchmarks/vote_share_legacy_details.json.gz"),
    gzipSync(JSON.stringify({ inputs, details })),
  );
}
const percentage = (x: number) => (100 * x).toFixed(2),
  modelName = (id: string) =>
    id === "selected"
      ? "Selected candidate"
      : id === "polls"
        ? "Legacy + poll refresh"
        : "Legacy, no general refresh";
const table = [
  "### Full-domain historical results",
  "",
  "At the 15-day source cutoff; 512 draws. Error30 and coverage are percentages; MAE is in percentage points. Lower error/MAE/WIS is better.",
  "",
  "| Election | Scope | Procedure | Error30 % | Coverage90 % | Mean MAE pp | WIS |",
  "| --- | --- | --- | ---: | ---: | ---: | ---: |",
];
for (const target of ["camera-2018", "camera-2022"])
  for (const scope of ["municipality", "college"]) {
    const group = rows.filter((r) => r.mode === "full" && r.target === target),
      variants = [{ variant: "selected", scores: group[0].selected }, ...group];
    for (const r of variants) {
      const s = r.scores[scope];
      table.push(
        `| ${target.slice(-4)} | ${scope} | ${modelName(r.variant)} | ${percentage(s.error30)} | ${percentage(s.coverage90)} | ${percentage(s.meanMae)} | ${s.wis.toFixed(4)} |`,
      );
    }
  }
table.push(
  "",
  "### Reduced panels across all four cutoffs",
  "",
  "Equal weight per cutoff, including reused identical no-refresh forecasts.",
  "",
  "| Election | Procedure | Municipal Error30 % | Municipal coverage90 % | Municipal MAE pp | College Error30 % | College coverage90 % |",
  "| --- | --- | ---: | ---: | ---: | ---: | ---: |",
);
for (const target of ["camera-2013", "camera-2018", "camera-2022"])
  for (const variant of ["selected", "no-polls", "polls"]) {
    const cases = rows.filter(
      (r) =>
        r.mode === "reduced" &&
        r.target === target &&
        r.variant === (variant === "selected" ? "no-polls" : variant),
    );
    const mean = (scope: string, key: string) =>
      cases.reduce(
        (n, r) =>
          n + (variant === "selected" ? r.selected : r.scores)[scope][key],
        0,
      ) / cases.length;
    table.push(
      `| ${target.slice(-4)} | ${modelName(variant)} | ${percentage(mean("municipality", "error30"))} | ${percentage(mean("municipality", "coverage90"))} | ${percentage(mean("municipality", "meanMae"))} | ${percentage(mean("college", "error30"))} | ${percentage(mean("college", "coverage90"))} |`,
    );
  }
table.push(
  "",
  "### Whole-draw simulation error for full-domain college tails",
  "",
  "Legacy minus selected Error30, in percentage points. Intervals describe Monte Carlo error only.",
  "",
  "| Election | Legacy procedure | Difference pp | Monte Carlo SE pp | Normal 95% interval pp |",
  "| --- | --- | ---: | ---: | --- |",
);
for (const r of rows.filter((r) => r.mode === "full")) {
  const d = r.tailDifferences.college.error30;
  table.push(
    `| ${r.target.slice(-4)} | ${modelName(r.variant)} | ${percentage(d.legacyMinusSelected)} | ${percentage(d.mcse)} | ${d.normal95.map(percentage).join(" to ")} |`,
  );
}
table.push(
  "",
  "### Requested future ballot in Chromium",
  "",
  "1,000 attempted paired simulations per procedure, including all failed attempts.",
  "",
  "| Legacy procedure | Browser | Total min | Load s | Preparation s | Generation min | Scrutiny min | Exceptions | Non-computable returned results | Filled 588-seat results / returned results |",
  "| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |",
);
for (const f of future) {
  const r = f.result,
    returned = r.ledger.filter((x: any) => !x.error);
  table.push(
    `| ${modelName(f.variant)} | ${f.browser} | ${(r.milliseconds / 60000).toFixed(2)} | ${(r.loadMs / 1000).toFixed(2)} | ${(r.prepareMs / 1000).toFixed(2)} | ${(r.generationMs / 60000).toFixed(2)} | ${(r.scrutinyMs / 60000).toFixed(2)} | ${r.failed} | ${r.nonComputable} | ${returned.filter((x: any) => x.filled === 588).length} / ${returned.length} |`,
  );
}
table.push(
  "",
  future.length
    ? "Failed draw IDs: " +
        future
          .map(
            (f) =>
              `${f.variant}: ${
                f.result.ledger
                  .filter((r: any) => r.error)
                  .map((r: any) => r.id)
                  .join(", ") || "none"
              }`,
          )
          .join("; ") +
        "."
    : "Browser measurements are pending. This historical-only report does not freeze complete benchmark evidence.",
  "",
  "Elapsed time includes failed attempts. The generation/scrutiny sub-timers include completed attempts only; they must not be used to hide time spent in exceptions.",
  ...(future.length
    ? [
        "",
        "Non-computable draw IDs: " +
          future
            .map(
              (f) =>
                `${f.variant}: ${
                  f.result.ledger
                    .filter((r: any) => r.status === "non-computable")
                    .map((r: any) => r.id)
                    .join(", ") || "none"
                }`,
            )
            .join("; ") +
          ".",
      ]
    : []),
  "",
);
const docPath = resolve(
    root,
    "docs/vote-generation-legacy-comparison-results.md",
  ),
  doc = await readFile(docPath, "utf8");
await writeFile(
  docPath,
  doc.replace(
    /<!-- BEGIN GENERATED COMPARISON -->[\s\S]*?<!-- END GENERATED COMPARISON -->/,
    "<!-- BEGIN GENERATED COMPARISON -->\n" +
      table.join("\n") +
      "\n<!-- END GENERATED COMPARISON -->",
  ),
);
console.log(
  JSON.stringify({
    historical: rows.length,
    future: future.map((r) => ({
      variant: r.variant,
      minutes: r.result.milliseconds / 60000,
      failed: r.result.failed,
      nonComputable: r.result.nonComputable,
      failures: r.result.ledger.filter((r: any) => r.error || r.filled !== 588),
    })),
  }),
);
