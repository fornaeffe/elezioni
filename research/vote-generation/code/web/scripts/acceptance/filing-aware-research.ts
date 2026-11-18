/** Frozen political-election comparison, separate from production acceptance. */
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { resolve } from "node:path";
import { createHash } from "node:crypto";
import { gzipSync } from "fflate";
import protocol from "../../../data/filing-aware-research-protocol.json" with { type: "json" };
import frozenSelections from "../../../data/filing-aware-selections.json" with { type: "json" };
import { prepareReplay, replay, type ReplaySource } from "./fast-share-replay";
import {
  distributionScores,
  summarizeScores,
  summarizeUnits,
  scoreAreas,
  type AreaMapping,
} from "./filing-aware-scores";
import {
  loadCandidate,
  loadCandidateSource,
} from "../../src/lib/politics/reform-2026/vote-share/release";
import {
  readBytes,
  implementationKey,
} from "../data-preparation/fast-share-release-io";
import { normalizeName } from "../data-preparation/formats";
import type { FieldModel } from "../../src/lib/politics/reform-2026/vote-share/feasible-field";
import { writeResearchArtifact } from "./filing-aware-artifact";
const mode = process.argv[2] ?? "smoke",
  baseMode = mode.replace(/^(reference|mass|regional)-/, "");
if (!["smoke", "development", "validation", "full"].includes(baseMode))
  throw new Error(
    "Use smoke, development, validation, full SOURCE_DIRECTORY_NAME, or reference-/mass- variants",
  );
const referenceRound = mode.startsWith("reference-");
const root = resolve(".."),
  directory = resolve(
    root,
    "data/derived/filing-aware-research",
    mode,
    ...(baseMode === "full" ? [process.argv[3]] : []),
  );
await mkdir(directory, { recursive: true });
const staticDirectory = resolve("static/data/reform-2026/fast-share"),
  loaded = await loadCandidate(
    JSON.parse(
      await readFile(resolve(staticDirectory, "candidate.json"), "utf8"),
    ),
    readBytes(staticDirectory),
  ),
  source = await loadCandidateSource(
    loaded.catalog,
    readBytes(staticDirectory),
  );
if (
  loaded.scenario.lists.camera.length !== 24 ||
  !loaded.scenario.lists.camera.some((l) => l.id === "futuro-nazionale")
)
  throw new Error(
    "Install the requested future scenario before research tests",
  );
const metadata = {
  protocolHash: createHash("sha256")
    .update(JSON.stringify(protocol))
    .digest("hex"),
  futureScenarioHash: createHash("sha256")
    .update(
      await readFile(resolve(root, "data/fast-share-future-scenario.json")),
    )
    .digest("hex"),
  generationImplementationId: await implementationKey(
    root,
    "web/src/lib/politics/reform-2026/vote-share/generation.ts",
  ),
  defaultFittedState: loaded.scenario.data.fittedStateId,
};
const entries: any[] = [];
for (const phase of ["development", "confirmation"]) {
  const archive = JSON.parse(
    await readFile(
      resolve(
        root,
        "research/vote-generation/fv01/evidence/data/derived/party-drift/fast-" +
          phase +
          "-final-20261027/cases.json",
      ),
      "utf8",
    ),
  );
  entries.push(
    ...archive.cases.filter(
      (c: any) => c.variant === "kind-common-conservative",
    ),
  );
}
const fullManifest =
  baseMode === "full"
    ? JSON.parse(
        await readFile(
          resolve(
            root,
            "data/derived/party-drift",
            process.argv[3],
            "manifest.json",
          ),
          "utf8",
        ),
      )
    : null;
const chosen = fullManifest
  ? [
      {
        target: fullManifest.target,
        horizon: fullManifest.horizon,
        inputDirectory: process.argv[3],
      },
    ]
  : baseMode === "smoke"
    ? entries.filter((c) => c.target === "camera-2018" && c.horizon === 15)
    : entries.filter(
        (c) =>
          (baseMode === "development"
            ? protocol.development
            : protocol.validation
          ).includes(c.target) && protocol.horizonsDays.includes(c.horizon),
      );
const selection =
  baseMode === "validation" || baseMode === "full"
    ? frozenSelections.initial
    : null;
const referenceProtocol =
  referenceRound || mode.startsWith("mass-") || mode.startsWith("regional-")
    ? JSON.parse(
        await readFile(
          resolve(
            root,
            referenceRound
              ? "data/filing-aware-reference-comparison.json"
              : mode.startsWith("regional-")
                ? "data/filing-aware-regional-comparison.json"
                : "data/filing-aware-mass-comparison.json",
          ),
          "utf8",
        ),
      )
    : null;
const candidates: any[] = referenceProtocol
  ? referenceProtocol.candidates
  : selection
    ? [selection.leadingCandidate, ...selection.comparators]
    : protocol.candidates;
if (
  (mode.startsWith("mass-") || mode.startsWith("regional-")) &&
  baseMode !== "development"
) {
  const massSelection = mode.startsWith("regional-")
    ? frozenSelections.regional
    : frozenSelections.mass;
  candidates.splice(0, candidates.length, massSelection.leadingCandidate);
}
const report = {
  stage: "filing-aware-comparison",
  mode,
  startedAt: new Date().toISOString(),
  ...metadata,
  requestedCases: chosen.length,
  draws: protocol.draws,
  rows: [] as any[],
};
const unitKeys = new Map<string, Set<string>>();
for (const u of source.payload.units)
  if (u.targetCode) {
    const key = u.region + ":" + normalizeName(u.name),
      codes = unitKeys.get(key) ?? new Set<string>();
    codes.add(u.targetCode);
    unitKeys.set(key, codes);
  }
const colleges = new Map(
  source.payload.geography.colleges.map((c) => [c.id, c]),
);
for (const entry of chosen) {
  const folder = entry.inputDirectory.split(/[\\/]/).at(-1),
    bytes = await readFile(
      resolve(root, "data/derived/party-drift", folder, "profiles.json"),
    ),
    input = JSON.parse(bytes.toString()) as ReplaySource,
    seed = protocol.seedPrefix + ":" + entry.target + ":" + entry.horizon,
    prepared = prepareReplay(input, seed),
    mapping: AreaMapping[] = [],
    territories: AreaMapping[] = [],
    unmatched: string[] = [];
  for (const city of prepared.profiles.municipalityIds) {
    const at = city.indexOf(":"),
      key = city.slice(0, at) + ":" + normalizeName(city.slice(at + 1)),
      codes = unitKeys.get(key);
    if (codes?.size !== 1) {
      unmatched.push(city);
      continue;
    }
    const code = [...codes][0],
      target = source.payload.geography.targets.find((t) => t.code === code)!;
    const cells = source.payload.geography.cells.filter(
        (c) => c.chamber === "camera" && c.municipality === code,
      ),
      areas = cells.map((c) => ({
        id: c.collegeId,
        weight: c.population / target.population,
      }));
    mapping.push({ municipality: city, areas });
    const grouped = new Map<string, number>();
    for (const a of areas) {
      const id = colleges.get(a.id)!.territoryId;
      grouped.set(id, (grouped.get(id) ?? 0) + a.weight);
    }
    territories.push({
      municipality: city,
      areas: [...grouped].map(([id, weight]) => ({ id, weight })),
    });
  }
  for (const candidate of candidates) {
    const started = performance.now(),
      label =
        candidate.id +
        "-scale-" +
        candidate.scale +
        (candidate.reference ? "-country-reference" : "") +
        (candidate.profileRegionCorrelation === undefined
          ? ""
          : "-rho-" + candidate.profileRegionCorrelation);
    try {
      const result = replay(input, seed, "reference", protocol.draws, {
        prepared,
        aggregateDomain: true,
        retainCounts: true,
        onProgress: (draw, total) =>
          console.log(
            JSON.stringify({
              progress: true,
              target: entry.target,
              horizon: entry.horizon,
              candidate: label,
              draw,
              total,
            }),
          ),
        ...(candidate.id === "legacy-global-beta-v1"
          ? {}
          : {
              model: candidate.id as FieldModel,
              scaleMultiplier: candidate.scale,
              referenceConstruction: candidate.reference,
              profileRegionCorrelation: candidate.profileRegionCorrelation,
            }),
      });
      const municipality = result.metrics
          .filter((r) => r.scope === "municipal")
          .map((r) => {
            const t = input.truth.find((t) => t.id === r.municipality)!;
            return {
              unit: r.municipality!,
              score: distributionScores(
                r.values,
                r.truth,
                t.electors - t.nonValid,
              ),
            };
          }),
        aggregate = result.metrics
          .filter((r) => r.scope === "national")
          .map((r) =>
            distributionScores(
              r.values,
              r.truth,
              input.truth.reduce((s, t) => s + t.electors - t.nonValid, 0),
            ),
          ),
        college = scoreAreas(
          input,
          result.municipalityIds!,
          result.componentIds!,
          result.municipalDraws!,
          mapping,
        ),
        constituency = scoreAreas(
          input,
          result.municipalityIds!,
          result.componentIds!,
          result.municipalDraws!,
          territories,
        );
      const row = {
        target: entry.target,
        horizon: entry.horizon,
        candidate: label,
        milliseconds: performance.now() - started,
        municipalities: result.municipalityIds!.length,
        colleges: college.areas,
        constituencies: constituency.areas,
        unmatched,
        sourceDirectory: folder,
        sourceHash: createHash("sha256").update(bytes).digest("hex"),
        scores: {
          municipality: summarizeUnits(municipality),
          college: college.scores,
          constituency: constituency.scores,
          sampledArea: summarizeScores(aggregate),
        },
        electorScores: {
          municipality: result.scores.municipalElector,
          aggregate: result.scores.nationalElector,
        },
        maximumResidual: result.maximumResidual,
        referenceConditionedDraws: result.referenceConditionedDraws,
        fitDiagnostics: result.fitDiagnostics,
      };
      await writeResearchArtifact(
        resolve(
          directory,
          entry.target + "-" + entry.horizon + "-" + label + ".json.gz",
        ),
        {
          metrics: result.metrics,
          college: college.rows,
          constituency: constituency.rows,
        },
      );
      report.rows.push(row);
      console.log(
        JSON.stringify({
          target: entry.target,
          horizon: entry.horizon,
          candidate: label,
          milliseconds: row.milliseconds,
          municipality: row.scores.municipality,
        }),
      );
    } catch (error) {
      report.rows.push({
        target: entry.target,
        horizon: entry.horizon,
        candidate: label,
        error: String(error),
      });
      process.exitCode = 1;
      console.error(String(error));
    }
    await writeFile(
      resolve(directory, "report.json"),
      JSON.stringify(report, null, 2) + "\n",
    );
  }
}
