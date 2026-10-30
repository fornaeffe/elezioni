/** Matched held-out evaluation of the installed production legacy kernel. */
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { resolve } from "node:path";
import { gunzipSync, gzipSync } from "node:zlib";
import { historicalInput, digest } from "./legacy-comparison-input";
import {
  preparePairedVoteReferences,
  generatePairedMunicipalVotes,
} from "../../src/lib/politics/reform-2026/paired-vote-generation";
import {
  distributionScores,
  summarizeUnits,
  scoreAreas,
  type AreaMapping,
} from "./filing-aware-scores";
import {
  loadCandidate,
  loadCandidateSource,
} from "../../src/lib/politics/reform-2026/vote-share/release";
import { readBytes } from "../data-preparation/fast-share-release-io";
import { normalizeName } from "../data-preparation/formats";
import { loadPartyResearchData } from "../data-preparation/party-drift-panel-data";
import policy from "../../../data/legacy-comparison-policy.json" with { type: "json" };
const root = resolve(".."),
  dir = resolve(root, "data/derived/legacy-comparison");
await mkdir(dir, { recursive: true });
const json = async (p: string) => JSON.parse(await readFile(p, "utf8"));
const input = await loadPartyResearchData(root);
const catalog = await json(
  "src/lib/politics/reform-2026/data/generation-catalog.generated.json",
);
const snapshot = JSON.parse(
    gunzipSync(await readFile(resolve("static", catalog.path))).toString(),
  ),
  template = await json(
    resolve("src/lib/politics/reform-2026/data", catalog.defaultsPath),
  );
const staticDir = resolve("static/data/reform-2026/fast-share"),
  loaded = await loadCandidate(
    await json(resolve(staticDir, "candidate.json")),
    readBytes(staticDir),
    { validateGeneration: false },
  ),
  source = await loadCandidateSource(loaded.catalog, readBytes(staticDir));
const keys = new Map<string, Set<string>>();
for (const u of source.payload.units)
  if (u.targetCode) {
    const k = u.region + ":" + normalizeName(u.name),
      v = keys.get(k) ?? new Set();
    v.add(u.targetCode);
    keys.set(k, v);
  }
const targets = new Map(
    source.payload.geography.targets.map((t) => [t.code, t]),
  ),
  cells = new Map<string, typeof source.payload.geography.cells>();
for (const c of source.payload.geography.cells)
  if (c.chamber === "camera") {
    const v = cells.get(c.municipality) ?? [];
    v.push(c);
    cells.set(c.municipality, v);
  }
const colleges = new Map(
  source.payload.geography.colleges.map((c) => [c.id, c]),
);
let entries: any[] = [];
for (const phase of ["development", "confirmation"])
  entries.push(
    ...(
      await json(
        resolve(
          root,
          "research/vote-generation/fv01/evidence/data/derived/party-drift/fast-" +
            phase +
            "-final-20261027/cases.json",
        ),
      )
    ).cases.filter(
      (c: any) =>
        c.variant === "kind-common-conservative" &&
        c.target.startsWith("camera"),
    ),
  );
const mode = process.argv[2] ?? "smoke";
if (mode === "full")
  entries = [
    {
      target: "camera-2018",
      horizon: 15,
      inputDirectory: "modular-1d5b31b17cfb",
    },
    {
      target: "camera-2022",
      horizon: 15,
      inputDirectory: "modular-939576fd812f",
    },
  ];
else if (mode === "smoke")
  entries = entries.filter(
    (e) => e.target === "camera-2018" && e.horizon === 15,
  );
if (process.env.LEGACY_TARGET)
  entries = entries.filter((e) => e.target === process.env.LEGACY_TARGET);
if (process.env.LEGACY_HORIZON)
  entries = entries.filter(
    (e) => e.horizon === Number(process.env.LEGACY_HORIZON),
  );
const draws = Number(process.env.LEGACY_DRAWS ?? policy.draws),
  report: any = {
    version: policy.version,
    mode,
    draws,
    startedAt: new Date().toISOString(),
    policyHash: digest(policy),
    rows: [],
  };
const cache = new Map<string, any>();
for (const entry of entries) {
  const folder = entry.inputDirectory.split(/[\\/]/).at(-1),
    sourceBytes = await readFile(
      resolve(root, "data/derived/party-drift", folder, "profiles.json"),
    ),
    truthSource = JSON.parse(sourceBytes.toString());
  const ids = truthSource.truth.map((t: any) => t.id).sort(),
    truth = new Map<string, any>(truthSource.truth.map((t: any) => [t.id, t])),
    positions = new Map<string, number>(
      truthSource.components.map((id: string, i: number) => [id, i + 1]),
    ),
    stride = positions.size + 1;
  const mapping: AreaMapping[] = [],
    territories: AreaMapping[] = [],
    unmatched: string[] = [];
  for (const city of ids) {
    const at = city.indexOf(":"),
      codes = keys.get(
        city.slice(0, at) + ":" + normalizeName(city.slice(at + 1)),
      );
    if (codes?.size !== 1) {
      unmatched.push(city);
      continue;
    }
    const code = [...codes][0],
      target = targets.get(code)!,
      areas = (cells.get(code) ?? []).map((c) => ({
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
  for (const variant of (process.env.LEGACY_VARIANTS ?? "no-polls,polls").split(
    ",",
  )) {
    const started = performance.now();
    try {
      const prepared = historicalInput(
          input,
          entry.target,
          truthSource.cutoff,
          template,
          snapshot,
          variant,
        ),
        key = digest({ effective: prepared.audit.effectiveKey, ids, draws });
      await writeFile(
        resolve(
          dir,
          entry.target +
            "-" +
            entry.horizon +
            "-" +
            variant +
            (mode === "full" ? "-full" : "") +
            "-inputs.json",
        ),
        JSON.stringify(
          { scenario: prepared.scenario, audit: prepared.audit },
          null,
          2,
        ),
      );
      let replay = cache.get(key),
        reused = false;
      if (!replay) {
        const refs = preparePairedVoteReferences(
            prepared.scenario,
            prepared.snapshot,
          ),
          selected = refs.units.filter((u) => truth.has(u.municipality.code)),
          rawLists = prepared.lists.map((l) => l.id),
          offset = new Map<string, number>(
            ids.map((id: string, i: number) => [id, i]),
          );
        const rawDraws: Uint32Array[] = [];
        let maximumResidual = 0;
        for (let d = 1; d <= draws; d++) {
          const values = new Uint32Array(ids.length * (rawLists.length + 1));
          for (const row of generatePairedMunicipalVotes(
            prepared.scenario,
            prepared.snapshot,
            selected,
            d,
          )) {
            const i = offset.get(row.municipalityCode)!;
            values[i * (rawLists.length + 1)] = row.abstention;
            let total = row.abstention;
            row.votes.forEach((v, j) => {
              values[i * (rawLists.length + 1) + j + 1] = v.votes;
              total += v.votes;
            });
            maximumResidual = Math.max(
              maximumResidual,
              Math.abs(total - row.electors),
            );
          }
          rawDraws.push(values);
          if (d % 128 === 0)
            console.log(
              JSON.stringify({
                target: entry.target,
                horizon: entry.horizon,
                variant,
                draw: d,
                draws,
                mode,
              }),
            );
        }
        replay = {
          rawDraws,
          rawLists,
          maximumResidual,
          case: entry.target + "/" + entry.horizon + "/" + variant,
        };
        cache.set(key, replay);
      } else reused = true;
      // Encode an entire observable group at its first coordinate. This is a
      // lossless scoring carrier, NOT a fabricated internal historical split.
      // scoreAreas merges all coordinates belonging to each observable group.
      const encoded: Uint32Array[] = replay.rawDraws.map((raw: Uint32Array) => {
        const out = new Uint32Array(ids.length * stride);
        ids.forEach((id: string, i: number) => {
          const t = truth.get(id)!,
            jByLabel = new Map<string, number>();
          t.sourceLabels.forEach((labels: string[], g: number) =>
            labels.forEach((label) =>
              jByLabel.set(
                digest(label).slice(0, 16),
                positions.get(t.groups[g][0])!,
              ),
            ),
          );
          out[i * stride] = raw[i * (replay.rawLists.length + 1)];
          replay.rawLists.forEach((list: string, j: number) => {
            const n = raw[i * (replay.rawLists.length + 1) + j + 1];
            if (n && !jByLabel.has(list))
              throw new Error(
                "Unobservable positive prediction " + id + "/" + list,
              );
            if (jByLabel.has(list)) out[i * stride + jByLabel.get(list)!] += n;
          });
        });
        return out;
      });
      const municipal: any[] = [],
        elector: any[] = [],
        details: any[] = [];
      ids.forEach((id: string, i: number) => {
        const t = truth.get(id)!;
        t.groups.forEach((g: string[], j: number) => {
          const at = g.map((c) => positions.get(c)!),
            count = encoded.map((d) =>
              at.reduce((n, k) => n + d[i * stride + k], 0),
            ),
            values = count.map(
              (n, d) => n / Math.max(1, t.electors - encoded[d][i * stride]),
            ),
            score = distributionScores(
              values,
              t.counts[j] / Math.max(1, t.electors - t.nonValid),
              t.electors - t.nonValid,
            );
          municipal.push({ unit: id, score });
          elector.push({
            unit: id,
            score: distributionScores(
              count.map((n) => n / t.electors),
              t.counts[j] / t.electors,
              t.electors,
            ),
          });
          details.push({
            municipality: id,
            members: g,
            truth: t.counts[j] / Math.max(1, t.electors - t.nonValid),
            score,
          });
        });
        elector.push({
          unit: id,
          score: distributionScores(
            encoded.map((d) => d[i * stride] / t.electors),
            t.nonValid / t.electors,
            t.electors,
          ),
        });
      });
      const area = scoreAreas(
          truthSource,
          ids,
          truthSource.components,
          encoded,
          mapping,
        ),
        territory = scoreAreas(
          truthSource,
          ids,
          truthSource.components,
          encoded,
          territories,
        ),
        national = scoreAreas(
          truthSource,
          ids,
          truthSource.components,
          encoded,
          ids.map((municipality: string) => ({
            municipality,
            areas: [{ id: "evaluated-area", weight: 1 }],
          })),
        );
      const row = {
        target: entry.target,
        horizon: entry.horizon,
        cutoff: truthSource.cutoff,
        variant,
        key,
        reused: reused ? replay.case : null,
        sourceHash: digest(sourceBytes.toString()),
        milliseconds: performance.now() - started,
        municipalities: ids.length,
        unmatched,
        baseline: prepared.audit.baseline,
        newListReferences: prepared.audit.newLists,
        unsupported: prepared.audit.unsupported,
        fallback: prepared.audit.fallback.length,
        maximumResidual: replay.maximumResidual,
        scores: {
          municipality: summarizeUnits(municipal),
          college: area.scores,
          constituency: territory.scores,
          sampledArea: national.scores,
        },
        electorScores: { municipality: summarizeUnits(elector) },
      };
      report.rows.push(row);
      await writeFile(
        resolve(
          dir,
          `${entry.target}-${entry.horizon}-${variant}-${mode}-scores.json.gz`,
        ),
        gzipSync(
          JSON.stringify({
            municipality: details,
            college: area.rows,
            constituency: territory.rows,
            national: national.rows,
          }),
        ),
      );
      console.log(
        JSON.stringify({
          completed: true,
          target: entry.target,
          horizon: entry.horizon,
          variant,
          reused: row.reused,
          milliseconds: row.milliseconds,
          scores: row.scores.municipality,
        }),
      );
      // Retain only reusable reduced draws; full arrays need not stay resident.
      if (mode === "full") cache.delete(key);
    } catch (e) {
      report.rows.push({
        target: entry.target,
        horizon: entry.horizon,
        variant,
        error: String(e),
      });
      console.error(String(e));
      process.exitCode = 1;
    }
    await writeFile(
      resolve(dir, (process.env.LEGACY_REPORT ?? mode) + "-report.json"),
      JSON.stringify(report, null, 2) + "\n",
    );
  }
}
