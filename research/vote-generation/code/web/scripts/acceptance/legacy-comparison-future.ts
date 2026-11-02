import { readFile, writeFile, mkdir } from "node:fs/promises";
import { resolve } from "node:path";
import { gunzipSync } from "node:zlib";
import {
  loadCandidate,
  loadCandidateSource,
} from "../../src/lib/politics/reform-2026/vote-share/release";
import { readBytes } from "../data-preparation/fast-share-release-io";
import {
  prepareReformGeneration,
  generateReformSimulation,
} from "../../src/lib/politics/reform-2026/generation";
import { preparePairedVoteReferences } from "../../src/lib/politics/reform-2026/paired-vote-generation";
import { runPoliticsReformScrutiny } from "../../src/lib/politics/reform-2026/scrutiny";
import { links, addReferences, digest } from "./legacy-comparison-input";
import future from "../../../data/fast-share-future-scenario.json" with { type: "json" };
import futurePolls from "../../../data/legacy-comparison-future-polls.json" with { type: "json" };
const json = async (p: string) => JSON.parse(await readFile(p, "utf8")),
  root = resolve(".."),
  dir = resolve(root, "data/derived/legacy-comparison");
await mkdir(dir, { recursive: true });
const catalog = await json(
    "src/lib/politics/reform-2026/data/generation-catalog.generated.json",
  ),
  snapshot = JSON.parse(
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
const raw = {
  supplementalPolls: futurePolls.polls,
  polls: source.payload.polls
    .filter((p) => p.status === "supported" && p.context === "camera")
    .map((p) => ({
      id: p.id,
      surveyId: p.surveyId,
      kind: "camera",
      date: p.date,
      end: p.fieldworkEnd,
      available: p.availableDate,
      sample: p.effectiveSample,
      source: p.citation,
      groups: p.normalized
        .filter((a) => !a.other)
        .map((a) => a.members.map((m) => loaded.scenario.componentMap[m] ?? m)),
      shares: p.normalized.filter((a) => !a.other).map((a) => a.share),
    })),
};
const lists = future.lists.map((l) => ({
  ...l,
  members: l.members.map(
    (m) => future.aliases.find((a) => a.from === m)?.to ?? m,
  ),
}));
const reports: any[] = [];
for (const variant of ["no-polls", "polls"]) {
  const scenario = {
    ...structuredClone(template),
    seed: "installed-legacy-future-v1",
    electionDate: loaded.scenario.electionDate,
    projectionDate: loaded.scenario.referenceDate,
    lists: {
      camera: loaded.scenario.lists.camera.map(({ componentIds, ...l }) => l),
      senato: loaded.scenario.lists.senato.map(({ componentIds, ...l }) => l),
    },
    coverage: loaded.scenario.coverage,
    bonusLinks: loaded.scenario.bonusLinks,
    candidateGeneration: loaded.scenario.candidateGeneration,
    candidateTemplates: loaded.scenario.candidateTemplates,
    shares: [],
    abstention: [],
    correspondences: [],
    presentation: {
      name: "Requested 24-list legacy comparison",
      coalitions: loaded.scenario.presentation.coalitions,
      lists: future.lists.map((l, i) => ({
        id: l.id,
        color: ["#2563eb", "#dc2626", "#16a34a"][i % 3],
      })),
    },
  } as any;
  for (const ch of ["camera", "senato"])
    for (const h of snapshot.historicalLists) {
      const choice = source.payload.choices
        .filter((c) => c.eventId === ch + "-2022" && c.label === h.name)
        .sort(
          (a, b) =>
            Number(a.method === "opaque-special-ballot") -
            Number(b.method === "opaque-special-ballot"),
        )[0];
      if (!choice) continue;
      const split = (
        futurePolls.historicalTicketSplits as Record<
          string,
          Record<string, number>
        >
      )[h.name];
      if (ch === "senato" && split) {
        for (const [futureListId, factor] of Object.entries(split))
          scenario.correspondences.push({
            chamber: ch,
            pastListId: h.id,
            futureListId,
            factor,
          });
        continue;
      }
      const members = (
        h.name === "TEAM K"
          ? ["team-k"]
          : (loaded.scenario.sourceMemberships[choice.id]?.members ??
            choice.members)
      )
        .map((m) => loaded.scenario.componentMap[m] ?? m)
        .filter((m) => m !== "historical-rest");
      for (const l of links("camera-2022", h.name, members, lists))
        scenario.correspondences.push({
          chamber: ch,
          pastListId: h.id,
          futureListId: l.id,
          factor: l.factor,
        });
    }
  // Existing scenario's explicitly authored regional point assumptions only;
  // no candidate fitted profiles, fitted parameters, or inferred counts.
  const regional = new Map<string, any>();
  for (const ref of loaded.scenario.references.filter(
    (r) => r.scope.kind !== "national",
  )) {
    const list = lists.find(
      (l) => l.id === ref.subject.id || l.members.includes(ref.subject.id),
    );
    if (!list) continue;
    const key = JSON.stringify([ref.scope, list.id]),
      row = regional.get(key) ?? {
        scope: ref.scope,
        listId: list.id,
        common: null,
        camera: null,
        senato: null,
      };
    if (ref.channel === "common") row.common = ref.mean;
    else row[ref.channel] = ref.mean;
    regional.set(key, row);
  }
  // The requested VdA ballot offers four lists. With no recognized historical
  // lineage for these broad tickets, complete their explicit regional scenario
  // shares; the residual is an assumption, not FN's national poll estimate.
  const vda = {
    scope: { code: "02", kind: "region" },
    listId: "futuro-nazionale",
    common: null,
    camera: 1,
    senato: 1,
  };
  for (const row of regional.values())
    if (row.scope.kind === "region" && row.scope.code === "02") {
      vda.camera -= row.camera ?? 0;
      vda.senato -= row.senato ?? 0;
    }
  regional.set(JSON.stringify([vda.scope, vda.listId]), vda);
  const taaFn = {
    scope: { code: "04", kind: "region" },
    listId: "futuro-nazionale",
    common: null,
    camera: 0.072,
    senato: 0.072,
  };
  regional.set(JSON.stringify([taaFn.scope, taaFn.listId]), taaFn);
  for (const r of loaded.scenario.nonValidReferences) {
    const key = JSON.stringify(r.scope),
      row = scenario.abstention.find(
        (x: any) => JSON.stringify(x.scope) === key,
      ) ?? { scope: r.scope, common: null, camera: null, senato: null };
    row[r.channel] = r.mean;
    if (!scenario.abstention.includes(row)) scenario.abstention.push(row);
  }
  scenario.shares.push(...regional.values());
  const audit = addReferences(
    scenario,
    snapshot,
    raw,
    loaded.scenario.referenceDate,
    lists,
    variant,
  );
  for (const [key, row] of regional)
    if (
      !scenario.shares.some(
        (r: any) => JSON.stringify([r.scope, r.listId]) === key,
      )
    )
      scenario.shares.push(row);
  scenario.projectionDate = loaded.scenario.referenceDate;
  if (scenario.pollReferences)
    scenario.pollReferences.date = scenario.projectionDate;
  const support = preparePairedVoteReferences(scenario, snapshot).units;
  audit.specialRegionSupport = [];
  for (const ch of ["camera", "senato"] as const)
    for (const region of ["02", "04"]) {
      const colleges = new Set(
        snapshot.seats.geography.colleges
          .filter(
            (c: any) =>
              c.chamber === ch &&
              snapshot.seats.geography.territories.some(
                (t: any) => t.id === c.territoryId && t.regionId === region,
              ),
          )
          .map((c: any) => c.id),
      );
      const offered = [
        ...new Set<string>(
          scenario.coverage
            .filter((c: any) => c.chamber === ch && colleges.has(c.collegeId))
            .flatMap((c: any) => c.listIds),
        ),
      ];
      const units = support.filter((u) => u.municipality.regionId === region),
        valid = units.reduce(
          (n, u) => n + u.municipality[ch].electors * (1 - u[ch].abstention),
          0,
        );
      for (const listId of offered) {
        const share =
          units.reduce(
            (n, u) =>
              n +
              u.municipality[ch].electors *
                (1 - u[ch].abstention) *
                (u[ch].shares.get(listId) ?? 0),
            0,
          ) / valid;
        if (!(share > 0))
          throw Error(
            "Unsupported filed special-region list: " +
              ch +
              "/" +
              region +
              "/" +
              listId,
          );
        audit.specialRegionSupport.push({ chamber: ch, region, listId, share });
      }
    }
  const started = performance.now();
  let error: string | null = null;
  try {
    const prepared = prepareReformGeneration(scenario, snapshot),
      generated = generateReformSimulation(prepared, 1),
      legal = runPoliticsReformScrutiny(generated.input);
    console.log(
      JSON.stringify({
        variant,
        ms: performance.now() - started,
        seats: legal.seats.length,
        filled: legal.seats.filter((s) => s.status === "filled").length,
        warnings: legal.warnings?.length,
      }),
    );
  } catch (e) {
    error = String(e);
    console.error(variant, error);
  }
  await writeFile(
    resolve(dir, "future-" + variant + ".json"),
    JSON.stringify({
      snapshot,
      scenario,
      audit: {
        ...audit,
        key: digest({ snapshot, scenario }),
        unmapped: snapshot.historicalLists
          .filter(
            (l: any) =>
              !scenario.correspondences.some((c: any) => c.pastListId === l.id),
          )
          .map((l: any) => ({ id: l.id, name: l.name })),
        regionalPriors: [...regional.values()],
      },
    }),
  );
  reports.push({ variant, error });
}
if (reports.some((r) => r.error)) process.exitCode = 1;
