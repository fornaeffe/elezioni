/** Authored future filing; original grouped observations are never decomposed. */
import futureData from "../../../data/fast-share-future-scenario.json" with { type: "json" };
import type { DefaultSharePolicy } from "./fast-share-default";
import type {
  CompactSource,
  OriginalRow,
} from "../../src/lib/politics/reform-2026/vote-share/source-types";
import type { FastReference } from "../../src/lib/politics/reform-2026/vote-share/forecast-types";
import type { ReformGenerationScenario } from "../../src/lib/politics/reform-2026/generation-types";
import type { SourceSelection } from "../../src/lib/politics/reform-2026/vote-share/source-compiler";
import { normalizeName } from "./formats";
import { sum } from "../../src/lib/politics/reform-2026/vote-share/types";
const recognized = new Map(
  [
    ["TEAM K", "team-k"],
    ["DIE FREIHEITLICHEN", "freiheitlichen"],
    ["UNION VALDOTAINE", "union-valdotaine"],
    ["SÜD-TIROLER FREIHEIT", "suedtiroler-freiheit"],
  ].map(([a, b]) => [normalizeName(a), b]),
);
export function futureSharePlan(
  source: CompactSource,
  old: Pick<ReformGenerationScenario, "lists" | "coverage">,
  policy: DefaultSharePolicy,
  camera: OriginalRow[],
  senato: OriginalRow[],
) {
  if (policy.futureScenario !== futureData.id)
    throw new Error("Unknown authored future scenario");
  const inventory = [...policy.polls, ...Object.keys(policy.excludedPolls)];
  if (
    new Set(inventory).size !== inventory.length ||
    inventory.length !== source.payload.polls.length ||
    source.payload.polls.some((p) => !inventory.includes(p.id))
  )
    throw new Error(
      "Default poll inventory drift; review the pinned selection",
    );
  const future = futureData,
    allMembers = [...new Set(future.lists.flatMap((l) => l.members))].sort();
  const known = new Set(allMembers),
    pool: Record<string, string> = {},
    aliases = new Map(future.aliases.map((a) => [a.from, a.to]));
  for (const choice of source.payload.choices) {
    const match = recognized.get(normalizeName(choice.label));
    if (match && choice.members.length === 1)
      aliases.set(choice.members[0], match);
  }
  const observed = new Set(
    [
      ...source.payload.choices
        .filter((c) => policy.history.includes(c.eventId))
        .flatMap((c) => c.members),
      ...source.payload.polls
        .filter((p) => policy.polls.includes(p.id))
        .flatMap((p) => p.normalized.flatMap((a) => a.members)),
    ].map((id) => aliases.get(id) ?? id),
  );
  const fitted = allMembers.filter((id) => observed.has(id)),
    newMembers = allMembers.filter((id) => !observed.has(id)),
    components = [...fitted, "historical-rest"].sort();
  for (const id of new Set([
    ...source.payload.choices.flatMap((c) => c.members),
    ...source.payload.polls.flatMap((p) =>
      p.normalized.flatMap((a) => a.members),
    ),
  ])) {
    const target = aliases.get(id) ?? id;
    pool[id] =
      known.has(target) && observed.has(target) ? target : "historical-rest";
  }
  const regions = new Map([
    ["01", "01"],
    ["02", "01"],
    ["03", "03"],
    ["04", "03"],
    ["05", "03"],
    ["06", "03"],
    ["07", "05"],
    ["08", "05"],
    ["09", "06"],
    ["10", "07"],
    ["11", "08"],
    ["12", "09"],
    ["13", "10"],
    ["14", "11"],
    ["15", "12"],
    ["16", "12"],
    ["17", "13"],
    ["18", "14"],
    ["19", "15"],
    ["20", "15"],
    ["21", "16"],
    ["22", "17"],
    ["23", "18"],
    ["24", "19"],
    ["25", "19"],
    ["26", "20"],
    ["27", "02"],
    ["28", "04"],
  ]);
  const restrictedSenate = new Set(
    future.restrictedCameraTerritories.map((t) => regions.get(t)!),
  );
  const byCollege = new Map(
    source.payload.geography.colleges.map((c) => [c.id, c]),
  );
  const chamberLists = () =>
    future.lists.map((l) => ({
      id: l.id,
      name: l.name,
      coalitionId: l.coalition ?? null,
      protectedMinority: l.minorityRegion
        ? {
            regionId: l.minorityRegion,
            legalReference:
              "Scenario declaration requested by Luca; eligibility remains subject to the reform legal rules.",
          }
        : null,
      componentIds: l.members,
    }));
  const lists = { camera: chamberLists(), senato: chamberLists() };
  const coverage = old.coverage.map((c) => {
    const college = byCollege.get(c.collegeId),
      territory =
        college?.territoryId.split(":").at(-1) ??
        (c.collegeId === "camera:P:040001" ? "28" : undefined);
    if (!territory)
      throw new Error("Unknown future filing college " + c.collegeId);
    const region = c.chamber === "camera" ? regions.get(territory)! : territory,
      kind =
        c.chamber === "camera"
          ? territory === "27"
            ? "vda"
            : territory === "28"
              ? "taa"
              : "ordinary"
          : region === "02"
            ? "vda"
            : region === "04"
              ? "taa"
              : "ordinary";
    return {
      ...c,
      listIds: future.lists
        .filter(
          (l) =>
            l.filing === "all" ||
            l.filing === kind ||
            (l.filing === "ordinary-taa" && kind !== "vda") ||
            (l.filing === "restricted" &&
              kind === "ordinary" &&
              (c.chamber === "camera"
                ? future.restrictedCameraTerritories.includes(territory)
                : restrictedSenate.has(region))),
        )
        .map((l) => l.id),
    };
  });
  const units = new Map(source.payload.units.map((u) => [u.id, u])),
    byCode = new Map(
      camera
        .filter((r) => units.get(r.unit)?.targetCode)
        .map((r) => [units.get(r.unit)!.targetCode!, r]),
    );
  const targets = source.payload.geography.targets.map((t) => {
    const row = byCode.get(t.code);
    if (!row) throw new Error("Missing original electorate " + t.code);
    return { code: t.code, electors: row.electors };
  });
  const selection: SourceSelection = {
    referenceDate: policy.referenceDate,
    electionIds: policy.history,
    pollIds: policy.polls,
    components,
    componentMap: pool,
    sourceMemberships: Object.fromEntries(
      source.payload.choices.map((c) => [
        c.id,
        {
          members: c.members,
          provenance: c.method + "; original grouped counts retained",
        },
      ]),
    ),
    targets,
    targetContext: "camera",
  };
  const references: FastReference[] = [],
    nonValidReferences: {
      id: string;
      scope: { kind: "region"; code: string };
      channel: "camera" | "senato";
      mean: number;
      sdPercentagePoints: number;
    }[] = [];
  // Scenario anchors are distinct from identity correspondences. A whole 2022
  // alliance may inform a labelled proxy, but never becomes invented UV counts.
  const proxies = [
    [
      "valdostano-cdx",
      "02",
      "LEGA PER SALVINI PREM.-FORZA IT.-NOI MODERATI-FRAT. D'ITALIA",
    ],
    ["valdostano-csx", "02", "VALLE D'AOSTA APERTA"],
    [
      "union-valdotaine",
      "02",
      "VALLEE D?AOSTE ? AUTONOMIE PROGRES FEDERALISME",
    ],
    ["svp-patt", "04", "SUDTIROLER VOLKSPARTEI (SVP) - PATT"],
    ["team-k", "04", "TEAM K"],
    ["freiheitlichen", "04", "DIE FREIHEITLICHEN"],
  ];
  for (const [ch, rows] of [
    ["camera", camera],
    ["senato", senato],
  ] as const) {
    for (const region of ["02", "04"]) {
      const selected = rows.filter((r) => units.get(r.unit)!.region === region),
        e = sum(selected.map((r) => r.electors)),
        valid = sum(selected.flatMap((r) => r.choices.map((c) => c.count)));
      nonValidReferences.push({
        id: "baseline-nonvalid:" + ch + ":" + region,
        scope: { kind: "region", code: region },
        channel: ch,
        mean: 1 - valid / e,
        sdPercentagePoints: 0,
      });
      for (const [component, r, label] of proxies.filter(
        (x) => x[1] === region,
      )) {
        if (future.subregionalReferences.some((p) => p.component === component))
          continue;
        const choices = new Set(
          source.payload.choices
            .filter(
              (c) =>
                c.eventId === ch + "-2022" &&
                (normalizeName(c.label) === normalizeName(label) ||
                  (c.members.length === 1 && c.members[0] === component)),
            )
            .map((c) => c.id),
        );
        const count = sum(
          selected.flatMap((row) =>
            row.choices.filter((c) => choices.has(c.id)).map((c) => c.count),
          ),
        );
        // An absent separately identifiable Camera observation may use the
        // same-date Senate group as an explicit anchor, not fabricated counts.
        const other = ch === "camera" ? senato : camera;
        const otherSelected = other.filter(
            (row) => units.get(row.unit)!.region === region,
          ),
          otherIds = new Set(
            source.payload.choices
              .filter(
                (c) =>
                  c.eventId ===
                    (ch === "camera" ? "senato" : "camera") + "-2022" &&
                  normalizeName(c.label) === normalizeName(label),
              )
              .map((c) => c.id),
          );
        const otherCount = sum(
            otherSelected.flatMap((row) =>
              row.choices.filter((c) => otherIds.has(c.id)).map((c) => c.count),
            ),
          ),
          otherValid = sum(
            otherSelected.flatMap((row) => row.choices.map((c) => c.count)),
          );
        const mean =
          count > 0
            ? count / valid
            : otherCount > 0
              ? otherCount / otherValid
              : 0;
        if (mean > 0)
          references.push({
            id: "declared-regional-anchor:" + ch + ":" + component,
            scope: { kind: "region", code: region },
            channel: ch,
            subject: observed.has(component)
              ? {
                  kind: "list",
                  id: future.lists.find(
                    (l) => l.members.length === 1 && l.members[0] === component,
                  )!.id,
                }
              : { kind: "component", id: component },
            mean,
            sdPercentagePoints: 0,
          });
      }
    }
  }
  for (const prior of future.unobservedReferences)
    if (newMembers.includes(prior.component)) {
      const scope = prior.scope as {
        kind: "national" | "region";
        code?: string;
      };
      if (scope.kind === "region" && !scope.code)
        throw new Error("Missing unobserved-reference region");
      references.push({
        id: "declared-unidentified:" + prior.component,
        scope:
          scope.kind === "national"
            ? { kind: "national" }
            : { kind: "region", code: scope.code! },
        channel: "common",
        subject: { kind: "component", id: prior.component },
        mean: prior.mean,
        sdPercentagePoints: prior.sdPercentagePoints,
      });
    }
  for (const prior of future.subregionalReferences) {
    let mean = "mean" in prior ? prior.mean! : prior.fallbackMean!;
    if ("sourceEvent" in prior) {
      const rows = prior.sourceEvent === "senato-2022" ? senato : camera,
        selected = rows.filter(
          (r) => units.get(r.unit)!.province === prior.province,
        ),
        valid = sum(selected.flatMap((r) => r.choices.map((c) => c.count))),
        ids = new Set(
          source.payload.choices
            .filter(
              (c) =>
                c.eventId === prior.sourceEvent &&
                normalizeName(c.label) === normalizeName(prior.sourceLabel!),
            )
            .map((c) => c.id),
        ),
        count = sum(
          selected.flatMap((r) =>
            r.choices.filter((c) => ids.has(c.id)).map((c) => c.count),
          ),
        );
      if (count > 0 && valid > 0) mean = count / valid;
    }
    const subject = newMembers.includes(prior.component)
      ? { kind: "component" as const, id: prior.component }
      : {
          kind: "list" as const,
          id: future.lists.find(
            (l) => l.members.length === 1 && l.members[0] === prior.component,
          )!.id,
        };
    references.push({
      id: "declared-subregional:" + prior.component + ":" + prior.province,
      scope: { kind: "province", code: prior.province },
      channel: "common",
      subject,
      mean,
      sdPercentagePoints: prior.sdPercentagePoints,
    });
  }
  for (const id of newMembers)
    if (!references.some((r) => r.subject.id === id))
      throw new Error("Missing explicit new-component reference: " + id);
  const bonusLinks = [
    ...future.coalitions.map((c) => ({
      camera: { kind: "coalition" as const, id: c.id },
      senato: { kind: "coalition" as const, id: c.id },
      cameraDeclares: true,
      senatoDeclares: true,
    })),
    ...future.lists
      .filter((l) => !("coalition" in l))
      .map((l) => ({
        camera: { kind: "list" as const, id: l.id },
        senato: { kind: "list" as const, id: l.id },
        cameraDeclares: true,
        senatoDeclares: true,
      })),
  ];
  return {
    selection,
    lists,
    coverage,
    bonusLinks,
    listMembers: Object.fromEntries(
      (["camera", "senato"] as const).map((ch) => [
        ch,
        Object.fromEntries(future.lists.map((l) => [l.id, l.members])),
      ]),
    ) as { camera: Record<string, string[]>; senato: Record<string, string[]> },
    electorates: targets.map((t) => ({
      code: t.code,
      date: "2022-09-25",
      camera: t.electors,
      senato: t.electors,
      provenance: policy.electoratePolicy,
    })),
    references,
    nonValidReferences,
    components: [
      ...components.map((id) => ({
        id,
        kind: "fitted" as const,
        provenance:
          "Future component coordinate; observations remain grouped. Unidentified/new coordinates retain prior uncertainty, not empirical identification.",
      })),
      ...newMembers.map((id) => ({
        id,
        kind: "new" as const,
        provenance:
          "Explicit scenario reference; no separately observed compatible fitting support.",
        fallback: policy.newComponentFallback,
      })),
    ],
    assumptions: [
      ...future.identityNotes,
      "Unobserved and subregional reference assumptions are authored explicitly in fast-share-future-scenario.json; they are not polling likelihoods or zero historical counts.",
      "Valdostan references use labelled whole-2022-alliance proxy anchors; UV is not identified with every vote of its former alliance. New territorial support without a separately observed history retains explicit prior uncertainty.",
    ],
    presentationCoalitions: future.coalitions.map((c, i) => ({
      ...c,
      color: i === 0 ? "#2563eb" : "#dc2626",
      chambers: ["camera", "senato"] as ("camera" | "senato")[],
    })),
  };
}
