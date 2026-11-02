/** Joint-draw scores at observable groups; source truth is used only here. */
import {
  score,
  aggregate,
  quantile,
  type ReplaySource,
} from "./fast-share-replay";
import { sum } from "../../src/lib/politics/reform-2026/vote-share/types";
export function distributionScores(
  values: number[],
  truth: number,
  valid: number,
) {
  const basic = score(values, truth),
    sorted = [...values].sort((a, b) => a - b),
    n = values.length,
    band = Math.max(0.02, 0.5 / Math.sqrt(Math.max(1, valid)));
  let pair = 0;
  for (let i = 0; i < n; i++) pair += (2 * i - n + 1) * sorted[i];
  const coverage = Object.fromEntries(
    [0.5, 0.8, 0.9, 0.95].map((level) => {
      const alpha = 1 - level;
      return [
        String(level),
        Number(
          quantile(sorted, alpha / 2) <= truth &&
            truth <= quantile(sorted, 1 - alpha / 2),
        ),
      ];
    }),
  );
  return {
    ...basic,
    coverage,
    crps: sum(values.map((x) => Math.abs(x - truth))) / n - pair / (n * n),
    extreme80: values.filter((x) => x > 0.8).length / n,
    error30: values.filter((x) => Math.abs(x - truth) > 0.3).length / n,
    error15: values.filter((x) => Math.abs(x - truth) > 0.15).length / n,
    nearTruth: values.filter((x) => Math.abs(x - truth) <= band).length / n,
  };
}
export function summarizeScores(rows: ReturnType<typeof distributionScores>[]) {
  if (!rows.length) return null;
  return {
    ...aggregate(rows),
    coverage: Object.fromEntries(
      [0.5, 0.8, 0.9, 0.95].map((l) => [
        String(l),
        sum(rows.map((r) => r.coverage[String(l)])) / rows.length,
      ]),
    ),
    ...Object.fromEntries(
      ["crps", "extreme80", "error30", "error15", "nearTruth"].map((k) => [
        k,
        sum(rows.map((r) => r[k as "crps"])) / rows.length,
      ]),
    ),
    variables: rows.length,
  };
}
export function summarizeUnits(
  rows: { unit: string; score: ReturnType<typeof distributionScores> }[],
) {
  if (!rows.length) return null;
  const counts = new Map<string, number>();
  for (const r of rows) counts.set(r.unit, (counts.get(r.unit) ?? 0) + 1);
  const mean = (get: (r: ReturnType<typeof distributionScores>) => number) =>
    sum(rows.map((r) => get(r.score) / counts.get(r.unit)!)) / counts.size;
  return {
    ...(Object.fromEntries(
      [
        "coverage90",
        "width90",
        "meanMae",
        "meanBias",
        "wis",
        "crps",
        "extreme80",
        "error30",
        "error15",
        "nearTruth",
      ].map((key) => [key, mean((r) => r[key as "wis"])]),
    ) as Record<
      | "coverage90"
      | "width90"
      | "meanMae"
      | "meanBias"
      | "wis"
      | "crps"
      | "extreme80"
      | "error30"
      | "error15"
      | "nearTruth",
      number
    >),
    coverage: Object.fromEntries(
      [0.5, 0.8, 0.9, 0.95].map((l) => [
        String(l),
        mean((r) => r.coverage[String(l)]),
      ]),
    ),
    variables: rows.length,
    units: counts.size,
  };
}
export interface AreaMapping {
  municipality: string;
  areas: { id: string; weight: number }[];
}
export function scoreAreas(
  source: Pick<ReplaySource, "truth">,
  municipalityIds: string[],
  componentIds: string[],
  draws: Uint32Array[],
  mapping: AreaMapping[],
) {
  const n = componentIds.length + 1,
    pos = new Map(componentIds.map((id, j) => [id, j + 1])),
    locations = new Map(municipalityIds.map((id, i) => [id, i])),
    truth = new Map(source.truth.map((t) => [t.id, t]));
  const areas = new Map<string, { municipality: string; weight: number }[]>();
  for (const entry of mapping)
    for (const area of entry.areas) {
      const entries = areas.get(area.id) ?? [];
      entries.push({ municipality: entry.municipality, weight: area.weight });
      areas.set(area.id, entries);
    }
  const output: {
    area: string;
    members: string[];
    truth: number;
    values: number[];
    score: ReturnType<typeof distributionScores>;
  }[] = [];
  for (const [area, entries] of areas) {
    // Coarsest common observable partition. Overlapping labels never become
    // independently observed intra-list component shares.
    const parent = new Map(componentIds.map((id) => [id, id]));
    const root = (id: string): string =>
      parent.get(id) === id ? id : root(parent.get(id)!);
    for (const e of entries)
      for (const group of truth.get(e.municipality)!.groups)
        for (const id of group.slice(1)) parent.set(root(id), root(group[0]));
    const groups = new Map<string, string[]>();
    for (const id of componentIds) {
      const key = root(id),
        group = groups.get(key) ?? [];
      group.push(id);
      groups.set(key, group);
    }
    const valid = sum(
      entries.map((e) => {
        const t = truth.get(e.municipality)!;
        return e.weight * (t.electors - t.nonValid);
      }),
    );
    for (const members of groups.values()) {
      const actual = sum(
        entries.map((e) => {
          const t = truth.get(e.municipality)!;
          return (
            e.weight *
            sum(
              t.groups.map((g, j) =>
                g.every((id) => members.includes(id)) ? t.counts[j] : 0,
              ),
            )
          );
        }),
      );
      // Unfiled groups have no observed choice in this area's ballots.
      if (
        !entries.some((e) =>
          truth
            .get(e.municipality)!
            .groups.some((g) => g.some((id) => members.includes(id))),
        )
      )
        continue;
      const values = draws.map((draw) => {
        let votes = 0,
          v = 0;
        for (const e of entries) {
          const i = locations.get(e.municipality)!,
            t = truth.get(e.municipality)!;
          v += e.weight * (t.electors - draw[i * n]);
          for (const id of members)
            votes += e.weight * draw[i * n + pos.get(id)!];
        }
        return v ? votes / v : 0;
      });
      output.push({
        area,
        members,
        truth: actual / valid,
        values,
        score: distributionScores(values, actual / valid, valid),
      });
    }
  }
  return {
    areas: areas.size,
    rows: output,
    scores: summarizeUnits(
      output.map((r) => ({ unit: r.area, score: r.score })),
    ),
  };
}
