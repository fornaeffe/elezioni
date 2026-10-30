/** Research-only user scenario adapter. Never imports fitted candidate profiles. */
import { createHash } from "node:crypto";
import { membershipFor } from "../data-preparation/party-drift-memberships";
import { preparePairedVoteReferences } from "../../src/lib/politics/reform-2026/paired-vote-generation";
import {
  apportion,
  generationRng,
} from "../../src/lib/politics/reform-2026/generation-math";
import type {
  ReformGenerationScenario,
  ReformGenerationSnapshot,
} from "../../src/lib/politics/reform-2026/generation-types";
import policy from "../../../data/legacy-comparison-policy.json" with { type: "json" };
export const digest = (x: unknown) =>
  createHash("sha256").update(JSON.stringify(x)).digest("hex");
export function links(
  event: string,
  label: string,
  members: string[],
  lists: { id: string; members: string[] }[],
) {
  const weights = (
    policy.jointWeights as Record<string, Record<string, number>>
  )[event + ":" + label];
  return lists
    .map((l) => ({
      id: l.id,
      factor: members
        .filter((m) => l.members.includes(m))
        .reduce((n, m) => n + (weights ? (weights[m] ?? 0) : 1), 0),
    }))
    .filter((l) => l.factor > 0);
}
export function eligiblePolls(input: any, cutoff: string) {
  return [
    ...input.polls,
    ...(input.supplementalPolls ?? policy.supplementalPolls).map((p: any) => ({
      ...p,
      end: p.date,
      kind: "camera",
      sample: p.sample ?? 300,
    })),
  ]
    .filter(
      (p) =>
        p.kind === "camera" &&
        p.available <= cutoff &&
        p.end <= cutoff &&
        p.date.slice(0, 10) <= cutoff,
    )
    .sort(
      (a, b) =>
        b.available.localeCompare(a.available) ||
        b.end.localeCompare(a.end) ||
        a.id.localeCompare(b.id),
    );
}
export function pollPoints(
  input: any,
  cutoff: string,
  lists: { id: string; members: string[] }[],
) {
  const points = new Map<
    string,
    {
      share: number;
      date: string;
      available: string;
      id: string;
      source: string;
      sample: number;
    }
  >();
  for (const p of eligiblePolls(input, cutoff)) {
    const local = new Map<string, number>();
    p.groups.forEach((g: string[], i: number) => {
      const targets = links("", "", g, lists),
        total = targets.reduce((n, t) => n + t.factor, 0);
      for (const t of targets)
        local.set(
          t.id,
          (local.get(t.id) ?? 0) + (p.shares[i] * t.factor) / total,
        );
    });
    for (const [id, share] of local)
      if (!points.has(id))
        points.set(id, {
          share,
          date: p.end,
          available: p.available,
          id: p.id,
          source: p.source,
          sample: p.sample ?? 300,
        });
  }
  return points;
}
export function addReferences(
  s: ReformGenerationScenario,
  snapshot: ReformGenerationSnapshot,
  input: any,
  cutoff: string,
  lists: { id: string; members: string[] }[],
  variant: string,
) {
  const points = pollPoints(input, cutoff, lists),
    active =
      s.renewal === "joint"
        ? (["camera", "senato"] as const)
        : ([s.renewal] as const);
  const audit: any = {
    variant,
    cutoff,
    points: [...points].map(([listId, p]) => ({ listId, ...p })),
    newLists: [],
    unsupported: [],
    warnings: [],
  };
  // Ordinary users supply a common reference date. Freeze it to information,
  // not the requested cutoff, so identical effective inputs share a cache key.
  const fresh = lists.filter(
    (l) =>
      !s.correspondences.some(
        (c) => active.includes(c.chamber as never) && c.futureListId === l.id,
      ),
  );
  for (const l of fresh) {
    const p = points.get(l.id);
    if (!p || p.share <= 0) {
      audit.unsupported.push(l.id);
      continue;
    }
    s.shares.push({
      scope: { kind: "national" },
      listId: l.id,
      common: null,
      camera: p.share,
      senato: s.renewal === "joint" ? p.share : null,
    });
    audit.newLists.push({ listId: l.id, ...p });
  }
  s.projectionDate =
    audit.newLists
      .map((p: any) => p.date)
      .sort()
      .at(-1) ?? snapshot.historicalDate;
  if (variant === "polls") {
    const latest = eligiblePolls(input, cutoff)[0]?.available;
    // Refresh established parties from the latest publication, rather than
    // combining obsolete minor-party polls with a new major-party wave.
    for (const [id, p] of points)
      if (p.available !== latest && !fresh.some((l) => l.id === id))
        points.delete(id);
    const baseline = preparePairedVoteReferences(s, snapshot).units;
    const date =
      [...points.values()]
        .map((p) => p.date)
        .sort()
        .at(-1) ?? snapshot.historicalDate;
    const rows: any[] = [];
    for (const ch of active) {
      const ordinary = new Set(
        snapshot.seats.geography.colleges
          .filter((c) => c.chamber === ch && c.kind === "ordinary")
          .map((c) => c.id),
      );
      const codes = new Set(
        snapshot.memberships
          .filter((m) => m.chamber === ch && ordinary.has(m.collegeId))
          .map((m) => m.municipalityCode),
      );
      const units = baseline.filter((u) => codes.has(u.municipality.code));
      const total = units.reduce(
        (n, u) => n + u.municipality[ch].electors * (1 - u[ch].abstention),
        0,
      );
      const base = new Map(
        lists.map((l) => [
          l.id,
          units.reduce(
            (n, u) =>
              n +
              u.municipality[ch].electors *
                (1 - u[ch].abstention) *
                (u[ch].shares.get(l.id) ?? 0),
            0,
          ) / (total || 1),
        ]),
      );
      const offered = new Set(
          s.coverage
            .filter((c) => c.chamber === ch && ordinary.has(c.collegeId))
            .flatMap((c) => c.listIds),
        ),
        ordinaryLists = lists.filter((l) => offered.has(l.id));
      const known = ordinaryLists.filter(
        (l) => points.has(l.id) && points.get(l.id)!.share > 0,
      );
      const mass = known.reduce((n, l) => n + points.get(l.id)!.share, 0),
        free = ordinaryLists.filter((l) => !known.includes(l)),
        remainder = free.reduce((n, l) => n + base.get(l.id)!, 0);
      if (!known.length) {
        audit.warnings.push("No named poll refresh for " + ch);
        continue;
      }
      const scale = mass >= 1 ? (1 - remainder) / mass : 1,
        remaining = 1 - mass * scale;
      if (scale !== 1)
        audit.warnings.push(
          "Overcomplete mixed-wave references normalized; historical unreported mass retained: " +
            ch +
            "/" +
            mass,
        );
      for (const l of ordinaryLists) {
        const p = points.get(l.id),
          share = p
            ? p.share * scale
            : remainder
              ? (base.get(l.id)! * remaining) / remainder
              : 0;
        rows.push({
          chamber: ch,
          listId: l.id,
          share,
          sigma: p
            ? Math.hypot(
                Math.sqrt((p.share * (1 - p.share)) / p.sample) /
                  Math.max(0.01, p.share * (1 - p.share)),
                0.08,
              )
            : 0,
          donorHistoricalListIds: [],
        });
      }
      // If all lists have poll points, normalize the explicit offered set.
      const r = rows.filter((r) => r.chamber === ch),
        sum = r.reduce((n, r) => n + r.share, 0);
      for (const row of r) row.share /= sum;
    }
    if (rows.length) {
      s.pollReferences = {
        version: policy.version,
        archiveVersion: digest(audit.points),
        policyVersion: "poll-reference-v1",
        date,
        sourceQuestionIds: [...new Set([...points.values()].map((p) => p.id))],
        assumptions: [
          "Latest reviewed named observations; mixed waves and residual baseline allocation are user assumptions, not fitted estimates.",
        ],
        rows,
      };
      s.schemaVersion = 3;
      // Positive new-list points seed the baseline above. The poll path owns
      // their final references and preserves territorial age/measurement error.
      s.shares = [];
      s.projectionDate = date;
    }
  }
  audit.effectiveKey = digest({
    policy: policy.version,
    snapshot,
    scenario: s,
  });
  return audit;
}
export function historicalInput(
  input: any,
  targetId: string,
  cutoff: string,
  template: ReformGenerationScenario,
  base: ReformGenerationSnapshot,
  variant: string,
) {
  const target = input.history.events.find((e: any) => e.id === targetId),
    history = input.history.events
      .filter((e: any) => e.kind === "camera" && e.date < cutoff)
      .at(-1);
  const labels = [
    ...new Set<string>(
      target.municipalities
        .filter((m: any) => m.regionId !== "02")
        .flatMap((m: any) => m.lists.map((l: any) => l.name)),
    ),
  ].sort();
  const lists = labels.map((name) => ({
    id: digest(name).slice(0, 16),
    name,
    members: membershipFor(input.policy, targetId, name).members,
  }));
  const past = [
    ...new Set<string>(
      history.municipalities.flatMap((m: any) =>
        m.lists.map((l: any) => l.name),
      ),
    ),
  ].sort();
  const pastId = new Map(past.map((name) => [name, digest(name).slice(0, 16)]));
  const correspondences = past.flatMap((name) =>
    links(
      history.id,
      name,
      membershipFor(input.policy, history.id, name).members,
      lists,
    ).map((l) => ({
      chamber: "camera" as const,
      pastListId: pastId.get(name)!,
      futureListId: l.id,
      factor: l.factor,
    })),
  );
  const usableHistory = history.municipalities.filter(
    (m: any) => m.matchable && m.electors > 0 && m.validVotes <= m.electors,
  );
  const previous = new Map(usableHistory.map((m: any) => [m.id, m]));
  const region = new Map<string, any>();
  for (const m of usableHistory) {
    const r = region.get(m.regionId) ?? { electors: 0, votes: new Map() };
    r.electors += m.electors;
    for (const l of m.lists)
      r.votes.set(l.name, (r.votes.get(l.name) ?? 0) + l.votes);
    region.set(m.regionId, r);
  }
  const fallback: string[] = [],
    coverage = new Map<string, string[]>(),
    memberships: any[] = [];
  const municipalities = target.municipalities
    .filter(
      (m: any) =>
        m.regionId !== "02" &&
        m.matchable &&
        m.electors > 0 &&
        m.validVotes <= m.electors,
    )
    .map((m: any) => {
      let h: any = previous.get(m.id),
        votes;
      if (h)
        votes = h.lists.map((l: any) => ({
          listId: pastId.get(l.name)!,
          votes: l.votes,
        }));
      else {
        fallback.push(m.id);
        const r = region.get(m.regionId);
        if (!r) throw new Error("No eligible regional history for " + m.id);
        h = { electors: r.electors };
        votes = [...r.votes].map(([name, v]: any) => ({
          listId: pastId.get(name)!,
          votes: v,
        }));
      }
      // Conditional target electorates, as in candidate tests. Scale probabilities
      // without inventing zero history for mergers/new municipality names.
      const counts = apportion(
        m.electors,
        [
          ...votes.map((l: any) => l.votes),
          h.electors - votes.reduce((n: number, l: any) => n + l.votes, 0),
        ],
        generationRng(policy.seed, 0, "historical-rescale", m.id),
      );
      const scaled = votes.map((l: any, i: number) => ({
        ...l,
        votes: counts[i],
      }));
      const filed = m.lists.map((l: any) => digest(l.name).slice(0, 16)).sort(),
        college =
          (m.regionId === "04" ? "taa-" : "filing-") +
          digest(filed).slice(0, 12);
      coverage.set(college, filed);
      memberships.push({
        chamber: "camera",
        municipalityCode: m.id,
        collegeId: college,
        population: 1,
      });
      const row = { electors: m.electors, votes: scaled };
      return {
        code: m.id,
        name: m.name,
        regionId: m.regionId,
        provinceId: m.provinceId ?? m.regionId,
        camera: row,
        senato: row,
      };
    });
  const snapshot = {
    ...base,
    version:
      "legacy-fold-" +
      digest({ targetId, history: history.sourceSha256 }).slice(0, 16),
    historicalDate: history.date,
    historicalLists: past.map((name) => ({
      id: pastId.get(name)!,
      name,
      kind: "list" as const,
    })),
    historicalCoalitions: [],
    municipalities,
    memberships,
    seats: {
      ...base.seats,
      geography: {
        ...base.seats.geography,
        colleges: [...coverage.keys()].map((id) => ({
          id,
          chamber: "camera",
          kind: id.startsWith("taa-") ? "special-uninominal" : "ordinary",
          territoryId: id,
        })),
      },
    },
  } as unknown as ReformGenerationSnapshot;
  const scenario = {
    ...structuredClone(template),
    renewal: "camera",
    seed: policy.seed + ":" + targetId,
    electionDate: target.date,
    snapshotVersion: snapshot.version,
    lists: {
      camera: lists.map((l) => ({
        id: l.id,
        name: l.name,
        coalitionId: null,
        protectedMinority: null,
      })),
      senato: [],
    },
    coverage: [...coverage].map(([collegeId, listIds]) => ({
      chamber: "camera",
      collegeId,
      listIds,
    })),
    correspondences,
    shares: [],
    abstention: [],
    pollReferences: undefined,
  } as ReformGenerationScenario;
  const audit = addReferences(
    scenario,
    snapshot,
    input,
    cutoff,
    lists,
    variant,
  );
  const unrefreshed = {
    ...scenario,
    shares: [],
    abstention: [],
    pollReferences: undefined,
  };
  const projected = preparePairedVoteReferences(unrefreshed, snapshot).units;
  const originalValid = municipalities.reduce(
      (n: number, m: any) =>
        n + m.camera.votes.reduce((n: number, l: any) => n + l.votes, 0),
      0,
    ),
    retainedValid = projected.reduce(
      (n, u) => n + u.municipality.camera.electors * (1 - u.camera.abstention),
      0,
    );
  return {
    snapshot,
    scenario,
    lists,
    audit: {
      ...audit,
      targetId,
      baseline: history.id,
      fallback,
      originalValid,
      retainedValid,
      unmappedOrUnfiledValid: originalValid - retainedValid,
      correspondences: past.map((name) => ({
        name,
        members: membershipFor(input.policy, history.id, name).members,
        links: links(
          history.id,
          name,
          membershipFor(input.policy, history.id, name).members,
          lists,
        ),
      })),
    },
  };
}
