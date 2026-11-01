/** Agreed 1 November 2026. Historical choice sets are observations, not filing. */
import type { FastGenerationScenario, PhysicalCell } from "./forecast-types";
import type { Chamber } from "../input";
import { scopeMatches } from "./reference-plans";
import { sum } from "./types";

export const COMPONENT_POLICY = "component-prefiling-conditioning-v2";
export interface ComponentObservation {
  code: string;
  region: string;
  electors: number;
  /** Non-valid first; fractional legacy decompositions remain unrounded. */
  counts: number[];
  offered: boolean[];
}

/** MM maximization of sum y_cj log(q_j / q(O_c)). Unobserved coordinates
 * retain donor mass; zero observations remain likelihood information. */
export function poolConditional(
  rows: readonly ComponentObservation[],
  width: number,
  donor?: ArrayLike<number>,
) {
  const observed = Array(width).fill(false) as boolean[],
    totals = new Float64Array(width);
  for (const r of rows) {
    if (
      r.counts.length !== width ||
      r.offered.length !== width ||
      !(r.electors > 0) ||
      r.counts.some(
        (v, j) => !Number.isFinite(v) || v < 0 || (!r.offered[j] && v !== 0),
      ) ||
      Math.abs(sum(r.counts) - r.electors) > 1e-6
    )
      throw new Error("INVALID_COMPONENT_OBSERVATION");
    r.offered.forEach((v, j) => {
      observed[j] ||= v;
      totals[j] += r.counts[j];
    });
  }
  if (!rows.length) {
    if (!donor) throw new Error("MISSING_COMPONENT_HISTORY");
    return {
      probabilities: Float64Array.from(donor),
      observed,
      unidentified: observed.map(() => false),
    };
  }
  const fixed = sum(observed.map((v, j) => (v ? 0 : (donor?.[j] ?? 0)))),
    available = 1 - fixed;
  let q = Float64Array.from(observed, (v, j) =>
    v ? totals[j] : (donor?.[j] ?? 0),
  );
  const total = sum(totals);
  q.forEach((_, j) => {
    if (observed[j]) q[j] = (available * totals[j]) / total;
  });
  // At a boundary MLE, a zero alternative cannot connect otherwise disjoint
  // positive choice sets. Their relative masses are not identified by counts.
  const parent = Array.from({ length: width }, (_, j) => j);
  const root = (j: number): number =>
    parent[j] === j ? j : (parent[j] = root(parent[j]));
  for (const r of rows) {
    const positive = r.offered.flatMap((v, j) =>
      v && totals[j] > 0 ? [j] : [],
    );
    for (const j of positive.slice(1)) parent[root(j)] = root(positive[0]);
  }
  const blocks = new Map<number, number[]>();
  totals.forEach((v, j) => {
    if (v > 0) {
      const key = root(j);
      blocks.set(key, [...(blocks.get(key) ?? []), j]);
    }
  });
  const unidentified = observed.map(
    (v, j) => v && totals[j] > 0 && blocks.size > 1,
  );
  const finish = () => {
    if (donor && blocks.size > 1) {
      const supported = sum(
        [...blocks.values()].flatMap((g) => g.map((j) => donor[j])),
      );
      if (!(supported > 0)) throw new Error("MISSING_CONDITIONAL_POOL_ANCHOR");
      for (const g of blocks.values()) {
        const desired = (available * sum(g.map((j) => donor[j]))) / supported,
          actual = sum(g.map((j) => q[j]));
        for (const j of g) q[j] *= desired / actual;
      }
    }
    return { probabilities: q, observed, unidentified };
  };
  for (let it = 0; it < 10000; it++) {
    const denominators = new Float64Array(width);
    for (const r of rows) {
      const mass = sum(q.map((v, j) => (r.offered[j] ? v : 0)));
      if (!(mass > 0)) throw new Error("EMPTY_HISTORICAL_SUPPORT");
      r.offered.forEach((v, j) => {
        if (v) denominators[j] += r.electors / mass;
      });
    }
    const next = q.map((v, j) =>
      observed[j] ? (totals[j] ? totals[j] / denominators[j] : 0) : v,
    );
    const mass = sum(next.map((v, j) => (observed[j] ? v : 0)));
    next.forEach((_, j) => {
      if (observed[j]) next[j] *= available / mass;
    });
    const error = Math.max(...next.map((v, j) => Math.abs(v - q[j])));
    q = next;
    if (error < 1e-12) return finish();
  }
  throw new Error("CONDITIONAL_POOL_DID_NOT_CONVERGE");
}

export function completeLocal(
  row: ComponentObservation | undefined,
  donor: ArrayLike<number>,
) {
  if (!row) return Float64Array.from(donor);
  const mass = sum(Array.from(donor, (v, j) => (row.offered[j] ? v : 0)));
  if (!(mass > 0)) throw new Error("EMPTY_HISTORICAL_SUPPORT");
  // Zero-only half vote is applied before references, with ORIGINAL electorate.
  return Float64Array.from(donor, (v, j) =>
    row.offered[j]
      ? (mass * (row.counts[j] === 0 && j > 0 ? 0.5 : row.counts[j])) /
        row.electors
      : v,
  );
}

export function requireReferenceCoverage(
  s: FastGenerationScenario,
  cells: Record<Chamber, PhysicalCell[]>,
  required: readonly string[],
) {
  for (const ch of s.renewal === "joint"
    ? (["camera", "senato"] as const)
    : [s.renewal])
    for (const id of required) {
      const filed = new Set(
        s.lists[ch].filter((l) => l.componentIds.includes(id)).map((l) => l.id),
      );
      for (const c of cells[ch]) {
        if (
          !s.coverage.some(
            (r) =>
              r.chamber === ch &&
              r.collegeId === c.collegeId &&
              r.listIds.some((l) => filed.has(l)),
          )
        )
          continue;
        if (
          !s.references.some(
            (r) =>
              r.subject.kind === "component" &&
              r.subject.id === id &&
              (r.channel === "common" || r.channel === ch) &&
              scopeMatches(c, r.scope),
          )
        )
          throw new Error(
            "COMPONENT_REFERENCE_COVERAGE_REQUIRED:" +
              ch +
              "/" +
              id +
              "/" +
              c.id,
          );
      }
    }
}
