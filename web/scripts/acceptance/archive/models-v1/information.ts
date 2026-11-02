import { identificationGroups, originalPartitions } from "./identification";
import {
  conjugateGradient,
  denseFromOperator,
  factorSpd,
  helmert,
  solveSpd,
  type LinearOperator,
  type SpdFactor,
} from "../../../../src/lib/politics/reform-2026/vote-share/linalg";
import { gaussianMode, fitGaussianSteps } from "../../../../src/lib/politics/reform-2026/vote-share/gaussian";
import { completeSteps, type FitProgress } from "../../../../src/lib/politics/reform-2026/vote-share/execution";
import {
  clip,
  day,
  median,
  requireWorkspace,
  sum,
  years,
  type FitContext,
  type GaussianTarget,
  type InformationPoll,
  type NationalElection,
  type NationalSource,
  type SolverOptions,
  type Vector,
} from "./types";

export function normalizeSource(source: NationalSource): NationalSource {
  const components = [...source.components].sort();
  if (
    !components.length ||
    components.length > 128 ||
    new Set(components).size !== components.length ||
    components.some((x) => !x)
  )
    throw new RangeError("Invalid fitted component basis (maximum 128)");
  day(source.referenceDate);
  if (source.elections.length > 64)
    throw new RangeError(
      "Unsupported historical envelope (maximum 64 national events)",
    );
  if (source.polls.length > 512)
    throw new RangeError("Unsupported poll envelope (maximum 512)");
  const known = new Set(components),
    eventIds = new Set<string>(),
    contextDates = new Set<string>(),
    surveyIds = new Set<string>();
  const check = (
    id: string,
    date: string,
    context: FitContext,
    groups: string[][],
  ) => {
    if (!id || eventIds.has(id))
      throw new RangeError("Duplicate/empty observation ID");
    eventIds.add(id);
    if (!["camera", "european"].includes(context))
      throw new RangeError(
        "Unsupported numerical context; Senate needs its separate adapter",
      );
    if (day(date) > day(source.referenceDate))
      throw new RangeError("Observation is after reference cutoff");
    const members = groups.flat();
    if (
      !groups.length ||
      groups.some((g) => !g.length) ||
      new Set(members).size !== members.length ||
      members.some((x) => !known.has(x))
    )
      throw new RangeError("Invalid grouped offered set");
    return groups
      .map((g, i) => ({ members: [...g].sort(), i }))
      .sort((a, b) =>
        JSON.stringify(a.members) < JSON.stringify(b.members)
          ? -1
          : JSON.stringify(a.members) > JSON.stringify(b.members)
            ? 1
            : 0,
      );
  };
  const elections = source.elections
    .map((e) => {
      const sorted = check(e.id, e.date, e.context, e.groups);
      for (const r of e.strata ?? []) {
        const ids = r.groups.flat();
        if (
          !Number.isSafeInteger(r.electors) ||
          r.electors <= 0 ||
          r.counts.length !== r.groups.length + 1 ||
          r.counts.some((v) => !Number.isSafeInteger(v) || v < 0) ||
          sum(r.counts) !== r.electors ||
          r.groups.some((g) => !g.length) ||
          new Set(ids).size !== ids.length ||
          ids.some((id) => !known.has(id))
        )
          throw new RangeError("Invalid original historical stratum");
      }
      if (contextDates.has(e.context + e.date))
        throw new RangeError("Duplicate national context/date");
      contextDates.add(e.context + e.date);
      if (
        !Number.isSafeInteger(e.electors) ||
        e.electors <= 0 ||
        e.counts.length !== e.groups.length + 1 ||
        e.counts.some((n) => !Number.isSafeInteger(n) || n < 0) ||
        sum(e.counts) !== e.electors ||
        e.counts[0] <= 0 ||
        e.counts[0] >= e.electors
      )
        throw new RangeError("Invalid election elector/group counts");
      return {
        ...e,
        groups: sorted.map((g) => g.members),
        counts: [e.counts[0], ...sorted.map((g) => e.counts[g.i + 1])],
      };
    })
    .sort((a, b) => a.date.localeCompare(b.date) || a.id.localeCompare(b.id));
  const polls = source.polls
    .map((p) => {
      const sorted = check(p.id, p.date, p.context, p.groups);
      const survey = p.surveyId ?? p.id;
      if (!survey || surveyIds.has(survey))
        throw new RangeError("Duplicate survey evidence");
      surveyIds.add(survey);
      if (
        !(p.sample > 0) ||
        !Number.isFinite(p.sample) ||
        !p.errorGroup ||
        p.shares.length !== p.groups.length ||
        p.shares.some((x) => !(x > 0) || !Number.isFinite(x)) ||
        Math.abs(sum(p.shares) - 1) > 1e-10 ||
        day(p.availableDate) > day(source.referenceDate) ||
        day(p.fieldworkEnd) > day(source.referenceDate) ||
        day(p.date) > day(p.fieldworkEnd)
      )
        throw new RangeError(
          "Invalid poll sample/shares/availability (printed zeros unsupported)",
        );
      return {
        ...p,
        groups: sorted.map((g) => g.members),
        shares: sorted.map((g) => p.shares[g.i]),
      };
    })
    .sort((a, b) => a.date.localeCompare(b.date) || a.id.localeCompare(b.id));
  if (!elections.length)
    throw new RangeError("A prepared historical baseline is required");
  return {
    components,
    referenceDate: source.referenceDate,
    elections,
    polls,
    ...(source.referenceRequired?.length
      ? {
          referenceRequired: source.referenceRequired,
          referenceDirections: source.referenceDirections ?? [],
        }
      : {}),
  };
}
export function estimateScales(source: NationalSource) {
  const political: number[] = [],
    participation: number[] = [];
  const transitions: {
    from: string;
    to: string;
    politicalPairs: number;
    politicalRate: number | null;
  }[] = [];
  for (const kind of ["camera", "european"]) {
    const records = source.elections.filter((e) => e.context === kind);
    for (let i = 1; i < records.length; i++) {
      const before = records[i - 1],
        after = records[i],
        dt = years(after.date, before.date);
      if (!(dt > 0)) throw new RangeError("Nonpositive election transition");
      const named = (e: NationalElection) => {
        const v = sum(e.counts) - e.counts[0],
          result = new Map<string, number>();
        e.groups.forEach((g, j) => {
          if (g.length === 1 && e.counts[j + 1] / v >= 0.005)
            result.set(
              g[0],
              (e.counts[j + 1] + 0.5) / (v + 0.5 * e.groups.length),
            );
        });
        return result;
      };
      const a = named(before),
        b = named(after),
        common = [...a.keys()].filter((id) => b.has(id)).sort(),
        rates: number[] = [];
      for (let j = 0; j < common.length; j++)
        for (let h = 0; h < j; h++)
          rates.push(
            (Math.log(b.get(common[j])! / b.get(common[h])!) -
              Math.log(a.get(common[j])! / a.get(common[h])!)) **
              2 /
              (2 * dt),
          );
      if (rates.length) political.push(median(rates));
      const p = before.counts[0] / before.electors,
        q = after.counts[0] / after.electors;
      participation.push(
        (Math.log(q / (1 - q)) - Math.log(p / (1 - p))) ** 2 / dt,
      );
      transitions.push({
        from: before.id,
        to: after.id,
        politicalPairs: rates.length,
        politicalRate: rates.length ? median(rates) : null,
      });
    }
  }
  return {
    political: clip(
      Math.sqrt((4 * 0.35 ** 2 + sum(political)) / (4 + political.length)),
      0.12,
      0.7,
    ),
    participation: clip(
      Math.sqrt(
        (4 * 0.15 ** 2 + sum(participation)) / (4 + participation.length),
      ),
      0.05,
      0.5,
    ),
    transitions,
  };
}
function initialReference(source: NationalSource): Vector {
  let q = new Float64Array(source.components.length).fill(
    1 / source.components.length,
  );
  const indexes = new Map(source.components.map((id, i) => [id, i]));
  for (const e of source.elections) {
    const next = q.slice(),
      v = e.electors - e.counts[0];
    e.groups.forEach((g, h) => {
      const js = g.map((id) => indexes.get(id)!),
        old = sum(js.map((j) => q[j]));
      for (const j of js)
        next[j] =
          (((e.counts[h + 1] + 0.5) / (v + 0.5 * e.groups.length)) * q[j]) /
          old;
    });
    const total = sum(next);
    q = next.map((x) => x / total);
  }
  q = q.map((x) => Math.max(1e-4, x));
  const total = sum(q);
  return q.map((x) => x / total);
}
export function estimateDiscrepancy(source: NationalSource, sigma: number) {
  const events: {
    election: string;
    poll: string;
    gapYears: number;
    variance: number;
    contrasts: number;
  }[] = [];
  for (const e of source.elections) {
    const candidates = source.polls.filter(
      (p) =>
        p.context === e.context &&
        p.availableDate <= e.date &&
        p.fieldworkEnd <= e.date &&
        years(e.date, p.date) >= 0 &&
        years(e.date, p.date) <= 45 / 365.25,
    );
    const p = candidates.at(-1);
    if (!p) continue;
    const parent = new Map(source.components.map((id) => [id, id]));
    const root = (id: string): string => {
      let x = id;
      while (parent.get(x) !== x) x = parent.get(x)!;
      return x;
    };
    for (const g of [...e.groups, ...p.groups])
      for (const member of g.slice(1)) parent.set(root(member), root(g[0]));
    const actual = new Map<string, number>(),
      reported = new Map<string, number>();
    e.groups.forEach((g, j) =>
      actual.set(root(g[0]), (actual.get(root(g[0])) ?? 0) + e.counts[j + 1]),
    );
    p.groups.forEach((g, j) =>
      reported.set(root(g[0]), (reported.get(root(g[0])) ?? 0) + p.shares[j]),
    );
    const common = [...actual.keys()].filter((id) => reported.has(id)),
      ta = sum(common.map((id) => actual.get(id)!)),
      tr = sum(common.map((id) => reported.get(id)!));
    if (!(ta > 0 && tr > 0)) continue;
    const contrasts: number[] = [],
      gap = years(e.date, p.date);
    for (let i = 0; i < common.length; i++)
      for (let j = 0; j < i; j++) {
        const a = actual.get(common[i])! / ta,
          b = actual.get(common[j])! / ta,
          r = reported.get(common[i])! / tr,
          u = reported.get(common[j])! / tr;
        if (Math.min(a, b, r, u) < 0.005) continue;
        contrasts.push(
          Math.max(
            0,
            (Math.log(a / b) - Math.log(r / u)) ** 2 / 2 -
              (1 / r + 1 / u) / (2 * p.sample) -
              sigma ** 2 * gap -
              0.12 ** 2,
          ),
        );
      }
    if (contrasts.length)
      events.push({
        election: e.id,
        poll: p.id,
        gapYears: gap,
        variance: median(contrasts),
        contrasts: contrasts.length,
      });
  }
  return {
    scale: clip(
      Math.sqrt(
        (4 * 0.25 ** 2 + sum(events.map((e) => e.variance))) /
          (4 + events.length),
      ),
      0.1,
      0.5,
    ),
    events,
  };
}
interface DesignRecord {
  id: string;
  context: number;
  age: number;
  period: string | null;
  offset: number;
  rows: number;
  design: Vector;
  noise: Vector;
  available: Vector;
  groups: number;
  helmert: Vector;
  groupIndex: Int32Array;
  observedContrast: Vector;
  regularizedNoise?: boolean;
}
function buildDesigns(source: NationalSource): DesignRecord[] {
  const k = source.components.length,
    index = new Map(source.components.map((id, i) => [id, i]));
  const records = [
    ...source.elections
      .flatMap(
        (e) =>
          e.strata?.map((r, i) => ({
            ...e,
            ...r,
            id: e.id + ":stratum:" + i,
          })) ?? [e],
      )
      .map((e) => {
        const n = e.electors - e.counts[0];
        return {
          ...e,
          sample: n,
          period: null,
          shares: e.counts.slice(1).map((x) => x / n),
          varianceShares: e.counts
            .slice(1)
            .map((x) => (x + 0.5) / (n + 0.5 * e.groups.length)),
          rounding: 0,
        };
      }),
    ...source.polls.map((p) => ({
      ...p,
      period: p.errorGroup,
      varianceShares: p.shares,
      rounding: 0.001,
    })),
  ];
  let offset = 0;
  return records
    .filter((r) => r.groups.length > 1)
    .map((record) => {
      const g = record.groups.length,
        r = g - 1,
        h = helmert(g),
        design = new Float64Array(r * k),
        noise = new Float64Array(r * r);
      const offered = new Int32Array(k).fill(-1),
        available = new Float64Array(k);
      record.groups.forEach((members, i) =>
        members.forEach((id) => {
          offered[index.get(id)!] = i;
          available[index.get(id)!] = 1;
        }),
      );
      const hr = new Float64Array(r),
        hv = new Float64Array(r);
      for (let i = 0; i < r; i++)
        for (let j = 0; j < g; j++) {
          hr[i] += h[i * g + j] * record.shares[j];
          hv[i] += h[i * g + j] * record.varianceShares[j];
        }
      for (let i = 0; i < r; i++) {
        for (let j = 0; j < k; j++)
          if (offered[j] >= 0)
            design[i * k + j] = h[i * g + offered[j]] - hr[i];
        for (let j = 0; j < r; j++) {
          let value = -hv[i] * hv[j];
          for (let a = 0; a < g; a++)
            value += h[i * g + a] * h[j * g + a] * record.varianceShares[a];
          noise[i * r + j] =
            value / record.sample + (i === j ? record.rounding ** 2 / 12 : 0);
        }
      }
      const result = {
        id: record.id,
        context: Number(record.context === "european"),
        age: Math.max(0, years(source.referenceDate, record.date)),
        period: record.period,
        offset,
        rows: r,
        design,
        noise,
        available,
        groups: g,
        helmert: h,
        groupIndex: offered,
        observedContrast: hr,
      };
      offset += r;
      return result;
    });
}
/** Exact Gaussian sufficient statistics, not poll deduplication. Repeated full-
 * ballot partitions with the same date/context/error group have identical
 * centred movement/bias rows. Integrate their independent sampling noise before
 * solving the shared covariance. Retain within-group residual information too:
 * A'C^-1 A = sum (Ai-Abar)'Ni^-1(Ai-Abar) + Abar'Cbar^-1 Abar.
 * Here Ai-Abar is rank one along the context's sum coordinate. No observation,
 * effective sample, correlation, rounding variance or original provenance is lost.
 * Partial offered sets stay separate because their centred designs can differ. */
function collapsePollRecords(input: DesignRecord[], k: number) {
  const groups = new Map<string, DesignRecord[]>();
  for (const r of input) {
    const full = r.period !== null && r.available.every((x) => x === 1),
      key = full
        ? JSON.stringify([r.context, r.age, r.period, [...r.groupIndex]])
        : "single:" + r.id;
    groups.set(key, [...(groups.get(key) ?? []), r]);
  }
  const records: DesignRecord[] = [],
    correction = [0, 0];
  let offset = 0;
  for (const group of groups.values()) {
    const first = group[0],
      n = first.rows;
    if (group.length === 1) {
      records.push({ ...first, offset });
      offset += n;
      continue;
    }
    const precision = new Float64Array(n * n),
      linear = new Float64Array(n),
      factors: SpdFactor[] = [];
    for (const r of group) {
      const noise = r.noise.slice();
      for (let i = 0; i < n; i++) noise[i * n + i] += 1e-16;
      const factor = factorSpd(noise, n);
      factors.push(factor);
      const solved = solveSpd(factor, r.observedContrast);
      for (let i = 0; i < n; i++) linear[i] += solved[i];
      for (let j = 0; j < n; j++) {
        const unit = new Float64Array(n);
        unit[j] = 1;
        const column = solveSpd(factor, unit);
        for (let i = 0; i < n; i++) precision[i * n + j] += column[i];
      }
    }
    for (let i = 0; i < n; i++)
      for (let j = 0; j < i; j++)
        precision[i * n + j] = precision[j * n + i] =
          (precision[i * n + j] + precision[j * n + i]) / 2;
    const factor = factorSpd(precision, n),
      mean = solveSpd(factor, linear),
      noise = new Float64Array(n * n);
    for (let j = 0; j < n; j++) {
      const unit = new Float64Array(n);
      unit[j] = 1;
      const column = solveSpd(factor, unit);
      for (let i = 0; i < n; i++) noise[i * n + j] = column[i];
    }
    for (let i = 0; i < n; i++)
      for (let j = 0; j < i; j++)
        noise[i * n + j] = noise[j * n + i] =
          (noise[i * n + j] + noise[j * n + i]) / 2;
    const design = new Float64Array(n * k);
    for (let i = 0; i < n; i++)
      for (let j = 0; j < k; j++)
        design[i * k + j] =
          first.helmert[i * first.groups + first.groupIndex[j]] - mean[i];
    group.forEach((r, h) => {
      const delta = Float64Array.from(
          mean,
          (x, i) => x - r.observedContrast[i],
        ),
        solved = solveSpd(factors[h], delta);
      for (let i = 0; i < n; i++)
        correction[first.context] += delta[i] * solved[i];
    });
    records.push({
      ...first,
      offset,
      design,
      noise,
      observedContrast: mean,
      regularizedNoise: true,
    });
    offset += n;
  }
  return { records, correction };
}
/** Exact covariance action using prefix/suffix Brownian sums and shared groups.
 * Work is O(contrasts * components), not a dense contrasts-squared matrix. */
export function informationCovariance(
  records: DesignRecord[],
  reference: Vector,
  historical: Vector,
  sigma: number,
) {
  const k = reference.length,
    n = records.reduce((s, r) => s + r.rows, 0),
    periods = [
      ...new Set(records.flatMap((r) => (r.period === null ? [] : [r.period]))),
    ].sort();
  const periodIndex = records.map((r) =>
    r.period === null ? -1 : periods.indexOf(r.period),
  );
  const anchor = new Float64Array(n),
    scale = reference.map((x, j) => Math.max(x, historical[j]));
  const noises: Vector[] = [],
    factors: SpdFactor[] = [];
  for (const record of records) {
    let mass = 0;
    for (let j = 0; j < k; j++) mass += record.available[j] * reference[j];
    mass = Math.max(1e-5, mass);
    const noise = record.regularizedNoise
        ? record.noise.slice()
        : record.noise.map((x) => x * mass * mass),
      diagonal = noise.slice();
    const movement = new Float64Array(record.rows * k),
      bias = new Float64Array(record.rows * k);
    for (let i = 0; i < record.rows; i++) {
      let aq = 0;
      for (let j = 0; j < k; j++) aq += record.design[i * k + j] * reference[j];
      for (let j = 0; j < k; j++) {
        const pos = i * k + j,
          centred = record.design[i * k + j] - aq;
        movement[pos] = centred * scale[j];
        bias[pos] = centred * reference[j];
      }
      anchor[record.offset + i] = aq;
      if (!record.regularizedNoise) {
        noise[i * record.rows + i] += 1e-16;
        diagonal[i * record.rows + i] += 1e-16;
      }
    }
    for (let i = 0; i < record.rows; i++)
      for (let j = 0; j < record.rows; j++) {
        let value = 0;
        for (let a = 0; a < k; a++) {
          value +=
            sigma ** 2 * record.age * movement[i * k + a] * movement[j * k + a];
          if (record.period !== null)
            value += 0.12 ** 2 * bias[i * k + a] * bias[j * k + a];
        }
        diagonal[i * record.rows + j] += value;
      }
    noises.push(noise);
    factors.push(factorSpd(diagonal, record.rows));
  }
  const ordered = records
    .map((r, i) => ({ r, i }))
    .sort((a, b) => a.r.age - b.r.age || a.r.id.localeCompare(b.r.id));
  const rw = new Float64Array(records.length * k),
    group = new Float64Array(periods.length * k),
    suffix = new Float64Array(k),
    prefix = new Float64Array(k);
  const transformed = new Float64Array(k + 1),
    grouped = new Float64Array(k + 1),
    latent = new Float64Array(k);
  // A = H(S - r*u). Apply A*diag(s) - (A*q)*s' through the incidence
  // partition, O(K+G^2) per record instead of O(K*G). No covariance is dropped.
  const response = (
    r: DesignRecord,
    weights: Vector,
    input: Vector,
    inputOffset: number,
    multiplier: number,
    out: Vector,
  ) => {
    grouped.fill(0);
    let full = 0,
      offered = 0;
    for (let j = 0; j < k; j++) {
      const value = weights[j] * input[inputOffset + j];
      full += value;
      if (r.groupIndex[j] >= 0) {
        grouped[r.groupIndex[j]] += value;
        offered += value;
      }
    }
    for (let i = 0; i < r.rows; i++) {
      let value =
        -r.observedContrast[i] * offered - anchor[r.offset + i] * full;
      for (let g = 0; g < r.groups; g++)
        value += r.helmert[i * r.groups + g] * grouped[g];
      out[r.offset + i] += multiplier * value;
    }
  };
  const op: LinearOperator = {
    size: n,
    precondition(x, out) {
      records.forEach((r, i) =>
        solveSpd(
          factors[i],
          x.subarray(r.offset, r.offset + r.rows),
          out.subarray(r.offset, r.offset + r.rows),
        ),
      );
    },
    apply(x, out) {
      rw.fill(0);
      group.fill(0);
      suffix.fill(0);
      prefix.fill(0);
      out.fill(0);
      records.forEach((r, h) => {
        transformed.fill(0);
        let observed = 0,
          referenceDot = 0;
        for (let i = 0; i < r.rows; i++) {
          for (let j = 0; j < r.rows; j++)
            out[r.offset + i] += noises[h][i * r.rows + j] * x[r.offset + j];
          for (let g = 0; g < r.groups; g++)
            transformed[g] += r.helmert[i * r.groups + g] * x[r.offset + i];
          observed += r.observedContrast[i] * x[r.offset + i];
          referenceDot += anchor[r.offset + i] * x[r.offset + i];
        }
        for (let j = 0; j < k; j++) {
          const value =
            (r.groupIndex[j] >= 0
              ? transformed[r.groupIndex[j]] - observed
              : 0) - referenceDot;
          rw[h * k + j] = scale[j] * value;
          suffix[j] += rw[h * k + j];
          if (periodIndex[h] >= 0)
            group[periodIndex[h] * k + j] += reference[j] * value;
        }
      });
      for (const { r, i: h } of ordered) {
        for (let j = 0; j < k; j++) latent[j] = prefix[j] + r.age * suffix[j];
        response(r, scale, latent, 0, sigma ** 2, out);
        if (periodIndex[h] >= 0)
          response(r, reference, group, periodIndex[h] * k, 0.12 ** 2, out);
        for (let j = 0; j < k; j++) {
          prefix[j] += r.age * rw[h * k + j];
          suffix[j] -= rw[h * k + j];
        }
      }
    },
  };
  return op;
}
/** Exact Gaussian elimination for heterogeneous historical ballot strata.
 * Whiten sampling noise, then eliminate shared Gaussian innovations by Givens
 * rotations. This avoids subtracting nearly equal inverse-covariance terms or
 * solving an ill-conditioned country-sized covariance. No ridge is changed. */
function* stratifiedPrecisionSteps(
  records: DesignRecord[],
  reference: Vector,
  historical: Vector,
  sigma: number,
  maximumBytes?: number,
) {
  const k = reference.length,
    d = 2 * k,
    times = [...new Set(records.map((r) => r.age).filter((t) => t > 0))].sort(
      (a, b) => a - b,
    ),
    periods = [
      ...new Set(records.flatMap((r) => (r.period === null ? [] : [r.period]))),
    ].sort(),
    latent = (times.length + periods.length) * k,
    original = records.reduce((n, r) => n + r.rows, 0),
    rows = original + latent,
    width = latent + d;
  requireWorkspace(rows * width * 8 + rows * 16 + d * d * 8, maximumBytes);
  const matrix = new Float64Array(rows * width),
    scale = reference.map((v, j) => Math.max(v, historical[j]));
  let offset = 0;
  for (const r of records) {
    const mass = Math.max(
        1e-5,
        sum(reference.map((v, j) => v * r.available[j])),
      ),
      noise = r.regularizedNoise
        ? r.noise.slice()
        : r.noise.map((v) => v * mass * mass);
    if (!r.regularizedNoise)
      for (let i = 0; i < r.rows; i++) noise[i * r.rows + i] += 1e-16;
    const f = factorSpd(noise, r.rows),
      block = new Float64Array(r.rows * width);
    for (let i = 0; i < r.rows; i++) {
      const aq = sum(reference.map((v, j) => v * r.design[i * k + j]));
      for (let j = 0; j < k; j++) {
        const a = r.design[i * k + j],
          centred = a - aq;
        block[i * width + latent + r.context * k + j] = a;
        for (let t = 0; t < times.length; t++)
          if (r.age >= times[t])
            block[i * width + t * k + j] =
              sigma *
              Math.sqrt(times[t] - (times[t - 1] ?? 0)) *
              centred *
              scale[j];
        if (r.period !== null)
          block[
            i * width + (times.length + periods.indexOf(r.period)) * k + j
          ] = 0.12 * centred * reference[j];
      }
    }
    // W=L^-1 D for D N D=L L'. W N W'=I.
    for (let col = 0; col < width; col++)
      for (let i = 0; i < r.rows; i++) {
        let value = block[i * width + col] * f.scale[i];
        for (let j = 0; j < i; j++)
          value -= f.lower[i * r.rows + j] * matrix[(offset + j) * width + col];
        matrix[(offset + i) * width + col] = value / f.lower[i * r.rows + i];
      }
    offset += r.rows;
    yield {
      phase: "strata-whitening",
      completed: offset,
      total: original,
    } satisfies FitProgress;
  }
  for (let j = 0; j < latent; j++) matrix[(original + j) * width + j] = 1;
  let rotations = 0,
    maximumOrthogonalityError = 0;
  for (let col = 0; col < latent; col++) {
    for (let row = col + 1; row < rows; row++) {
      const b = matrix[row * width + col];
      if (b === 0) continue;
      const a = matrix[col * width + col],
        norm = Math.hypot(a, b),
        c = a / norm,
        s = b / norm;
      maximumOrthogonalityError = Math.max(
        maximumOrthogonalityError,
        Math.abs(c * c + s * s - 1),
      );
      for (let j = col; j < width; j++) {
        const x = matrix[col * width + j],
          y = matrix[row * width + j];
        matrix[col * width + j] = c * x + s * y;
        matrix[row * width + j] = -s * x + c * y;
      }
      matrix[row * width + col] = 0;
      rotations++;
    }
    if (!(Math.abs(matrix[col * width + col]) > 0))
      throw new Error("STRATIFIED_QR_RANK_FAILURE");
    yield {
      phase: "strata-orthogonal-elimination",
      completed: col + 1,
      total: latent,
    } satisfies FitProgress;
  }
  if (maximumOrthogonalityError > 1e-14)
    throw new Error("STRATIFIED_QR_ORTHOGONALITY_FAILURE");
  const precision = new Float64Array(d * d);
  for (let i = 0; i < d; i++) {
    for (let j = 0; j <= i; j++) {
      let value = 0,
        correction = 0;
      for (let row = latent; row < rows; row++) {
        const term =
            matrix[row * width + latent + i] *
              matrix[row * width + latent + j] -
            correction,
          next = value + term;
        correction = next - value - term;
        value = next;
      }
      precision[i * d + j] = precision[j * d + i] = value;
    }
    yield {
      phase: "strata-information",
      completed: i + 1,
      total: d,
    } satisfies FitProgress;
  }
  return { precision, rotations, maximumOrthogonalityError };
}

export function buildPoliticalTarget(
  input: NationalSource,
  options: SolverOptions = {},
) {
  return completeSteps(buildPoliticalTargetSteps(input, options));
}
export function* buildPoliticalTargetSteps(
  input: NationalSource,
  options: SolverOptions = {},
) {
  const tolerance = options.relativeTolerance ?? 1e-11;
  if (
    !(tolerance > 0 && tolerance <= 1e-11) ||
    !Number.isFinite(tolerance) ||
    (options.maximumIterations !== undefined &&
      (!Number.isSafeInteger(options.maximumIterations) ||
        options.maximumIterations < 1))
  )
    throw new RangeError(
      "Numerical solve settings cannot weaken the frozen residual gate",
    );
  const source = normalizeSource(input),
    k = source.components.length,
    d = 2 * k;
  const rawRows = [...source.elections, ...source.polls].map((r) =>
    Math.max(0, r.groups.length - 1),
  );
  // Reject impossible input allocations before constructing/condensing designs.
  requireWorkspace(
    8 *
      (3 * sum(rawRows) * k + 4 * sum(rawRows.map((r) => r * r)) + 12 * d * d),
    options.maximumWorkspaceBytes,
  );
  const originalRecords = buildDesigns(source),
    originalRows = sum(originalRecords.map((r) => r.rows)),
    collapsed =
      options.collapseRepeatedPolls === false
        ? { records: originalRecords, correction: [0, 0] }
        : collapsePollRecords(originalRecords, k);
  const records = collapsed.records,
    rows = records.map((r) => r.rows),
    n = sum(rows);
  const noiseSize = sum(rows.map((r) => r * r));
  const denseBytes =
    8 *
    (3 * n * k +
      4 * noiseSize +
      16 * n +
      4 * source.polls.length * k +
      12 * d * d +
      3 * n * n);
  const solver =
    options.solver === "auto" || !options.solver
      ? n <= Math.max(192, 8 * k) &&
        denseBytes <= (options.maximumWorkspaceBytes ?? 256 * 1024 * 1024)
        ? "dense"
        : "matrix-free"
      : options.solver;
  const aggregationBytes =
    n < originalRows
      ? 8 *
        (3 * originalRows * k +
          4 * sum(originalRecords.map((r) => r.rows ** 2)))
      : 0;
  const workspaceBytes =
    aggregationBytes +
    8 *
      (3 * n * k +
        4 * noiseSize +
        16 * n +
        4 * source.polls.length * k +
        12 * d * d +
        (solver === "dense" ? 3 * n * n : 0));
  requireWorkspace(workspaceBytes, options.maximumWorkspaceBytes);
  const scales = estimateScales(source);
  const historical = Float64Array.from(source.components, (id) => {
    const separate: number[] = [],
      grouped: number[] = [];
    for (const record of [
      ...source.elections.map((e) => ({
        groups: e.groups,
        shares: e.counts.slice(1).map((x) => x / (e.electors - e.counts[0])),
      })),
      ...source.polls,
    ])
      record.groups.forEach((g, j) => {
        if (g.includes(id)) {
          grouped.push(record.shares[j]);
          if (g.length === 1) separate.push(record.shares[j]);
        }
      });
    return Math.max(
      1e-4,
      separate.length
        ? Math.max(...separate)
        : grouped.length
          ? Math.min(...grouped)
          : 1e-4,
    );
  });
  let qbar = initialReference(source),
    usedLinearization = qbar,
    target: GaussianTarget | undefined,
    mode: ReturnType<typeof gaussianMode> | undefined;
  const iterationAudit: {
    solver: string;
    maximumRelativeResidual: number;
    iterations: number;
    pivotRatio: number | null;
    maximumOrthogonalityError?: number;
    covarianceSolves?: number;
  }[] = [];
  for (let iteration = 0; iteration < 3; iteration++) {
    usedLinearization = qbar;
    const latentCount =
        (new Set(records.map((r) => r.age).filter((t) => t > 0)).size +
          new Set(records.flatMap((r) => (r.period === null ? [] : [r.period])))
            .size) *
        k,
      qrBytes =
        (n + latentCount) * (latentCount + d) * 8 +
        (n + latentCount) * 16 +
        d * d * 8,
      stratified =
        source.elections.some((e) => e.strata?.length) &&
        qrBytes <= (options.maximumWorkspaceBytes ?? 256 * 1024 * 1024),
      precision = new Float64Array(d * d),
      linear = new Float64Array(d).fill(4 / k);
    let maximumRelativeResidual = 0,
      count = 0,
      pivotRatio: number | null = null,
      maximumOrthogonalityError: number | undefined;
    if (stratified) {
      const exact = yield* stratifiedPrecisionSteps(
        records,
        qbar,
        historical,
        scales.political,
        options.maximumWorkspaceBytes,
      );
      precision.set(exact.precision);
      maximumOrthogonalityError = exact.maximumOrthogonalityError;
      count = exact.rotations;
      yield {
        phase: "covariance-solves",
        completed: iteration + 1,
        total: 3,
      } satisfies FitProgress;
    } else {
      const covariance = informationCovariance(
        records,
        qbar,
        historical,
        scales.political,
      );
      const dense =
        n && solver === "dense"
          ? factorSpd(
              denseFromOperator(covariance, options.maximumWorkspaceBytes),
              n,
            )
          : undefined;
      pivotRatio = dense?.pivotRatio ?? null;
      for (let col = 0; col < d && n; col++) {
        yield {
          phase: "covariance-solves",
          completed: iteration * d + col,
          total: 3 * d,
        } satisfies FitProgress;
        const rhs = new Float64Array(n);
        for (const r of records)
          if (r.context === Math.floor(col / k))
            for (let i = 0; i < r.rows; i++)
              rhs[r.offset + i] = r.design[i * k + (col % k)];
        const result = dense
          ? {
              solution: solveSpd(dense, rhs),
              iterations: 0,
              relativeResidual: 0,
            }
          : conjugateGradient(
              covariance,
              rhs,
              tolerance,
              options.maximumIterations ?? 4096,
            );
        if (dense) {
          const actual = new Float64Array(n);
          for (let refinement = 0; ; refinement++) {
            covariance.apply(result.solution, actual);
            let error = 0,
              norm = 0;
            for (let i = 0; i < n; i++) {
              error += (actual[i] - rhs[i]) ** 2;
              norm += rhs[i] ** 2;
            }
            result.relativeResidual = norm ? Math.sqrt(error / norm) : 0;
            if (result.relativeResidual <= tolerance) break;
            if (refinement === 4)
              throw new Error(
                "COVARIANCE_SOLVE_FAILED: dense residual " +
                  result.relativeResidual +
                  " exceeds gate " +
                  tolerance +
                  "; rows " +
                  n,
              );
            const residual = Float64Array.from(rhs, (x, i) => x - actual[i]),
              correction = solveSpd(dense, residual);
            for (let i = 0; i < n; i++) result.solution[i] += correction[i];
            result.iterations++;
          }
        }
        count += result.iterations;
        maximumRelativeResidual = Math.max(
          maximumRelativeResidual,
          result.relativeResidual,
        );
        for (const r of records) {
          const ht = new Float64Array(r.groups);
          let observed = 0;
          for (let i = 0; i < r.rows; i++) {
            for (let g = 0; g < r.groups; g++)
              ht[g] +=
                r.helmert[i * r.groups + g] * result.solution[r.offset + i];
            observed += r.observedContrast[i] * result.solution[r.offset + i];
          }
          for (let j = 0; j < k; j++)
            if (r.groupIndex[j] >= 0)
              precision[(r.context * k + j) * d + col] +=
                ht[r.groupIndex[j]] - observed;
        }
      }
    }
    for (let i = 0; i < d; i++) precision[i * d + i] += 4;
    for (let context = 0; context < 2; context++)
      if (collapsed.correction[context])
        for (let i = 0; i < k; i++)
          for (let j = 0; j < k; j++)
            precision[(context * k + i) * d + context * k + j] +=
              collapsed.correction[context];
    for (let j = 0; j < k; j++) {
      const w = 1 / (0.5 * Math.max(qbar[j], historical[j])) ** 2;
      precision[j * d + j] += w;
      precision[(k + j) * d + k + j] += w;
      precision[j * d + k + j] -= w;
      precision[(k + j) * d + j] -= w;
    }
    for (let i = 0; i < d; i++)
      for (let j = 0; j < i; j++)
        precision[i * d + j] = precision[j * d + i] =
          (precision[i * d + j] + precision[j * d + i]) / 2;
    target = { components: k, blocks: 2, precision, linear };
    const currentMode = gaussianMode(target);
    mode = currentMode;
    qbar = Float64Array.from({ length: k }, (_, j) =>
      Math.max(1e-4, (currentMode.point[j] + currentMode.point[k + j]) / 2),
    );
    const total = sum(qbar);
    qbar = qbar.map((x) => x / total);
    iterationAudit.push({
      solver: stratified ? "orthogonal-strata" : n ? solver : "prior-only",
      maximumRelativeResidual,
      iterations: count,
      pivotRatio,
      ...(stratified ? { maximumOrthogonalityError, covarianceSolves: 0 } : {}),
    });
  }
  const discrepancy = estimateDiscrepancy(source, scales.political);
  return {
    source,
    target: target!,
    mode: mode!,
    audit: {
      scales,
      historicalScaleBounds: historical,
      finalLinearization: usedLinearization,
      nextLinearization: qbar,
      iterations: iterationAudit,
      observations: originalRows,
      ...(n < originalRows ? { covarianceRows: n } : {}),
      workspaceBytes,
      electionDiscrepancy: discrepancy.scale,
      discrepancyCalibration: discrepancy.events,
    },
  };
}
export function buildParticipationTarget(input: NationalSource) {
  const source = normalizeSource(input),
    scales = estimateScales(source),
    n = source.elections.length;
  const p = source.elections.map((e) => e.counts[0] / e.electors),
    bar = p.at(-1)!;
  const covariance = new Float64Array(n * n),
    design = new Float64Array(n * 4);
  for (let i = 0; i < n; i++) {
    const e = source.elections[i],
      h = Number(e.context === "european");
    design[i * 4 + h * 2] = 1 - p[i];
    design[i * 4 + h * 2 + 1] = -p[i];
    for (let j = 0; j < n; j++)
      covariance[i * n + j] =
        scales.participation ** 2 *
          bar ** 2 *
          (1 - bar) ** 2 *
          Math.min(
            Math.max(0, years(source.referenceDate, e.date)),
            Math.max(0, years(source.referenceDate, source.elections[j].date)),
          ) +
        (i === j ? (p[i] * (1 - p[i])) / e.electors + 1e-16 : 0);
  }
  const f = factorSpd(covariance, n),
    precision = new Float64Array(16),
    linear = new Float64Array(4).fill(2);
  for (let col = 0; col < 4; col++) {
    const solved = solveSpd(
      f,
      Float64Array.from({ length: n }, (_, i) => design[i * 4 + col]),
    );
    for (let row = 0; row < 4; row++)
      for (let i = 0; i < n; i++)
        precision[row * 4 + col] += design[i * 4 + row] * solved[i];
  }
  for (let j = 0; j < 4; j++) precision[j * 4 + j] += 4;
  const w = 1 / (0.5 * bar * (1 - bar)) ** 2;
  for (let j = 0; j < 2; j++) {
    precision[j * 4 + j] += w;
    precision[(j + 2) * 4 + j + 2] += w;
    precision[j * 4 + j + 2] -= w;
    precision[(j + 2) * 4 + j] -= w;
  }
  for (let i = 0; i < 4; i++)
    for (let j = 0; j < i; j++)
      precision[i * 4 + j] = precision[j * 4 + i] =
        (precision[i * 4 + j] + precision[j * 4 + i]) / 2;
  const target: GaussianTarget = {
    components: 2,
    blocks: 2,
    precision,
    linear,
  };
  return { target, mode: gaussianMode(target), covariance, design };
}
export function fitReference(
  source: NationalSource,
  seed: string,
  options: SolverOptions = {},
) {
  return completeSteps(fitReferenceSteps(source, seed, options));
}
function* fitIdentifiedReferenceSteps(
  source: NationalSource,
  seed: string,
  options: SolverOptions = {},
) {
  const political = yield* buildPoliticalTargetSteps(source, options),
    participation = buildParticipationTarget(source);
  yield {
    phase: "political-reference",
    completed: 0,
    total: 1,
  } satisfies FitProgress;
  const politicalSamples = yield* fitGaussianSteps(political.target, {
    seed: seed + ":political",
    maximumWorkspaceBytes: options.maximumWorkspaceBytes,
  });
  yield {
    phase: "non-valid-reference",
    completed: 0,
    total: 1,
  } satisfies FitProgress;
  const participationSamples = yield* fitGaussianSteps(participation.target, {
    seed: seed + ":participation",
    maximumWorkspaceBytes: options.maximumWorkspaceBytes,
  });
  return {
    componentIds: political.source.components,
    contexts: ["camera", "european"] as const,
    referenceDate: source.referenceDate,
    political: politicalSamples,
    participation: participationSamples,
    audit: political.audit,
  };
}

/** Fit only identifiable group totals. Expansion is a latent coordinate layout,
 * not a decomposition of observed counts; required references anchor contrasts. */
export function* fitReferenceSteps(
  source: NationalSource,
  seed: string,
  options: SolverOptions = {},
) {
  const groups = identificationGroups(
    source.components,
    originalPartitions(source),
  );
  const required = groups.filter((g) => g.length > 1).flat();
  if (!required.length) {
    if (source.referenceRequired?.some((id) => !source.components.includes(id)))
      throw new Error("INVALID_REFERENCE_BASIS");
    return {
      ...(yield* fitIdentifiedReferenceSteps(source, seed, options)),
      ...({} as { identificationGroups?: string[][] }),
      ...(source.referenceRequired?.length
        ? {
            referenceRequired: source.referenceRequired,
            referenceDirections: source.referenceDirections,
          }
        : ({} as {
            referenceRequired?: string[];
            referenceDirections?: number[][];
          })),
    };
  }
  if (required.some((id) => !source.referenceRequired?.includes(id)))
    throw new Error("UNRESOLVED_COMPONENT_BASIS");
  const names = groups.map((_, i) => "group:" + String(i).padStart(3, "0")),
    remap = (g: string[]) => [
      ...new Set(g.map((id) => names[groups.findIndex((x) => x.includes(id))])),
    ];
  const quotient: NationalSource = {
    ...source,
    components: names,
    referenceRequired: [],
    elections: source.elections.map((e) => ({
      ...e,
      groups: e.groups.map(remap),
      ...(e.strata
        ? {
            strata: e.strata.map((r) => ({
              ...r,
              groups: r.groups.map(remap),
            })),
          }
        : {}),
    })),
    polls: source.polls.map((p) => ({ ...p, groups: p.groups.map(remap) })),
  };
  const fit = yield* fitIdentifiedReferenceSteps(quotient, seed, options),
    ids = [...source.components].sort(),
    old = fit.political;
  const values = new Float64Array(
    old.chains * old.drawsPerChain * 2 * ids.length,
  );
  const groupIndex = ids.map((id) => groups.findIndex((g) => g.includes(id)));
  for (let r = 0; r < old.chains * old.drawsPerChain; r++)
    for (let c = 0; c < 2; c++)
      ids.forEach((id, j) => {
        const g = groupIndex[j];
        values[r * 2 * ids.length + c * ids.length + j] =
          old.values[r * 2 * names.length + c * names.length + g] /
          groups[g].length;
      });
  const coordinates = [0, 1].flatMap((c) =>
    groupIndex.map((g) => old.diagnostics.coordinates[c * names.length + g]),
  );
  return {
    ...fit,
    componentIds: ids,
    political: {
      ...old,
      values,
      dimensions: 2 * ids.length,
      diagnostics: { ...old.diagnostics, coordinates },
    },
    identificationGroups: groups.filter((g) => g.length > 1),
    referenceRequired: source.referenceRequired ?? required,
    referenceDirections: source.referenceDirections,
  };
}
