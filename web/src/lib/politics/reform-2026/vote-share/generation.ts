import type { Chamber, ReferenceScope } from "../input";
import type {
  GenerationTrace,
  ReformAssemblyContext,
  ReformLegalSettings,
} from "../generation-types";
import { assembleReformSimulation, type CollegeCounts } from "../generation";
import { prepareReferenceBonusPlan } from "../reference-bonus-plan";
import { bonusKey } from "../bonus-reference-support";
import { multinomial } from "../generation-math";
import {
  FAST_SHARE_MODEL,
  FAST_SHARE_NUMERICS,
  day,
  requireWorkspace,
  sum,
  years,
} from "./types";
import {
  FAST_GENERATION_VERSION,
  SENATE_ADAPTER_VERSION,
  type FastGenerationScenario,
  type FastVoteAssets,
  type PhysicalCell,
} from "./forecast-types";
import { FORECAST_STREAM_VERSION, forecastRandom } from "./forecast-random";
import { physicalCells } from "./physical-cells";
import {
  referencePlans,
  referenceMembers,
  referenceSubjectKey,
  scopeKey,
  scopeMatches,
  sampledPlan,
  commonAnchor,
} from "./reference-plans";
import { REFERENCE_POLICY } from "./reference-family";
import { projectMargins, ProjectionCapacityError } from "./projection";
import {
  removeAbsent,
  politicalIntervention,
  nonValidIntervention,
} from "./interventions";
import { adaptSenateReference } from "./senate-adapter";
import { betaTransition, movementScales } from "./transition";
import { feasibleField } from "./feasible-field";
import { referenceNoise } from "./reference-noise";

export interface FastVoteAudit {
  projectionResidual: number;
  projectionIterations: number;
  referenceResidual: number;
  correlation: number;
  referenceTargets: {
    scope: ReferenceScope;
    chamber: Chamber;
    nonValid?: number;
    groups: { id: string; members: string[]; share: number }[];
  }[];
  warnings: string[];
  countryReference: Partial<Record<Chamber, number[]>>;
  countryForecast: Partial<Record<Chamber, number[]>>;
}
export interface FastGenerationOptions {
  chunkSize?: number;
  maximumWorkspaceBytes?: number;
}
type Surfaces = Record<Chamber, Float64Array[]>;
export function prepareFastGeneration(
  scenario: FastGenerationScenario,
  inputAssets: FastVoteAssets,
  context: ReformAssemblyContext,
  options: FastGenerationOptions = {},
) {
  context = structuredClone({
    version: context.version,
    sourceVersions: context.sourceVersions,
    assumptions: context.assumptions,
    seats: context.seats,
    models: context.models,
  });
  const s = structuredClone(scenario),
    assets = structuredClone(inputAssets),
    chambers: Chamber[] =
      s.renewal === "joint" ? ["camera", "senato"] : [s.renewal];
  const fit = assets.fit,
    ids = s.components.map((c) => c.id).sort(),
    componentsById = new Map(s.components.map((c) => [c.id, c])),
    listsById = {
      camera: new Map(s.lists.camera.map((l) => [l.id, l])),
      senato: new Map(s.lists.senato.map((l) => [l.id, l])),
    },
    index = new Map(ids.map((id, i) => [id, i + 1])),
    k = ids.length,
    n = k + 1;
  const chunkSize = options.chunkSize ?? 128;
  if (!Number.isSafeInteger(chunkSize) || chunkSize < 1)
    throw new Error("Invalid geographic chunk size");
  if (
    s.schemaVersion !== 4 ||
    s.kind !== "politics-reform-scenario" ||
    s.algorithmId !== "politiche-reform-2026-v1" ||
    s.models.statistical !== FAST_SHARE_MODEL ||
    s.models.numerical !== FAST_SHARE_NUMERICS ||
    s.models.references !== REFERENCE_POLICY ||
    s.models.chambers !== "fast-share-paired-v1" ||
    s.models.rng !== FORECAST_STREAM_VERSION ||
    assets.modelId !== FAST_SHARE_MODEL ||
    assets.numericalVersion !== FAST_SHARE_NUMERICS
  )
    throw new Error("FAST_GENERATION_MODEL_MISMATCH");
  if (
    s.referenceDate !== fit.referenceDate ||
    s.referenceDate !== assets.profileReferenceDate ||
    s.fitSeed !== assets.fitSeed ||
    day(s.electionDate) < day(s.referenceDate) ||
    s.data.sourceBundleId !== assets.sourceBundleId ||
    s.data.fittedStateId !== assets.fittedStateId ||
    s.data.profileStateId !== assets.profileStateId ||
    s.data.geographyVersion !== assets.geography.version ||
    assets.geography.version !== context.seats.geography.version
  )
    throw new Error(
      "FIT_STALE: incompatible date/source/fit/profile/geography",
    );
  if (
    !k ||
    k > 128 ||
    new Set(ids).size !== k ||
    s.components.some(
      (c) =>
        !c.id || !["fitted", "new"].includes(c.kind) || !c.provenance.trim(),
    ) ||
    !["joint", "camera", "senato"].includes(s.renewal)
  )
    throw new Error("Invalid future component basis/renewal");
  for (const component of s.components) {
    if (component.kind === "fitted" && !fit.componentIds.includes(component.id))
      throw new Error("COMPONENT_NOT_IDENTIFIED");
    if (
      component.kind === "new" &&
      (!component.fallback ||
        Object.entries(component.fallback).some(
          ([key, x]) =>
            !Number.isFinite(x) || (key !== "geographicLogProfile" && x < 0),
        ))
    )
      throw new Error("NEW_COMPONENT_FALLBACK_REQUIRED");
    if (
      component.kind === "new" &&
      !s.references.some(
        (r) =>
          (r.subject.kind === "component" && r.subject.id === component.id) ||
          (r.subject.kind === "list" &&
            chambers.some((ch) =>
              s.lists[ch].some(
                (l) =>
                  l.id === r.subject.id &&
                  l.componentIds.length === 1 &&
                  l.componentIds[0] === component.id,
              ),
            )),
      )
    )
      throw new Error("NEW_COMPONENT_REFERENCE_REQUIRED");
  }
  if (
    !fit.componentIds.length ||
    new Set(fit.componentIds).size !== fit.componentIds.length ||
    fit.political.dimensions !== 2 * fit.componentIds.length ||
    fit.participation.dimensions !== 4 ||
    fit.contexts.join("|") !== "camera|european"
  )
    throw new Error("FIT_COORDINATE_MISMATCH");
  for (const sample of [fit.political, fit.participation]) {
    if (
      !sample.diagnostics.passed ||
      sample.chains !== 4 ||
      ![2000, 4000, 8000].includes(sample.drawsPerChain) ||
      sample.values.length !==
        sample.chains * sample.drawsPerChain * sample.dimensions ||
      sample.values.some((x) => !Number.isFinite(x) || x < 0)
    )
      throw new Error("FIT_NOT_ACCEPTED");
    if (
      sample.diagnostics.coordinates.length !== sample.dimensions ||
      sample.diagnostics.coordinates.some((d) =>
        d.constant === "fixed"
          ? sample !== fit.political || fit.componentIds.length !== 1
          : d.constant !== false ||
            d.rhat === null ||
            !Number.isFinite(d.rhat) ||
            d.rhat > 1.05 ||
            !(d.bulkEss >= 400) ||
            !(d.tailEss >= 200),
      )
    )
      throw new Error(
        "FIT_NOT_ACCEPTED: missing/failed coordinate diagnostics or unsupported fixed constraint",
      );
    const width = sample === fit.political ? fit.componentIds.length : 2;
    for (let r = 0; r < sample.values.length; r += width) {
      let mass = 0;
      for (let j = 0; j < width; j++) mass += sample.values[r + j];
      if (Math.abs(mass - 1) > 1e-10)
        throw new Error("FIT_NOT_ACCEPTED: broken simplex");
    }
  }
  if (
    [
      fit.audit.scales.political,
      fit.audit.scales.participation,
      fit.audit.electionDiscrepancy,
    ].some((x) => !Number.isFinite(x) || x < 0)
  )
    throw new Error("Invalid fitted basis/scales");
  if (
    chambers.includes("senato") &&
    (assets.senateAdapter?.version !== SENATE_ADAPTER_VERSION ||
      assets.senateAdapter.referenceDate !== s.referenceDate ||
      assets.senateAdapter.componentIds.join("|") !==
        fit.componentIds.join("|"))
  )
    throw new Error("SENATE_ADAPTER_REQUIRED");
  const profile = context.models.chambers.profiles.find(
    (p) => p.id === s.chamberProfileId,
  );
  if (!profile) throw new Error("Unknown chamber dependence profile");
  const correlation =
    profile.sharedVariance +
    (1 - profile.sharedVariance) * profile.residualCorrelation;
  if (
    !Number.isFinite(correlation) ||
    Math.abs(correlation) > 1 ||
    !Number.isFinite(profile.gapMultiplier) ||
    profile.gapMultiplier < 0
  )
    throw new Error("Invalid chamber dependence budget");
  const cells: Record<Chamber, PhysicalCell[]> = { camera: [], senato: [] };
  for (const cell of physicalCells(
    assets.geography,
    assets.electorates,
    chambers,
    s.seed,
  ))
    cells[cell.chamber].push(cell);
  requireWorkspace(
    chambers.reduce((m, ch) => m + cells[ch].length * n * 8 * 8, 0) +
      fit.political.values.byteLength +
      fit.participation.values.byteLength,
    options.maximumWorkspaceBytes,
  );
  const filing = new Map<string, Map<string, string>>(),
    listPositions = new Map<string, Map<string, number[]>>();
  for (const ch of chambers) {
    if (new Set(s.lists[ch].map((l) => l.id)).size !== s.lists[ch].length)
      throw new Error("Duplicate future list");
    for (const list of s.lists[ch])
      if (
        !list.componentIds.length ||
        new Set(list.componentIds).size !== list.componentIds.length ||
        list.componentIds.some((id) => !index.has(id)) ||
        [
          list.nationalUncertaintyMultiplier ?? 1,
          list.localUncertaintyMultiplier ?? 1,
        ].some((x) => !Number.isFinite(x) || x < 0)
      )
        throw new Error("Invalid list composition/uncertainty");
    for (const id of ids)
      if (
        new Set(
          s.lists[ch]
            .filter((l) => l.componentIds.includes(id))
            .map((l) => l.nationalUncertaintyMultiplier ?? 1),
        ).size > 1
      )
        throw new Error(
          "COMPONENT_UNCERTAINTY_CONFLICT: one national component has conflicting list multipliers",
        );
    const expected = context.seats.chambers.find(
      (c) => c.chamber === ch,
    )!.colleges;
    const coverage = s.coverage.filter((c) => c.chamber === ch);
    if (
      new Set(coverage.map((c) => c.collegeId)).size !== coverage.length ||
      expected.some((c) => !coverage.some((x) => x.collegeId === c.id))
    )
      throw new Error("Incomplete physical/derived filing");
    for (const c of coverage) {
      if (
        !expected.some((x) => x.id === c.collegeId) ||
        new Set(c.listIds).size !== c.listIds.length
      )
        throw new Error("Invalid filing college/list");
      const available = new Map<string, string>(),
        lists = new Map<string, number[]>();
      for (const id of c.listIds) {
        const list = s.lists[ch].find((l) => l.id === id);
        if (!list) throw new Error("Unknown filed list");
        lists.set(
          id,
          list.componentIds.map((id) => index.get(id)!),
        );
        for (const member of list.componentIds) {
          if (available.has(member))
            throw new Error(
              "COMPONENT_OVERLAP: two lists on one physical ballot",
            );
          available.set(member, id);
        }
      }
      filing.set(ch + ":" + c.collegeId, available);
      listPositions.set(ch + ":" + c.collegeId, lists);
    }
  }
  const filedList = (c: PhysicalCell, id: string) =>
    filing.get(c.chamber + ":" + c.collegeId)?.get(id);
  const plans = referencePlans(s, cells, chambers, filedList),
    position = new Map(fit.componentIds.map((id, i) => [id, i])),
    profilePosition = new Map(
      assets.cameraProfile.municipalityIds.map((id, i) => [id, i]),
    );
  for (const p of [
    assets.cameraProfile,
    ...(assets.senateAdapter ? [assets.senateAdapter.profile] : []),
  ]) {
    if (
      p.componentIds.join("|") !== fit.componentIds.join("|") ||
      p.means.length !==
        p.municipalityIds.length * (fit.componentIds.length + 1) ||
      p.profileSd.length !== p.means.length ||
      p.means.some((x) => !Number.isFinite(x)) ||
      p.profileSd.some((x) => !Number.isFinite(x) || x < 0) ||
      p.futureScales.length !== fit.componentIds.length + 1 ||
      p.futureScales.some((x) => !Number.isFinite(x) || x < 0)
    )
      throw new Error("Invalid compatible profile");
    if (
      p.municipalityIds.join("|") !==
      assets.cameraProfile.municipalityIds.join("|")
    )
      throw new Error("Profile geography ordering mismatch");
  }
  for (const ch of chambers)
    for (const c of cells[ch])
      if (!profilePosition.has(c.municipality))
        throw new Error("Missing labelled target profile");
  const profileValue = (
    ch: Chamber,
    cell: PhysicalCell,
    j: number,
    kind: "means" | "profileSd" | "futureScales",
  ) => {
    const id = ids[j - 1],
      old = j === 0 ? 0 : (position.get(id) ?? -2) + 1,
      component = j ? componentsById.get(id) : undefined;
    if (old < 0) {
      const fallback = component!.fallback!;
      return kind === "means"
        ? fallback.geographicLogProfile
        : kind === "profileSd"
          ? fallback.profileSd
          : fallback.localInnovationSd;
    }
    const i = profilePosition.get(cell.municipality)!,
      stride = fit.componentIds.length + 1,
      at = kind === "futureScales" ? old : i * stride + old,
      c = assets.cameraProfile[kind][at];
    if (ch === "camera") return c;
    return kind === "means"
      ? c +
          profile.gapMultiplier * (assets.senateAdapter!.profile.means[at] - c)
      : assets.senateAdapter!.profile[kind][at];
  };
  // Profiles and filing do not change between draws of this prepared scenario.
  // Cache their resolved coordinates once, rather than repeat identity/geography
  // lookups for every municipality, component and posterior draw.
  const referenceProfiles = new Map<
    PhysicalCell,
    {
      means: Float64Array;
      sd: Float64Array;
      offered: boolean[];
      localMultipliers: Float64Array;
    }
  >();
  for (const ch of chambers)
    for (const cell of cells[ch])
      referenceProfiles.set(cell, {
        means: Float64Array.from({ length: n }, (_, j) =>
          profileValue(ch, cell, j, "means"),
        ),
        sd: Float64Array.from({ length: n }, (_, j) =>
          profileValue(ch, cell, j, "profileSd"),
        ),
        offered: [true, ...ids.map((id) => filedList(cell, id) !== undefined)],
        localMultipliers: Float64Array.from({ length: n }, (_, j) =>
          j === 0
            ? 1
            : (listsById[ch].get(filedList(cell, ids[j - 1]) ?? "")
                ?.localUncertaintyMultiplier ?? 1),
        ),
      });
  const baseCountry = (ch: Chamber, simulationId: number, central: boolean) => {
    const political = fit.political,
      participation = fit.participation,
      K = fit.componentIds.length;
    const sample = (x: typeof political, scope: string, dimension: number) => {
      if (!central) {
        const row = forecastRandom(
          s.seed,
          simulationId,
          scope,
          ch,
          correlation,
        ).integer(x.chains * x.drawsPerChain);
        return x.values.slice(
          row * x.dimensions,
          row * x.dimensions + dimension,
        );
      }
      const out = new Float64Array(dimension),
        count = x.chains * x.drawsPerChain;
      for (let r = 0; r < count; r++)
        for (let j = 0; j < dimension; j++)
          out[j] += x.values[r * x.dimensions + j] / count;
      return out;
    };
    let q = sample(political, "posterior-political-row", K),
      a = sample(participation, "posterior-non-valid-row", 1)[0];
    if (ch === "senato") {
      const adapted = adaptSenateReference(
        q,
        a,
        assets.senateAdapter!,
        profile.gapMultiplier,
      );
      q = adapted.political;
      a = adapted.nonValid;
    }
    const b = new Float64Array(n);
    b[0] = a;
    ids.forEach((id, j) => {
      const old = position.get(id);
      b[j + 1] = old === undefined ? 0 : (1 - a) * q[old];
    });
    const offered = [
      true,
      ...ids.map((id) => cells[ch].some((c) => filedList(c, id) !== undefined)),
    ];
    return removeAbsent(b, offered);
  };
  const geographicField = (
    ch: Chamber,
    makeRow: (i: number) => Float64Array,
  ) => {
    const field = new Float64Array(cells[ch].length * n);
    for (let start = 0; start < cells[ch].length; start += chunkSize)
      for (
        let i = start;
        i < Math.min(cells[ch].length, start + chunkSize);
        i++
      )
        field.set(makeRow(i), i * n);
    return (i: number) => field.subarray(i * n, (i + 1) * n);
  };
  const makeReferenceNoise = (simulationId: number, central: boolean) =>
    referenceNoise(
      s.seed,
      simulationId,
      n,
      correlation,
      s.models.profileRegionCorrelation ?? 0,
      central,
    );
  const surface = (
    ch: Chamber,
    simulationId: number,
    central: boolean,
    noiseField = makeReferenceNoise(simulationId, central),
  ) => {
    const b = baseCountry(ch, simulationId, central),
      logBase = b.map((x) => (x > 0 ? Math.log(x) : -Infinity)),
      scores = (i: number) => {
        const c = cells[ch][i],
          resolved = referenceProfiles.get(c)!,
          noise = noiseField(ch, c.municipality, c.region);
        return Float64Array.from({ length: n }, (_, j) =>
          !resolved.offered[j]
            ? -Infinity
            : b[j] > 0
              ? logBase[j] + resolved.means[j] + resolved.sd[j] * noise[j]
              : -Infinity,
        );
      };
    const field = geographicField(ch, scores);
    const conditional = {
      probabilities: (i: number) => {
        const row = field(i),
          max = Math.max(...row),
          weights = Float64Array.from(row, (x) => Math.exp(x - max)),
          total = sum(weights);
        if (!(total > 0)) throw new Error("EMPTY_REFERENCE_FILING");
        return weights.map((x) => x / total);
      },
    };
    let projected: Pick<ReturnType<typeof projectMargins>, "probabilities">,
      referenceConditioned = false;
    if (
      s.models.forecast &&
      s.models.forecastReference !== "country-calibrated"
    )
      projected = conditional;
    else
      try {
        projected = projectMargins(
          b,
          cells[ch].length,
          (i) => ({ weight: cells[ch][i].electors, scores: field(i) }),
          options,
        );
      } catch (error) {
        if (
          s.models.forecast &&
          s.models.forecastReference === "country-calibrated" &&
          error instanceof ProjectionCapacityError
        ) {
          projected = conditional;
          referenceConditioned = true;
        } else throw error;
      }
    const rows = cells[ch].map((_, i) => projected.probabilities(i));
    return {
      rows,
      backup: rows.map((r) => r.slice()),
      scores,
      b,
      referenceConditioned,
    };
  };
  const centralBase: Surfaces = { camera: [], senato: [] };
  for (const ch of chambers) centralBase[ch] = surface(ch, 0, true).rows;
  const aggregate = (
    ch: Chamber,
    rows: Float64Array[],
    scope?: ReferenceScope,
    members?: number[],
  ) => {
    let mass = 0,
      denominator = 0;
    cells[ch].forEach((c, i) => {
      if (scope && !scopeMatches(c, scope)) return;
      const valid = members ? 1 - rows[i][0] : 1;
      denominator += c.electors * valid;
      mass +=
        c.electors *
        (members ? sum(members.map((j) => rows[i][j])) : rows[i][0]);
    });
    return denominator ? mass / denominator : 0;
  };
  const applyReferences = (
    surfaces: Surfaces,
    backups: Surfaces,
    simulationId: number,
    central: boolean,
    audit: FastVoteAudit,
    noiseField: ReturnType<typeof referenceNoise>,
  ) => {
    const fallback = (ch: Chamber, i: number, j: number) => {
      const cell = cells[ch][i];
      if (j > 0 && !filedList(cell, ids[j - 1])) return 0;
      if (backups[ch][i][j] > 0)
        return backups[ch][i][j] / Math.max(1e-300, 1 - backups[ch][i][0]);
      if (j > 0 && componentsById.get(ids[j - 1])!.kind === "new") {
        const noise = noiseField(ch, cell.municipality, cell.region)[j];
        return Math.exp(
          referenceProfiles.get(cell)!.means[j] +
            referenceProfiles.get(cell)!.sd[j] * noise,
        );
      }
      return 0;
    };
    for (const plan of plans) {
      const sampled = sampledPlan(plan, s.seed, simulationId, central);
      const commonNv = sampled.nonValid.common,
        commonNvValues =
          commonNv === undefined
            ? undefined
            : commonAnchor(
                commonNv,
                aggregate("camera", centralBase.camera, plan.scope),
                aggregate("senato", centralBase.senato, plan.scope),
                profile.gapMultiplier,
              );
      for (const ch of chambers) {
        const selected = cells[ch].flatMap((c, i) =>
            scopeMatches(c, plan.scope) ? [i] : [],
          ),
          rows = selected.map((i) => surfaces[ch][i]),
          electors = selected.map((i) => cells[ch][i].electors);
        const targetNv = sampled.nonValid[ch] ?? commonNvValues?.[ch];
        if (targetNv !== undefined) {
          const r = nonValidIntervention(
            rows,
            electors,
            targetNv,
            (i, j) => fallback(ch, selected[i], j),
            options,
          );
          audit.referenceResidual = Math.max(
            audit.referenceResidual,
            r.residual,
          );
        }
        const resolved = new Map<
          string,
          {
            id: string;
            members: string[];
            share: number;
            subject: import("./forecast-types").FastReference["subject"];
          }
        >();
        for (const channel of ["common", ch] as const) {
          const p = plan.political[channel],
            draw = sampled.political[channel];
          if (!p || !draw) continue;
          p.family.groups.forEach((g, i) => {
            const row = p.rows.get(g.id)!,
              members = referenceMembers(s, row, ch);
            let value = draw.values[i];
            if (channel === "common" && chambers.length === 2) {
              const positions = members.map((id) => index.get(id)!);
              const c = commonAnchor(
                value,
                aggregate("camera", centralBase.camera, plan.scope, positions),
                aggregate("senato", centralBase.senato, plan.scope, positions),
                profile.gapMultiplier,
              );
              value = c[ch];
              if (c.fallback)
                audit.warnings.push("COMMON_CONTRAST_FALLBACK:" + g.id);
            }
            const key = referenceSubjectKey(s, row, ch);
            resolved.set(key, {
              id: g.id,
              members,
              share: value,
              subject: row.subject,
            });
          });
        }
        const values = [...resolved.values()];
        if (values.length) {
          const r = politicalIntervention(
            rows,
            electors,
            (i) =>
              values.map((g) =>
                g.members
                  .filter(
                    (id) =>
                      g.subject.kind === "component" ||
                      filedList(cells[ch][selected[i]], id) === g.subject.id,
                  )
                  .map((id) => index.get(id)!),
              ),
            values.map((g) => g.share),
            (i, j) => fallback(ch, selected[i], j),
            options,
          );
          audit.referenceResidual = Math.max(
            audit.referenceResidual,
            r.residual,
          );
        }
        if (targetNv !== undefined || values.length)
          audit.referenceTargets.push({
            scope: plan.scope,
            chamber: ch,
            ...(targetNv === undefined ? {} : { nonValid: targetNv }),
            groups: values.map(({ id, members, share }) => ({
              id,
              members,
              share,
            })),
          });
      }
    }
  };
  const freshAudit = (): FastVoteAudit => ({
    projectionResidual: 0,
    projectionIterations: 0,
    referenceResidual: 0,
    correlation,
    referenceTargets: [],
    warnings: [],
    countryReference: {},
    countryForecast: {},
  });
  const referenceSurfaces = (
    simulationId: number,
    central: boolean,
    audit: FastVoteAudit,
  ) => {
    const rows: Surfaces = { camera: [], senato: [] },
      backup: Surfaces = { camera: [], senato: [] },
      noiseField = makeReferenceNoise(simulationId, central);
    for (const ch of chambers) {
      if (central) {
        rows[ch] = centralBase[ch].map((r) => r.slice());
        backup[ch] = centralBase[ch];
      } else {
        const x = surface(ch, simulationId, false, noiseField);
        rows[ch] = x.rows;
        backup[ch] = x.backup;
        if (x.referenceConditioned)
          audit.warnings.push(
            "REFERENCE_POSTERIOR_CONDITIONED_ON_FILING:" + ch,
          );
      }
    }
    applyReferences(rows, backup, simulationId, central, audit, noiseField);
    return rows;
  };
  const centralAudit = freshAudit(),
    central = referenceSurfaces(0, true, centralAudit),
    support = new Map<string, number>();
  for (const ch of chambers)
    cells[ch].forEach((c, i) => {
      const college = context.seats.geography.colleges.find(
        (x) => x.id === c.collegeId,
      )!;
      if (college.kind !== "ordinary") return;
      for (const [list, positions] of listPositions.get(
        ch + ":" + c.collegeId,
      )!) {
        const key = bonusKey(ch, college.territoryId, list);
        support.set(
          key,
          (support.get(key) ?? 0) +
            c.electors * sum(positions.map((j) => central[ch][i][j])),
        );
      }
    });
  const bonusPlan = s.candidateGeneration.bonusPolicy
    ? prepareReferenceBonusPlan(s, context, [], {
        referenceDate: s.referenceDate,
        support,
      })
    : undefined;
  const legalLists = (ch: Chamber) =>
    s.lists[ch].map((l) => ({
      id: l.id,
      name: l.name,
      coalitionId: l.coalitionId,
      protectedMinority: l.protectedMinority,
      ...(l.kind ? { kind: l.kind } : {}),
    }));
  const legalScenario: ReformLegalSettings = {
    ...s,
    lists: { camera: legalLists("camera"), senato: legalLists("senato") },
  };
  const baseTrace: GenerationTrace = {
    warnings: [],
    references: [],
    normalization: [],
    streamVersion: FORECAST_STREAM_VERSION,
    snapshotVersion: assets.sourceBundleId,
    modelVersion: FAST_SHARE_MODEL,
  };
  const traceScopes: ReferenceScope[] = [
    { kind: "national" },
    ...plans.filter((p) => p.scope.kind !== "national").map((p) => p.scope),
  ];
  for (const scope of traceScopes)
    for (const listId of [
      ...new Set(chambers.flatMap((ch) => s.lists[ch].map((l) => l.id))),
    ].sort()) {
      const ref = (ch: Chamber) => {
        if (!chambers.includes(ch)) return null;
        const list = s.lists[ch].find((l) => l.id === listId);
        if (!list) return null;
        const supplied = s.references.some(
          (r) =>
            scopeKey(r.scope) === scopeKey(scope) &&
            (r.channel === "common" || r.channel === ch) &&
            (r.subject.kind === "list"
              ? r.subject.id === listId
              : list.componentIds.length === 1 &&
                list.componentIds[0] === r.subject.id),
        );
        if (scope.kind !== "national" && !supplied) return null;
        let numerator = 0,
          denominator = 0;
        cells[ch].forEach((c, i) => {
          if (!scopeMatches(c, scope)) return;
          denominator += c.electors * (1 - central[ch][i][0]);
          for (const id of list.componentIds)
            if (filedList(c, id) === listId)
              numerator += c.electors * central[ch][i][index.get(id)!];
        });
        return {
          share: denominator ? numerator / denominator : 0,
          referenceDate: s.referenceDate,
          origin: supplied ? ("supplied" as const) : ("model-derived" as const),
        };
      };
      const camera = ref("camera"),
        senato = ref("senato");
      if (!camera && !senato) continue;
      const plan = plans.find((p) => scopeKey(p.scope) === scopeKey(scope)),
        common = plan?.political.common;
      const commonRow = common?.family.groups.find((g) => {
        const r = common.rows.get(g.id)!;
        return r.subject.kind === "list" && r.subject.id === listId;
      });
      baseTrace.references.push({
        scope,
        listIds: {
          camera: camera ? listId : null,
          senato: senato ? listId : null,
        },
        common: commonRow
          ? { share: commonRow.mean, referenceDate: s.referenceDate }
          : null,
        camera,
        senato,
      });
    }
  for (const message of centralAudit.warnings)
    baseTrace.warnings.push({ code: message.split(":")[0], message });
  for (const plan of plans)
    for (const channel of ["common", ...chambers] as const) {
      const f = plan.political[channel]?.family;
      if (f?.normalization !== null && f?.normalization !== undefined) {
        for (const chamber of channel === "common" ? chambers : [channel])
          baseTrace.normalization.push({
            scope: plan.scope,
            chamber,
            reason: channel + " reference centres before joint calibration",
            before: f.normalization,
            after: 1,
          });
        const changes = f.groups.map((g) => ({
          id: g.id,
          before: {
            mean: plan.political[channel]!.rows.get(g.id)!.mean,
            sdPercentagePoints: plan.political[channel]!.rows.get(g.id)!
              .sdPercentagePoints,
          },
          after: { mean: g.mean, sdPercentagePoints: g.sdPercentagePoints },
        }));
        baseTrace.warnings.push({
          code: "NORMALIZED_REFERENCE_CENTRES",
          message:
            "Reference means and SDs divided by " +
            f.normalization +
            " before joint calibration (" +
            plan.key +
            "/" +
            channel +
            "). " +
            JSON.stringify(changes),
        });
      }
    }
  if (bonusPlan)
    baseTrace.bonusSelection = {
      policy: bonusPlan.policy,
      referenceDate: bonusPlan.referenceDate,
      slates: bonusPlan.slates.map(
        ({ chamber, territoryId, subject, allocations }) => ({
          chamber,
          territoryId,
          subject,
          allocations,
        }),
      ),
    };
  const run = (simulationId: number) => {
    if (!Number.isSafeInteger(simulationId) || simulationId < 1)
      throw new Error("Expected positive simulation ID");
    const audit = freshAudit(),
      reference = referenceSurfaces(simulationId, false, audit),
      counts = new Map<string, CollegeCounts>();
    const horizon = years(s.electionDate, s.referenceDate);
    for (const ch of chambers) {
      const total = sum(cells[ch].map((c) => c.electors)),
        b = new Float64Array(n);
      cells[ch].forEach((c, i) =>
        reference[ch][i].forEach((p, j) => (b[j] += (c.electors * p) / total)),
      );
      audit.countryReference[ch] = [...b];
      const q = b.slice(1).map((p) => (1 - b[0] > 0 ? p / (1 - b[0]) : 0)),
        nv = Float64Array.from([b[0], 1 - b[0]]),
        scale = movementScales(
          fit.audit.scales.political,
          fit.audit.scales.participation,
          forecastRandom(
            s.seed,
            simulationId,
            "movement-scales",
            ch,
            correlation,
          ),
        );
      const multipliers = ids.map((id) => {
        const list = s.lists[ch].find((l) => l.componentIds.includes(id));
        return list?.nationalUncertaintyMultiplier ?? 1;
      });
      const politicalScales = ids.map((id, j) =>
        componentsById.get(id)!.kind === "new"
          ? componentsById.get(id)!.fallback!.nationalInnovationSd *
            multipliers[j]
          : scale.political * multipliers[j],
      );
      let projected: Pick<
        ReturnType<typeof projectMargins>,
        "probabilities" | "residual" | "iterations"
      >;
      if (s.models.forecast) {
        const generated = feasibleField({
          model: s.models.forecast,
          reference: reference[ch],
          cells: cells[ch],
          politicalScales,
          localScales: Float64Array.from({ length: n }, (_, j) =>
            profileValue(ch, cells[ch][0], j, "futureScales"),
          ),
          participationScale: scale.nonValid,
          discrepancy: 1.5 * fit.audit.electionDiscrepancy,
          discrepancyScales: multipliers.map(
            (m) => 1.5 * fit.audit.electionDiscrepancy * m,
          ),
          localMultipliers: cells[ch].map(
            (c) => referenceProfiles.get(c)!.localMultipliers,
          ),
          horizon,
          seed: s.seed,
          simulationId,
          chamber: ch,
          correlation,
          scaleMultiplier: s.models.forecastScale,
        });
        projected = {
          probabilities: (i) => generated.probabilities[i],
          residual: generated.projectionResidual,
          iterations: generated.projectionIterations,
        };
        audit.countryForecast[ch] = [...generated.country];
      } else {
        let futureQ =
          sum(q) > 0
            ? betaTransition(
                q,
                politicalScales,
                horizon,
                forecastRandom(
                  s.seed,
                  simulationId,
                  "political-movement",
                  ch,
                  correlation,
                ),
              )
            : q;
        // Election discrepancy is unit exposure, including at zero calendar horizon.
        if (sum(futureQ) > 0)
          futureQ = betaTransition(
            futureQ,
            ids.map(
              (_, j) => 1.5 * fit.audit.electionDiscrepancy * multipliers[j],
            ),
            1,
            forecastRandom(
              s.seed,
              simulationId,
              "election-discrepancy",
              ch,
              correlation,
            ),
          );
        const futureNv = betaTransition(
            nv,
            [scale.nonValid, scale.nonValid],
            horizon,
            forecastRandom(
              s.seed,
              simulationId,
              "non-valid-movement",
              ch,
              correlation,
            ),
          ),
          target = Float64Array.from([
            futureNv[0],
            ...futureQ.map((p) => futureNv[1] * p),
          ]);
        audit.countryForecast[ch] = [...target];
        const localScale = Math.exp(
            0.2 *
              forecastRandom(
                s.seed,
                simulationId,
                "local-scale",
                ch,
                correlation,
              ).normal(),
          ),
          regionNoise = new Map<string, Float64Array>();
        for (const region of new Set(cells[ch].map((c) => c.region))) {
          const rng = forecastRandom(
            s.seed,
            simulationId,
            "region:" + region,
            ch,
            correlation,
          );
          regionNoise.set(
            region,
            Float64Array.from({ length: n }, () => rng.normal()),
          );
        }
        const scores = (i: number) => {
          const c = cells[ch][i],
            rng = forecastRandom(
              s.seed,
              simulationId,
              "local:" + c.municipality,
              ch,
              correlation,
            ),
            localNoise = Float64Array.from({ length: n }, () => rng.normal());
          return Float64Array.from({ length: n }, (_, j) => {
            if (
              !(target[j] > 0) ||
              !(reference[ch][i][j] > 0) ||
              (j > 0 && !filedList(c, ids[j - 1]))
            )
              return -Infinity;
            const list = j
              ? listsById[ch].get(filedList(c, ids[j - 1])!)
              : undefined;
            const noise =
              profileValue(ch, c, j, "futureScales") *
              (list?.localUncertaintyMultiplier ?? 1) *
              localScale *
              Math.sqrt(horizon) *
              (Math.sqrt(0.85) * localNoise[j] +
                Math.sqrt(0.15) * regionNoise.get(c.region)![j]);
            return (
              Math.log(target[j]) +
              Math.log(reference[ch][i][j]) -
              Math.log(b[j]) +
              noise
            );
          });
        };
        const field = geographicField(ch, scores);
        projected = projectMargins(
          target,
          cells[ch].length,
          (i) => ({ weight: cells[ch][i].electors, scores: field(i) }),
          options,
        );
      }
      audit.projectionResidual = Math.max(
        audit.projectionResidual,
        projected.residual,
      );
      audit.projectionIterations = Math.max(
        audit.projectionIterations,
        projected.iterations,
      );
      for (
        let start = 0;
        start < cells[ch].length;
        start += options.chunkSize ?? 128
      )
        for (
          let i = start;
          i < Math.min(cells[ch].length, start + (options.chunkSize ?? 128));
          i++
        ) {
          const c = cells[ch][i],
            rng = forecastRandom(
              s.seed,
              simulationId,
              "multinomial:" + c.id,
              ch,
              correlation,
            ),
            componentCounts = c.electors
              ? multinomial(
                  c.electors,
                  [...projected.probabilities(i)],
                  rng.uniform,
                )
              : Array(n).fill(0);
          const row = counts.get(c.collegeId) ?? {
            electors: 0,
            votes: new Map<string, number>(),
          };
          row.electors += c.electors;
          for (const [list, positions] of listPositions.get(
            ch + ":" + c.collegeId,
          )!) {
            const count = sum(positions.map((j) => componentCounts[j]));
            row.votes.set(list, (row.votes.get(list) ?? 0) + count);
          }
          if (sum(componentCounts) !== c.electors)
            throw new Error("Integer elector conservation failed");
          counts.set(c.collegeId, row);
        }
    }
    for (const derived of context.seats.geography.colleges.filter(
      (c) => c.kind === "taa-proportional" && chambers.includes(c.chamber),
    )) {
      const row: CollegeCounts = { electors: 0, votes: new Map() };
      for (const c of context.seats.geography.colleges.filter(
        (c) =>
          c.chamber === derived.chamber &&
          c.territoryId === derived.territoryId &&
          c.kind === "special-uninominal",
      )) {
        const x = counts.get(c.id)!;
        row.electors += x.electors;
        for (const [id, n] of x.votes)
          row.votes.set(id, (row.votes.get(id) ?? 0) + n);
      }
      counts.set(derived.id, row);
    }
    const trace = structuredClone(baseTrace),
      result = assembleReformSimulation(
        { scenario: legalScenario, snapshot: context, bonusPlan },
        simulationId,
        counts,
        trace,
        {
          voteParameterVersion: FAST_GENERATION_VERSION,
          chamberModelId: "fast-share-paired-v1",
          chamberParameterVersion:
            assets.senateAdapter?.version ?? "camera-only",
          sourceVersions: [
            assets.sourceBundleId,
            assets.fittedStateId,
            assets.profileStateId,
            FAST_SHARE_MODEL,
            ...(assets.senateAdapter?.sourceVersions ?? []),
          ],
          assumptions: [
            ...(assets.senateAdapter?.assumptions ?? []),
            "Conditional municipal 18+ electorates are apportioned to physical cells using census population weights before generation; this is a labelled reconstruction, not observed submunicipal turnout. Electorate dates: " +
              [...new Set(assets.electorates.map((e) => e.date))]
                .sort()
                .join(", ") +
              ".",
            ...[...new Set(assets.electorates.map((e) => e.provenance))],
            s.models.forecast
              ? "Filing-aware forecast field " +
                s.models.forecast +
                "; national totals are derived from feasible physical probabilities before multinomial counts. Reference construction: " +
                (s.models.forecastReference ?? "conditional") +
                "; reference-profile regional variance share " +
                (s.models.profileRegionCorrelation ?? 0) +
                "; innovation scale " +
                (s.models.forecastScale ?? 1) +
                ". Explicit list-reference interventions preserve turnout."
              : "Global physical-cell probability projection before independent multinomial counts; fixed-turnout reference interventions. Central legal reference metadata reports effective probabilities after scoped overrides; sampled targets are retained separately in the fast audit.",
            "The legal ballot representation remains final-attributed-list-totals-v1; statistical model " +
              FAST_SHARE_MODEL +
              ".",
          ],
        },
      );
    return { ...result, audit };
  };
  return {
    scenario: s,
    componentIds: ids,
    cells,
    referencePlans: plans,
    centralReference: central,
    bonusPlan,
    run,
  };
}
