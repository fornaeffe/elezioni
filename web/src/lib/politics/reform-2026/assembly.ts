import {
  REFORM_ALGORITHM_ID,
  type ChamberInput,
  type OrdinaryVotes,
  type Provenance,
  type ReformScrutinyInput,
  type SpecialVotes,
} from "./input";
import type {
  GenerationTrace,
  ReformAssemblyContext,
  ReformLegalSettings,
} from "./generation-types";
import { REFORM_MODEL_IDS } from "./generation-types";
import { GENERATION_STREAM_VERSION, sum } from "./generation-math";
import { generateReformCandidatures } from "./reform-candidate-generation";
import {
  generateSyntheticPreferences,
  preferenceParametersForRegion,
} from "./preference-generation";
import { validateReformScrutinyInput } from "./validation";
import type { PreparedBonusPlan } from "./reference-bonus-plan";
export interface CollegeCounts {
  electors: number;
  votes: Map<string, number>;
}
const renewedChamberIds = (s: Pick<ReformLegalSettings, "renewal">) =>
  s.renewal === "joint" ? (["camera", "senato"] as const) : [s.renewal];
export interface AssemblySource {
  voteParameterVersion: string;
  chamberModelId: "fast-share-paired-v1";
  chamberParameterVersion: string;
  sourceVersions: string[];
  assumptions: string[];
}
/** Shared final-attributed-count boundary. Statistical engines supply physical
 * college totals and prepared bonus support; candidate/preference/legal rules
 * are identical and never receive component-model or fit orchestration details. */
export function assembleReformSimulation(
  prepared: {
    scenario: ReformLegalSettings;
    snapshot: ReformAssemblyContext;
    bonusPlan?: PreparedBonusPlan;
  },
  simulationId: number,
  counts: Map<string, CollegeCounts>,
  trace: GenerationTrace,
  source?: AssemblySource,
): { input: ReformScrutinyInput; trace: GenerationTrace } {
  if (!Number.isSafeInteger(simulationId) || simulationId < 1)
    throw new RangeError("Expected a positive simulation ID");
  const { scenario: s, snapshot } = prepared;
  const candidatures = generateReformCandidatures(
    s,
    snapshot,
    simulationId,
    prepared.bonusPlan,
  );
  const provenance = (
    modelId: string,
    parameterVersion: string,
    profileId: string,
  ): Provenance => ({
    kind: "generated",
    modelId,
    parameterVersion,
    profileId,
    sourceVersions: [
      ...snapshot.sourceVersions,
      snapshot.version,
      snapshot.models.version,
      ...(s.pollReferences
        ? [s.pollReferences.archiveVersion, s.pollReferences.version]
        : []),
      ...(source?.sourceVersions ?? []),
    ],
    seed: s.seed,
    assumptions: [
      ...snapshot.assumptions,
      ...snapshot.models.assumptions,
      ...(s.conversion?.assumptions ?? []),
      ...(s.pollReferences?.assumptions ?? []),
      ...(source?.assumptions ?? []),
    ],
  });
  const voteProvenance = provenance(
    REFORM_MODEL_IDS.votes,
    source?.voteParameterVersion ?? snapshot.models.chambers.parameterVersion,
    s.chamberProfileId,
  );
  const preferenceProvenance = provenance(
    REFORM_MODEL_IDS.preferences,
    snapshot.models.preferences.parameterVersion,
    s.preferenceProfileId,
  );
  const chambers = renewedChamberIds(s).map((ch): ChamberInput => {
    const geo = snapshot.seats.chambers.find((c) => c.chamber === ch)!,
      candidates = candidatures.chambers.get(ch)!;
    const ordinaryVotes: ChamberInput["ordinaryVotes"][number][] = [],
      specialVotes: ChamberInput["specialVotes"][number][] = [];
    for (const college of geo.colleges) {
      const c = counts.get(college.id);
      if (!c) throw new RangeError(`Missing generated college ${college.id}`);
      const coverage = s.coverage.find(
        (r) => r.chamber === ch && r.collegeId === college.id,
      )!;
      const listVotes = coverage.listIds.map((listId) => ({
        listId,
        votes: c.votes.get(listId) ?? 0,
      }));
      const supplied = s.suppliedVotes.find(
        (r) => r.chamber === ch && r.collegeId === college.id,
      );
      const electorate = { minimumAge: 18 as const, electors: c.electors };
      if (college.kind === "special-uninominal") {
        if (supplied && !("candidateTotals" in supplied.votes))
          throw new RangeError(
            "Special vote override requires candidate totals",
          );
        const v: SpecialVotes = supplied
          ? (structuredClone(supplied.votes) as SpecialVotes)
          : {
              kind: "attributed",
              listVotes,
              provenance: voteProvenance,
              candidateTotals: candidates.specialCandidatures
                .filter((c) => c.collegeId === college.id)
                .map((candidate) => ({
                  candidateId: candidate.candidateId,
                  votes: sum(
                    listVotes
                      .filter((l) => candidate.linkedListIds.includes(l.listId))
                      .map((l) => l.votes),
                  ),
                })),
            };
        specialVotes.push({ collegeId: college.id, electorate, votes: v });
      } else {
        if (supplied && "candidateTotals" in supplied.votes)
          throw new RangeError(
            "Ordinary vote override cannot contain special candidate totals",
          );
        ordinaryVotes.push({
          collegeId: college.id,
          electorate,
          votes: supplied
            ? (structuredClone(supplied.votes) as OrdinaryVotes)
            : { kind: "attributed", listVotes, provenance: voteProvenance },
        });
      }
    }
    // Resolve the overlapping TAA view from the actual special override too.
    for (const college of geo.colleges.filter(
      (c) => c.kind === "taa-proportional",
    )) {
      if (
        s.suppliedVotes.some(
          (r) => r.chamber === ch && r.collegeId === college.id,
        )
      )
        continue;
      const r = ordinaryVotes.find((r) => r.collegeId === college.id)!;
      const physical = specialVotes.filter(
        (r) =>
          geo.colleges.find((c) => c.id === r.collegeId)?.territoryId ===
          college.territoryId,
      );
      const listVotes = s.coverage
        .find((c) => c.chamber === ch && c.collegeId === college.id)!
        .listIds.map((listId) => ({
          listId,
          votes: sum(
            physical.flatMap((p) =>
              (p.votes.kind === "components"
                ? p.votes.directListVotes
                : p.votes.listVotes
              )
                .filter((l) => l.listId === listId)
                .map((l) => l.votes),
            ),
          ),
        }));
      ordinaryVotes[ordinaryVotes.indexOf(r)] = {
        ...r,
        votes: { kind: "attributed", listVotes, provenance: voteProvenance },
      };
    }
    const people = new Map(candidatures.candidates.map((p) => [p.id, p]));
    const ordinarySlates = candidates.ordinarySlates.map((slate) => {
      const row = ordinaryVotes.find((r) => r.collegeId === slate.collegeId)!;
      const budget = (
        row.votes.kind === "components"
          ? row.votes.directListVotes
          : row.votes.listVotes
      ).find((r) => r.listId === slate.listId)?.votes;
      if (budget === undefined)
        throw new RangeError(
          "Preference generation has no explicit list ballot budget",
        );
      const supplied = s.suppliedPreferences.find(
        (p) =>
          p.chamber === ch &&
          p.collegeId === slate.collegeId &&
          p.listId === slate.listId,
      );
      const territory = geo.territories.find(
        (t) =>
          t.id ===
          geo.colleges.find((c) => c.id === slate.collegeId)!.territoryId,
      )!;
      const parameters = preferenceParametersForRegion(
        snapshot.models,
        territory.regionId,
      );
      return {
        ...slate,
        preferences: generateSyntheticPreferences({
          seed: s.seed,
          simulationId,
          chamber: ch,
          collegeId: slate.collegeId,
          listId: slate.listId,
          eligible: slate.candidateIds.slice(1).map((id) => people.get(id)!),
          ballotBudget: budget,
          parameters,
          profile: snapshot.models.preferences.profiles.find(
            (p) => p.id === s.preferenceProfileId,
          )!,
          provenance: preferenceProvenance,
          supplied: supplied?.preferences,
        }),
      };
    });
    const otherListCoverage = s.coverage
      .filter((c) => c.chamber === ch)
      .flatMap((c) =>
        c.listIds
          .filter((id) =>
            s.lists[ch].some((l) => l.id === id && l.kind === "other"),
          )
          .map((listId) => ({ collegeId: c.collegeId, listId })),
      );
    return {
      ...geo,
      ...candidates,
      ordinarySlates,
      ordinaryVotes,
      specialVotes,
      ...(otherListCoverage.length ? { otherListCoverage } : {}),
    };
  });
  const camera = chambers.find((c) => c.chamber === "camera"),
    senato = chambers.find((c) => c.chamber === "senato");
  const input: ReformScrutinyInput = {
    schemaVersion: 1,
    algorithmId: REFORM_ALGORITHM_ID,
    simulationId,
    electionDate: s.electionDate,
    legalVersion: "legge-180-2026",
    geographyVersion: snapshot.seats.geography.version,
    randomness: {
      seed: s.seed,
      streamVersion: `${GENERATION_STREAM_VERSION}/legal-v1`,
    },
    candidates: candidatures.candidates,
    renewal:
      s.renewal === "joint"
        ? { kind: "joint", camera: camera!, senato: senato! }
        : { kind: "single", chamber: chambers[0] },
    bonusLinks: s.renewal === "joint" ? s.bonusLinks : [],
    chamberModel: {
      kind: "generated",
      modelId: source?.chamberModelId ?? REFORM_MODEL_IDS.chambers,
      parameterVersion:
        source?.chamberParameterVersion ??
        snapshot.models.chambers.parameterVersion,
      covarianceProfileVersion: s.chamberProfileId,
      sourceVersions: [
        ...snapshot.sourceVersions,
        snapshot.version,
        snapshot.models.version,
        ...(s.pollReferences
          ? [s.pollReferences.archiveVersion, s.pollReferences.version]
          : []),
        ...(source?.sourceVersions ?? []),
      ],
      seed: s.seed,
      mode: "mean",
      assumptions: [
        ...snapshot.models.assumptions,
        ...snapshot.assumptions,
        ...(s.pollReferences?.assumptions ?? []),
        ...(source?.assumptions ?? []),
        ...(prepared.bonusPlan
          ? [
              `Bonus candidatures: ${prepared.bonusPlan.policy}, central support at ${prepared.bonusPlan.referenceDate}; constrained candidate allocations are not legal coalition-member entitlements.`,
            ]
          : []),
      ],
      references: trace.references,
    },
  };
  const checked = validateReformScrutinyInput(input);
  if (!checked.valid)
    throw new RangeError(
      `Generated reform input is invalid: ${checked.issues.map((i) => `${i.path}: ${i.message}`).join("; ")}`,
    );
  return { input: checked.input, trace };
}
