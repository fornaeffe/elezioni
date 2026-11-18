import {
  generatePairedMunicipalVotes,
  type ProjectedMunicipality
} from '../../src/lib/politics/reform-2026/paired-vote-generation';
import type {
  HistoricalMunicipality,
  ReformGenerationScenario,
  ReformGenerationSnapshot
} from '../../src/lib/politics/reform-2026/generation-types';

/** Research adapter for the actual, unchanged reform vote generator. The four
 * categories deliberately match the historical poll panel. Profile preparation
 * and national-reference reconciliation belong to the study, before this call. */
export interface BaselineGenerationInput {
  electionDate: string;
  cutoff: string;
  baselineDate: string;
  municipalities: readonly {
    id: string;
    regionId: string;
    electors: number;
    shares: readonly number[];
    validFraction: number;
  }[];
  nationalPoint: readonly number[];
  pollSigma?: readonly number[];
  seed: string;
}

const LIST_IDS = ['pd', 'm5s', 'lega', 'other'];

function validateInput(input: BaselineGenerationInput) {
  for (const date of [input.electionDate, input.cutoff, input.baselineDate]) {
    if (!Number.isFinite(Date.parse(date))) throw new Error(`Invalid baseline generation date: ${date}`);
    if (Date.parse(date) > Date.parse(input.electionDate)) throw new Error('Baseline reference follows election date');
  }
  const validShares = (shares: readonly number[]) => shares.length === LIST_IDS.length &&
    shares.every(p => Number.isFinite(p) && p >= 0 && p <= 1) && Math.abs(shares.reduce((a, b) => a + b, 0) - 1) < 1e-8;
  if (!validShares(input.nationalPoint)) throw new Error('Baseline national point must contain four shares summing to one');
  if (input.pollSigma && (input.pollSigma.length !== LIST_IDS.length || input.pollSigma.some(s => !Number.isFinite(s) || s < 0))) {
    throw new Error('Baseline polling sigmas must contain four nonnegative values');
  }
  const ids = new Set<string>();
  for (const municipality of input.municipalities) {
    if (ids.has(municipality.id)) throw new Error(`Duplicate baseline municipality: ${municipality.id}`);
    ids.add(municipality.id);
    if (!Number.isSafeInteger(municipality.electors) || municipality.electors <= 0 || !validShares(municipality.shares) ||
        !Number.isFinite(municipality.validFraction) || municipality.validFraction < 0 || municipality.validFraction > 1) {
      throw new Error(`Invalid baseline municipality: ${municipality.id}`);
    }
  }
}

/** Each draw is municipality-major [PD, M5S, Lega, other, valid/electors].
 * List shares use generated valid votes as denominator. Abstention is the
 * runtime convention including invalid/unmapped ballots, rather than turnout.
 *
 * National list references use cutoff; geographic and abstention references
 * retain baselineDate, exactly as refreshed poll references do in production.
 * A missing pollSigma supplies zero measurement error, not a fitted value.
 * To reproduce a wholly historical reference, set cutoff=baselineDate. */
export function createBaselineGenerator(input: BaselineGenerationInput): (simulationId: number) => Float64Array {
  validateInput(input);
  // generatePairedMunicipalVotes reads only these vote-generation fields. The
  // adapter does not fabricate or invoke legal seats, candidates or preferences.
  const scenario = {
    renewal: 'camera', chamberProfileId: 'central-v1', seed: input.seed,
    electionDate: input.electionDate
  } as ReformGenerationScenario;
  const snapshot = {
    historicalDate: input.baselineDate,
    models: { chambers: {
      sigmaPerSqrtDay: { national: 0.02, region: 0.01, local: 0.01 },
      profiles: [{ id: 'central-v1', sharedVariance: 0.5, residualCorrelation: 0.7, gapMultiplier: 1 }]
    } }
  } as ReformGenerationSnapshot;
  const prepared: ProjectedMunicipality[] = input.municipalities.map(m => {
    const historical = { electors: m.electors,
      votes: LIST_IDS.map((listId, i) => ({ listId, votes: m.electors * m.validFraction * m.shares[i] })) };
    const municipality: HistoricalMunicipality = { code: m.id, regionId: m.regionId,
      provinceId: m.regionId, name: m.id, camera: historical, senato: historical };
    const reference = () => ({ shares: new Map(LIST_IDS.map((id, i) => [id, m.shares[i]])),
      abstention: 1 - m.validFraction, dates: new Map(LIST_IDS.map(id => [id, input.cutoff])),
      // Presence selects the runtime's separate historical geographic date.
      measurement: new Map(LIST_IDS.map((id, i) => [id, input.pollSigma?.[i] ?? 0])),
      abstentionDate: input.baselineDate });
    return { municipality, camera: reference(), senato: reference() };
  });
  return simulationId => {
    if (!Number.isSafeInteger(simulationId) || simulationId < 0) throw new Error('Invalid baseline simulation ID');
    const rows = generatePairedMunicipalVotes(scenario, snapshot, prepared, simulationId);
    const output = new Float64Array(input.municipalities.length * 5);
    rows.forEach((row, municipalityIndex) => {
      const valid = row.votes.reduce((a, b) => a + b.votes, 0);
      row.votes.forEach((vote, listIndex) => { output[municipalityIndex * 5 + listIndex] = valid ? vote.votes / valid : 0; });
      output[municipalityIndex * 5 + 4] = valid / row.electors;
    });
    return output;
  };
}

export function generateBaselineDraw(input: BaselineGenerationInput, simulationId: number): Float64Array {
  return createBaselineGenerator(input)(simulationId);
}

export function baselineSamples(input: BaselineGenerationInput & { draws: number }): Float64Array[] {
  if (!Number.isSafeInteger(input.draws) || input.draws < 1) throw new Error('Baseline draws must be a positive integer');
  const generate = createBaselineGenerator(input);
  return Array.from({ length: input.draws }, (_, simulationId) => generate(simulationId));
}
