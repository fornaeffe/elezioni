import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import type { Source } from './sources';
import { readSource, sha256 } from './sources';
import { prepareTerritoryCatalog, readManifest } from './normalize';
import { archiveMember, delimited, normalizeName } from './formats';
import { preparePairedHistory } from './reform-history';
import { fitPreferenceModel, prepareErPreferences, prepareEuropeanPreferences } from './reform-preferences';
import { getBundledReformSeatTables } from '../../src/lib/politics/reform-2026/bundled-data';
import { REFORM_MODEL_IDS, type ReformGenerationSnapshot, type ReformModelBundle } from '../../src/lib/politics/reform-2026/generation-types';
import { createDefaultReformScenario, validateReformSnapshot } from '../../src/lib/politics/reform-2026/generation-scenario';

export interface ReformSourceManifest { schemaVersion: 1; id: string; reviewedOn: string; sources: Source[] }
export async function readReformManifest(root: string): Promise<ReformSourceManifest> {
  const manifest: ReformSourceManifest = JSON.parse(await readFile(resolve(root, 'data/reform-model-source-manifest.json'), 'utf8'));
  if (manifest.schemaVersion !== 1 || new Set(manifest.sources.map(s => s.id)).size !== manifest.sources.length ||
      manifest.sources.some(s => !/^[a-f0-9]{64}$/.test(s.sha256) || !s.url.startsWith('https://') || !Number.isSafeInteger(s.bytes) || s.bytes <= 0)) throw new Error('Invalid reform source manifest');
  return manifest;
}

export async function prepareReformModels(root: string, directory: string) {
  const manifest = await readReformManifest(root), used = new Set<string>();
  const source = async (id: string) => {
    const entry = manifest.sources.find(s => s.id === id);
    if (!entry) throw new Error(`Missing reform source ${id}`);
    used.add(id); return readSource(directory, entry);
  };
  const { catalog } = await prepareTerritoryCatalog(root, await readManifest(root), directory);
  const seats = getBundledReformSeatTables();
  if (seats.geography.version !== catalog.version) throw new Error('Regenerate/register matching geography seat tables before reform models');
  const history = preparePairedHistory(await source('camera-2022'), await source('senato-2022'), catalog);
  const er = await prepareErPreferences(source);
  const european = prepareEuropeanPreferences(await source('european-2024'));
  const preferenceFit = fitPreferenceModel([...er.observations, ...european.observations]);
  if (!preferenceFit.audit.passed) throw new Error(`Preference fit fails the predeclared transfer gates: ${JSON.stringify(preferenceFit.audit.validation)}`);
  const oldSenate = delimited(archiveMember(await source('senato-2018'), 'Senato2018_livComune.txt'), ';');
  const sourceVersions = manifest.sources.filter(s => used.has(s.id)).map(s => `${s.id}:${s.sha256}`);
  const assumptions = [
    'The 2022 paired baseline uses the common 18+ electorate. One election does not identify future national forecast covariance; 2018 Senate 25+ data are audit-only.',
    '2022 direct list fields exclude candidate-only ballots. Attribute exactly at historical uninominal college, then reconstruct municipal additions proportionally with controlled integer rounding conserving both list-college and municipal candidate-only margins.',
    'Special-only historical ballot-group labels remain separate synthetic list references. No unobserved intra-coalition list decomposition is inferred.',
    'Municipal history is reconstructed onto reform colleges using legal-census population weights, including split municipalities; these are estimates of electoral support, not observed submunicipal votes.',
    'Future national/regional/local marginal logit scales per square-root day are assumed, not estimated from cross-municipality chamber correlations.',
    'Common-anchor contrasts are historical log-odds differences clipped to +/-1 and shrunk by 1/2. Their transfer from old candidate ballots is provisional.',
    'Preference intensity is pooled with equal election weight. Pooled roster concentration and within-person local dispersion use Jeffreys 1/2 smoothing and approximate count-noise subtraction. Regional pools shrink toward the common baseline with 10000 ballot pseudo-observations.',
    'Historical rosters have no protected capolista. The reform removes a filed position and uses six exchangeable synthetic strengths, without assigning any historical person to a future name.',
    'Valid combinations use a central maximum-entropy counting measure. Historical marginals do not identify pairing or triple-preference behavior. The empty combination is not a bonus-only turnout estimate.',
    'Candidate multiple candidacies are synthetic compatible same-list reuse. Coalition bonus affiliations follow filed ordinary prefixes, not proportional bonus seat quotas.',
    'Candidate outcomes are conditional on synthetic preferences. Profile sensitivity is not a calibrated confidence interval.'
  ];
  const pools = preferenceFit.pools.map(p => ({ ...p, scope: p.scope === '08' ? '08' : catalog.regions.find(r => normalizeName(r.name.split('/')[0]) === p.scope)?.id ?? p.scope }));
  const parameterVersion = `r26-preferences-fit-v1-${sha256(JSON.stringify({ baseline: preferenceFit.baseline, pools, sourceVersions })).slice(0, 12)}`;
  const fittedPersistence = preferenceFit.baseline.strengthSigma ** 2 / (preferenceFit.baseline.strengthSigma ** 2 + preferenceFit.baseline.localSigma ** 2 || 1);
  const models: ReformModelBundle = { schemaVersion: 1, version: '', modelIds: REFORM_MODEL_IDS, sourceVersions, assumptions,
    preferences: { parameterVersion, baseline: preferenceFit.baseline, pools, profiles: [
      { id: 'central-v1', intensityMultiplier: 1, concentrationMultiplier: 1, persistence: fittedPersistence, tripleWeight: 1 },
      { id: 'concentrated-v1', intensityMultiplier: 1, concentrationMultiplier: 1.4, persistence: fittedPersistence, tripleWeight: 1 },
      { id: 'dispersed-v1', intensityMultiplier: 1, concentrationMultiplier: 0.7, persistence: fittedPersistence, tripleWeight: 1 },
      { id: 'weak-persistence-v1', intensityMultiplier: 1, concentrationMultiplier: 1, persistence: 0.2, tripleWeight: 1 },
      { id: 'strong-persistence-v1', intensityMultiplier: 1, concentrationMultiplier: 1, persistence: 0.95, tripleWeight: 1 },
      { id: 'no-triples-v1', intensityMultiplier: 1, concentrationMultiplier: 1, persistence: fittedPersistence, tripleWeight: 0 },
      { id: 'enhanced-triples-v1', intensityMultiplier: 1, concentrationMultiplier: 1, persistence: fittedPersistence, tripleWeight: 3 }
    ] },
    chambers: { parameterVersion: `r26-paired-2022-assumptions-v1-${sha256(JSON.stringify({ audit: history.audit, municipalities: history.municipalities })).slice(0, 12)}`,
      sigmaPerSqrtDay: { national: 0.02, region: 0.01, local: 0.01 }, profiles: [
        { id: 'central-v1', sharedVariance: 0.5, residualCorrelation: 0.7, gapMultiplier: 1 },
        { id: 'weaker-dependence-v1', sharedVariance: 0.5, residualCorrelation: 0, gapMultiplier: 1 },
        { id: 'stronger-dependence-v1', sharedVariance: 0.5, residualCorrelation: 0.96, gapMultiplier: 1 },
        { id: 'zero-gap-v1', sharedVariance: 0.5, residualCorrelation: 0.7, gapMultiplier: 0 },
        { id: 'double-gap-v1', sharedVariance: 0.5, residualCorrelation: 0.7, gapMultiplier: 2 }
      ] } };
  models.version = `r26-models-v1-${sha256(JSON.stringify(models)).slice(0, 12)}`;
  const membershipMap = new Map<string, ReformGenerationSnapshot['memberships'][number]>();
  for (const { chamber, collegeId, municipalityCode, population } of catalog.memberships) {
    const key = JSON.stringify([chamber, collegeId, municipalityCode]), old = membershipMap.get(key);
    membershipMap.set(key, { chamber, collegeId, municipalityCode, population: population + (old?.population ?? 0) });
  }
  const snapshot: ReformGenerationSnapshot = { schemaVersion: 1, kind: 'politics-reform-generation', version: '', historicalDate: '2022-09-25', sourceVersions,
    assumptions, seats, models, historicalLists: history.historicalLists, historicalCoalitions: history.historicalCoalitions, municipalities: history.municipalities,
    memberships: [...membershipMap.values()] };
  snapshot.version = `r26-generation-v1-${sha256(JSON.stringify(snapshot)).slice(0, 12)}`;
  const errors = validateReformSnapshot(snapshot);
  if (errors.length) throw new Error(`Invalid prepared reform snapshot: ${errors.join('; ')}`);
  const defaults = createDefaultReformScenario(snapshot, '2026-10-19', '2027-09-25');
  const audit = { methodVersion: 'r26-preparation-v1', sourceManifest: manifest.id, sourceVersions,
    history: history.audit, preferenceSources: { er: er.audit, european: european.audit }, preferenceFit: preferenceFit.audit,
    olderSenate: { source: 'senato-2018', rows: oldSenate.length, minimumAge: 25, centralFit: false,
      reason: 'Age-confounded with Camera 18+; no demographic model or temporal-volatility identification.' },
    municipalities: snapshot.municipalities.length, assumptions, modelVersion: models.version, snapshotVersion: snapshot.version };
  return { snapshot, defaults, models, audit };
}
