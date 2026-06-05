import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const inputPath = resolve(repoRoot, 'web/static/data/v1/politics-static.json');
const outputPath = resolve(repoRoot, 'web/src/lib/scenario/politics-defaults.generated.ts');

const coalitionColorByName = new Map([
  ['sinistra', '#d94848'],
  ['centro', '#7a6bb2'],
  ['destra', '#3267b1'],
  ['PaP', '#7a3b2e']
]);

const listColorByName = new Map([
  ['+Europa', '#e9897e'],
  ['Alleanza Verdi Sinistra', '#44a36f'],
  ['Azione - Italia Viva', '#7a6bb2'],
  ['Forza Italia', '#5d8ed8'],
  ["Fratelli d'Italia", '#3267b1'],
  ['Lega', '#2f8a68'],
  ['Movimento 5 Stelle', '#d8b400'],
  ['Partito Democratico', '#d94848'],
  ['Potere al Popolo!', '#7a3b2e']
]);

const listIdByName = new Map([
  ['+Europa', 'europa'],
  ['Alleanza Verdi Sinistra', 'avs'],
  ['Azione - Italia Viva', 'azione-iv'],
  ['Forza Italia', 'fi'],
  ["Fratelli d'Italia", 'fdi'],
  ['Lega', 'lega'],
  ['Movimento 5 Stelle', 'm5s'],
  ['Partito Democratico', 'pd'],
  ['Potere al Popolo!', 'pap']
]);

const coalitionIdByName = new Map([
  ['sinistra', 'sinistra'],
  ['centro', 'centro'],
  ['destra', 'destra'],
  ['PaP', 'pap']
]);

const coalitionOrderByName = new Map([
  ['sinistra', 0],
  ['centro', 1],
  ['destra', 2],
  ['PaP', 3]
]);

function stableId(prefix, value, fallback) {
  const normalized = String(value ?? '')
    .trim()
    .toLowerCase()
    .normalize('NFKD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-|-$/g, '');
  return normalized ? `${prefix}-${normalized}` : `${prefix}-${fallback}`;
}

function dateInRome(value) {
  const date = new Date(value);
  if (!Number.isFinite(date.getTime())) return String(value ?? '');

  const parts = new Intl.DateTimeFormat('en-CA', {
    timeZone: 'Europe/Rome',
    year: 'numeric',
    month: '2-digit',
    day: '2-digit'
  }).formatToParts(date);
  const byType = new Map(parts.map((part) => [part.type, part.value]));
  return `${byType.get('year')}-${byType.get('month')}-${byType.get('day')}`;
}

function asciiJson(value) {
  return JSON.stringify(value, null, 2).replace(/[^\x00-\x7f]/g, (character) => {
    const code = character.charCodeAt(0).toString(16).padStart(4, '0');
    return `\\u${code}`;
  });
}

function normalizeFiveShares(values, fallback = [1, 0, 0, 0, 0]) {
  if (
    !Array.isArray(values) ||
    values.length !== 5 ||
    values.some((value) => !Number.isFinite(Number(value)) || Number(value) < 0)
  ) {
    return [...fallback];
  }

  const total = values.reduce((sum, value) => sum + Number(value), 0);
  if (total <= 0) return [...fallback];
  return values.map((value) => Number(value) / total);
}

function pluricandidatureFractionsToCandidateCountShares(fractions) {
  if (
    !Array.isArray(fractions) ||
    fractions.length !== 5 ||
    fractions.some((fraction) => !Number.isFinite(Number(fraction)) || Number(fraction) < 0) ||
    Number(fractions[0]) <= 0
  ) {
    return [1, 0, 0, 0, 0];
  }

  return normalizeFiveShares(
    fractions.map((fraction, index) => {
      const next = Number(fractions[index + 1] ?? 0);
      return (Number(fraction) - next) / Number(fractions[0]);
    })
  );
}

const snapshot = JSON.parse(readFileSync(inputPath, 'utf8'));
const scenario = snapshot.default_scenario;
const politicalLists = scenario.liste.filter((row) => row.LISTA !== 'astensione');
const politicalTotal = politicalLists.reduce((sum, row) => sum + row.PERCENTUALE, 0);
const abstentionShare = scenario.liste.find((row) => row.LISTA === 'astensione')?.PERCENTUALE ?? Math.max(1 - politicalTotal, 0);

const coalitions = (scenario.coalizioni ?? [])
  .map((row, index) => ({
    id: coalitionIdByName.get(row.COALIZIONE) ?? stableId('coalition', row.COALIZIONE, index + 1),
    name: row.COALIZIONE,
    color: coalitionColorByName.get(row.COALIZIONE) ?? row.COLORE ?? '#6f7f8f'
  }))
  .sort(
    (left, right) =>
      (coalitionOrderByName.get(left.name) ?? Number.MAX_SAFE_INTEGER) -
        (coalitionOrderByName.get(right.name) ?? Number.MAX_SAFE_INTEGER) || left.name.localeCompare(right.name)
  );

const lists = politicalLists.map((row, index) => ({
  id: listIdByName.get(row.LISTA) ?? stableId('list', row.LISTA, index + 1),
  name: row.LISTA,
  coalition: row.COALIZIONE,
  color: listColorByName.get(row.LISTA) ?? '#6f7f8f',
  startingShare: Number(((100 * row.PERCENTUALE) / politicalTotal).toFixed(2)),
  shareOverride: false
}));

const listCorrespondences = (scenario.corrispondenza_liste ?? []).map((row, index) => {
  const pastDate = dateInRome(row.DATA);
  return {
    id: stableId('correspondence', `${pastDate}-${row.ELEZIONE}-${row.LISTA_ORIGINALE}-${row.LISTA}`, index + 1),
    futureList: row.LISTA,
    pastElection: row.ELEZIONE,
    pastDate,
    pastList: row.LISTA_ORIGINALE,
    factor: row.FATTORE,
    source: 'bundled'
  };
});

const defaultScenario = {
  id: scenario.id,
  name: scenario.name,
  electionDate: dateInRome(scenario.data_elezione),
  defaultSource: {
    kind: 'bundled',
    electionKind: 'politiche',
    territory: 'Italia',
    dataVersion: 'v1',
    snapshotId: 'politics-static.json'
  },
  globalShareMode: 'mean',
  abstentionShare: Number((100 * abstentionShare).toFixed(2)),
  abstentionOverride: false,
  coalitions,
  lists,
  listCorrespondences,
  localShareOverrides: [],
  candidateTemplates: [],
  candidateGeneration: {
    uninominalToPlurinominalShare: scenario.frazione_uni_in_pluri ?? 0,
    plurinominalCandidacyCountShares: pluricandidatureFractionsToCandidateCountShares(
      scenario.frazioni_pluricandidature
    )
  }
};

const content = `// Generated by scripts/export_politics_scenario_defaults.mjs from web/static/data/v1/politics-static.json.
// Do not edit this file by hand.

import type { Scenario } from '$lib/core/types';

export const generatedDefaultPoliticsScenario = ${asciiJson(defaultScenario)} satisfies Scenario;
`;

writeFileSync(outputPath, content);
console.log(`Wrote ${outputPath}`);
