import { expect, test } from 'vitest';
import { fileURLToPath } from 'node:url';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { describeWithGeneratedFixtures, loadGeneratedJsonFixture } from '../test/generated-fixtures';
import { generatePoliticsVotes } from '../politics/vote-generation';
import { buildPoliticsDirectScrutinySnapshot } from '../politics/pipeline';
import { projectScenarioOntoPoliticsSource } from '../politics/scenario-projection';
import { buildPoliticsPipelineSourceFromSnapshot } from '../politics/static-snapshot';
import { runPoliticsSimulation } from '../politics/simulation-scrutiny';
import { runPoliticsScrutiny } from '../politics/scrutiny';
import type { PoliticsStaticSnapshot, PoliticsVoteGenerationSource, PoliticsScrutinyInput, PoliticsScrutinyContext } from '../politics/types';
import { createDefaultPoliticsScenario } from '../scenario/politics';
import { isFiled } from './filing';

test('filing conditions each chamber with shared draws and conserved valid mass', () => {
  const lists = [
    { LISTA: 'All', COALIZIONE: 'All', share: .4, filing: { camera: ['101', '102'], senato: ['1', '2'] } },
    { LISTA: 'Local', COALIZIONE: 'Local', share: .3, filing: { camera: ['101'], senato: ['1', '2'] } },
    { LISTA: 'astensione', COALIZIONE: null, share: .3 },
  ];
  const ramo = (ch: 'camera' | 'senato') => ({
    uni: [1, 2].map(n => ({ UNI_COD: n, CIRC_COD: ch === 'camera' ? 100 + n : n, PLURI_COD: n })),
    candidati_uni_sim: [1, 2].flatMap(n => lists.slice(0, 2).map(l => ({ SIM: 1, UNI_COD: n, COALIZIONE: l.COALIZIONE,
      LISTA_MINORANZA: null, CANDIDATO_ID: `${l.LISTA}-${n}`, DATA_NASCITA: '1980-01-01' }))),
    candidati_pluri_template: [1, 2].flatMap(n => lists.slice(0, 2).filter(l => ch === 'senato' || l.LISTA === 'All' || n === 1)
      .map(l => ({ LISTA: l.LISTA, PLURI_COD: n, CIRC_COD: ch === 'camera' ? 100 + n : n, MINORANZA: false }))),
  });
  const source: PoliticsVoteGenerationSource = {
    data_elezione: '2027-09-25', simulazioni: 1,
    liste: lists.map(l => ({ ...l, DATA: '2026-11-02', LOGIT_P: Math.log(l.share / (1 - l.share)), SIGMA_GLOBAL: 0 })),
    comuni_liste: [1, 2].flatMap(n => lists.map(l => ({ CODICE_COMUNE: n, LISTA: l.LISTA, DATA: '2026-11-02', DELTA: 0, SIGMA_DELTA: 0 }))),
    base_dati: [1, 2].map(n => ({ CODICE_COMUNE: n, CODITA_20N: n, ELETTORI: 1000, CU20_COD: n, SU20_COD: n })),
    camera: ramo('camera'), senato: ramo('senato'),
  };
  const result = generatePoliticsVotes(source, { seed: 'filing' });
  expect(result.camera.uni_liste_sim.some(r => r.UNI_COD === 2 && r.LISTA === 'Local')).toBe(false);
  expect(result.camera.uni_liste_sim.find(r => r.UNI_COD === 2 && r.LISTA === 'All')?.VOTI_LISTA_SIM).toBe(700);
  expect(result.senato.uni_liste_sim.find(r => r.UNI_COD === 2 && r.LISTA === 'Local')?.VOTI_LISTA_SIM).toBe(300);
  for (const ch of ['camera', 'senato'] as const) for (const unit of [1, 2])
    expect(result[ch].uni_liste_sim.filter(r => r.UNI_COD === unit).reduce((s, r) => s + r.VOTI_LISTA_SIM, 0)).toBe(700);

  // With an unrestricted choice set, typed fields must retain the original
  // normal-draw traversal and rounded output, including multiple simulations.
  const allFiled = structuredClone(source);
  allFiled.simulazioni = 3;
  allFiled.liste = allFiled.liste.map(l => ({ ...l, SIGMA_GLOBAL: .02, filing: { camera: ['101', '102'], senato: ['1', '2'] } }));
  allFiled.comuni_liste = allFiled.comuni_liste.map(l => ({ ...l, SIGMA_DELTA: .01 }));
  const historical = structuredClone(allFiled);
  historical.liste = historical.liste.map(({ filing: _filing, ...list }) => list);
  expect(generatePoliticsVotes(allFiled, { seed: 'typed-field-equivalence' })).toEqual(generatePoliticsVotes(historical, { seed: 'typed-field-equivalence' }));
  expect(() => generatePoliticsVotes({ ...allFiled, simulazioni: 10_000_000 })).toThrow('256 MiB');
});

const path = fileURLToPath(new URL('../../../static/data/v1/politics-static.json', import.meta.url));
test('retains the frozen Camera simulation 584 flipper failure independently of revised production geography', () => {
  const fixture = JSON.parse(gunzipSync(readFileSync(fileURLToPath(new URL('../../../../test/fixtures/politiche/legacy-camera-flipper-584.json.gz', import.meta.url)))).toString()) as {
    input: PoliticsScrutinyInput; context: PoliticsScrutinyContext; expected: string;
  };
  expect(fixture.input.liste_uni.every(r => Number.isFinite(r.VOTI_LISTA) && r.VOTI_LISTA >= 0)).toBe(true);
  expect(() => runPoliticsScrutiny(fixture.input, fixture.context)).toThrow(fixture.expected);
});

describeWithGeneratedFixtures('authored default through old-law assembly', [path], () => {
  test('files only declared votes/candidates, preserves minority metadata and completes both chambers', { timeout: 30_000 }, () => {
    const snapshot = loadGeneratedJsonFixture<PoliticsStaticSnapshot>(path), scenario = createDefaultPoliticsScenario();
    const projection = projectScenarioOntoPoliticsSource(buildPoliticsPipelineSourceFromSnapshot(snapshot), scenario, {
      currentDate: '2026-11-02', historicalVotes: snapshot.data.comuni_liste_elezioni, municipalities: snapshot.data.municipalities,
      parameterPercentualiPartenza: 'europee', simulations: 1,
    });
    const output = buildPoliticsDirectScrutinySnapshot(projection.source, { seed: 'authored-default-filing-v1' });
    for (const ch of ['camera', 'senato'] as const) {
      const data = output.rami[ch], input = data.simulations[0].input;
      for (const row of input.liste_uni) expect(isFiled(scenario.lists.find(l => l.name === row.LISTA)!, ch, row.CIRCOSCRIZIONE)).toBe(true);
      for (const row of input.candidati_pluri) expect(isFiled(scenario.lists.find(l => l.name === row.LISTA)!, ch, row.CIRCOSCRIZIONE)).toBe(true);
      for (const list of scenario.lists.filter(l => l.minorityRegion)) expect(data.liste_naz.find(l => l.LISTA === list.name)?.MINORANZA).toBe(true);
    }
    const scrutinized = runPoliticsSimulation({ algorithmId: 'politiche-r-parity-v1', simulationId: 1,
      chambers: (['camera', 'senato'] as const).map(ch => ({ input: output.rami[ch].simulations[0].input,
        context: { ramo: ch, liste_naz: output.rami[ch].liste_naz, totali_pluri: output.rami[ch].totali_pluri, totale_seggi: output.rami[ch].totale_seggi } })) });
    if (scrutinized.algorithmId !== 'politiche-r-parity-v1') throw new Error('Wrong law');
    expect(scrutinized.chambers.map(c => c.chamber)).toEqual(['camera', 'senato']);
  });
});
