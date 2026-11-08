/** Strict authored-input loading and deterministic projections for transitional consumers. */
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { parsePoliticsScenario, validateActiveScenario, ScenarioValidationError } from '../../src/lib/politics/scenario/validation';
import { REFORM, ROSATELLUM, type PoliticsScenario } from '../../src/lib/politics/scenario/schema';
import { loadExecutionData, type ExecutionReaders } from '../../src/lib/politics/execution/assets';
import { commonVoteInputs } from '../../src/lib/politics/execution/inputs';
import { reformCoverage, reformLegalSettings } from '../../src/lib/politics/execution/reform';
import type { ReformAssemblyContext } from '../../src/lib/politics/reform-2026/generation-types';
import type { ReleasedScenario } from '../../src/lib/politics/reform-2026/vote-share/release';
import type { CompactSource } from '../../src/lib/politics/reform-2026/vote-share/source-types';
import type { Scenario } from '../../src/lib/core/types';
import type { PoliticsStaticSnapshot } from '../../src/lib/politics/types';
import { requireFastScenario } from '../../src/lib/politics/reform-2026/vote-share/scenario';
import { physicalCells } from '../../src/lib/politics/reform-2026/vote-share/physical-cells';
import { rosatellumCoverage, type RosatellumSeats } from '../../src/lib/politics/execution/rosatellum';
import type { SeatTableBundle } from '../../src/lib/politics/reform-2026/endowments';

export const readAuthoredPolitics = async (root: string) => parsePoliticsScenario(JSON.parse(await readFile(resolve(root, 'data/politics-default-scenario.json'), 'utf8')));
export async function validateAuthoredPolitics(s: PoliticsScenario, readers: ExecutionReaders) {
  for (const [law, model] of [[ROSATELLUM, 'M01'], [REFORM, 'M01'], [REFORM, 'M02']] as const) {
    const active: PoliticsScenario = { ...s, selection: { scrutiny: { ...s.selection.scrutiny, id: law },
      generatorsByLaw: { ...s.selection.generatorsByLaw, [law]: { ...s.selection.generatorsByLaw[law], id: model } } } };
    const data = await loadExecutionData(active, readers);
    const issues = validateActiveScenario(active, data.domain);
    if (issues.length) throw new ScenarioValidationError(issues);
    physicalCells(data.geography, data.electorates, ['camera', 'senato'], s.execution.simulationSeed);
    if (law === ROSATELLUM) rosatellumCoverage(active, data.seats as RosatellumSeats);
    else {
      const seats = data.seats as SeatTableBundle;
      if (seats.version !== active.data.lawAssets[law].seats.version || seats.geography.version !== data.geography.version)
        throw new Error('REFORM_SEATS_MISMATCH');
    }
  }
}
export async function reformDefaultInputs(s: PoliticsScenario, readers: ExecutionReaders, context: ReformAssemblyContext) {
  // M02 fitting is prepared even when an authored selection requests M01.
  const active: PoliticsScenario = { ...s, selection: { scrutiny: { ...s.selection.scrutiny, id: REFORM },
    generatorsByLaw: { ...s.selection.generatorsByLaw, [REFORM]: { ...s.selection.generatorsByLaw[REFORM], id: 'M02' } } } };
  const data = await loadExecutionData(active, readers);
  const inputs = commonVoteInputs(active, data, s.lawSettings[REFORM].chamberProfileId, reformCoverage(active, context));
  const legal = reformLegalSettings(active, data.domain, inputs.common);
  return { inputs, legal, data };
}
export function releasedDefault(s: PoliticsScenario, inputs: Awaited<ReturnType<typeof reformDefaultInputs>>, data: ReleasedScenario['data'], implementation: string): ReleasedScenario {
  const selected = s.selection.generatorsByLaw[REFORM], m01 = s.modelSettings.M01, m02 = s.modelSettings.M02;
  const generation = { model: selected.id, modelVersion: selected.version,
    compositions: Object.fromEntries(m01.compositions.map(c => [c.choiceId, { fractions: { ...c.fractions }, provenance: c.provenance, proxy: c.proxy }])),
    supplementalPolls: m01.supplementalPolls.map(p => ({ ...p, groups: p.groups.map(g => ({ ...g, members: [...g.members] })) })) };
  const result: ReleasedScenario = { ...inputs.inputs.fast, ...inputs.legal, lists: inputs.inputs.fast.lists, schemaVersion: 5, data,
    generation, historySelection: { electionIds: [...s.history.electionIds], pairedCamera: s.history.pairedCamera!, pairedSenato: s.history.pairedSenato! },
    sourceMemberships: inputs.inputs.common.sourceMemberships, componentMap: inputs.inputs.common.componentMap,
    polls: structuredClone(inputs.inputs.common.polls),
    models: { statistical: m02.statistical, numerical: m02.policies.numerical, references: m02.policies.references,
      chambers: m02.policies.chambers, rng: m02.policies.rng, forecast: m02.forecast, forecastScale: m02.forecastScale,
      profileRegionCorrelation: m02.profileRegionCorrelation, forecastReference: m02.forecastReference,
      generationImplementationId: implementation },
    presentation: { name: s.name, coalitions: s.subjects.coalitions.map(c => ({ ...c, chambers: [...c.chambers] })),
      components: [...s.subjects.components.map(c => ({ ...c })), ...s.newComponents.filter(c => inputs.inputs.fast.components.some(f=>f.id===c.id)).map(c => ({id:c.id,name:c.name,support:'reference' as const}))],
      lists: (['camera','senato'] as const).flatMap(chamber => s.subjects.lists.map(l => ({chamber,id:l.id,name:l.name,color:l.color}))) } };
  return requireFastScenario(result);
}
/** M00 UI compatibility until CP-06/08. This projection does not choose an independent default. */
export function legacyDefault(s: PoliticsScenario, snapshot: PoliticsStaticSnapshot, source: CompactSource, dataVersion: string): Scenario {
  const ros = s.lawSettings[ROSATELLUM];
  const paired = source.payload.events.find(e => e.id === s.history.pairedCamera);
  if (!paired) throw new Error('Missing authored conditional Camera reference');
  const nonValid = s.nonValidReferences.find(r => r.scope.kind === 'national' && r.channel !== 'senato' && r.appliesTo.includes('M01'));
  const constituency = (ch: 'camera'|'senato', location: typeof ros.filings[number]['locations'][number]) => snapshot.data[ch].uni.filter(u =>
    location.level === 'plurinominal' ? String(u.PLURI_COD) === location.code : String(u.UNI_COD) === location.code).map(u=>String(u.CIRC_COD));
  const names = new Map(s.subjects.lists.map(l=>[l.id,l.name]));
  const correspondences: Scenario['listCorrespondences'] = [];
  for (const c of source.payload.choices) {
    const event = source.payload.events.find(e=>e.id===c.eventId)!;
    const edit = s.history.sourceMemberships.find(m=>m.choiceId===c.id);
    const comp = s.modelSettings.M01.compositions.find(v=>v.choiceId===c.id);
    const members = edit?.members ?? c.members;
    const fractions = comp?.fractions ?? Object.fromEntries(members.map(id=>[id,1/members.length]));
    const destinations = new Map<string,number>();
    for (const [id, fraction] of Object.entries(fractions)) {
      const list = s.memberships.find(m=>m.camera.includes(id) || m.newComponentId===id);
      const name = list ? names.get(list.listId)! : 'astensione';
      destinations.set(name,(destinations.get(name)??0)+fraction);
    }
    for (const [futureList,factor] of destinations) if(factor>0) correspondences.push({ id:`authored:${c.id}:${futureList}`,futureList,
      pastElection:c.eventId.replace('camera-','camera ').replace('senato-','senato ').replace('european-','europee '),pastDate:event.date,pastList:c.label,factor,source:'bundled' });
  }
  return { id:s.id,name:s.name,electionDate:s.electionDate,defaultSource:{kind:'bundled',electionKind:'politiche',territory:'Italia',dataVersion,
    snapshotId:'politics-static.json',notes:[...s.provenance.assumptions,'Transitional M00 presentation uses Camera/common reference means; full common M01 execution is provided by the CP-03 boundary.']},
    globalShareMode:'mean',abstentionShare:(nonValid?.mean ?? 1 - paired.national.valid / paired.national.electors)*100,abstentionOverride:!!nonValid,
    coalitions:s.subjects.coalitions.map(c=>({id:c.id,name:c.name,color:c.color})),
    lists:s.subjects.lists.map(l=>{const member=s.memberships.find(m=>m.listId===l.id)!;
      const ref=s.references.find(r=>r.scope.kind==='national'&&r.channel!=='senato'&&(r.subject.id===l.id||r.subject.id===member.newComponentId));
      const members = new Set(member.camera.length ? member.camera : [member.newComponentId]);
      let pollMean: number | undefined;
      for (const poll of [...s.modelSettings.M01.supplementalPolls].filter(p=>p.enabled&&p.available<=s.referenceDate).sort((a,b)=>a.date.localeCompare(b.date))) {
        const groups=poll.groups.filter(g=>g.members.every(id=>members.has(id)));
        if(groups.length) pollMean=groups.reduce((n,g)=>n+g.share,0);
      }
      const mean=pollMean??ref?.mean;
      const historical=correspondences.some(c=>c.futureList===l.name&&c.factor>0);
      return {id:l.id,name:l.name,color:l.color,coalition:s.subjects.coalitions.find(c=>c.id===l.coalitionId)?.name??null,startingShare:(mean??0)*100,
        shareOverride:mean!==undefined&&!historical,filing:Object.fromEntries((['camera','senato'] as const).map(ch=>[ch,[...new Set(ros.filings.find(f=>f.listId===l.id)!.locations.filter(v=>v.chamber===ch).flatMap(v=>constituency(ch,v)))]])) as {camera:string[];senato:string[]},
        ...(l.minority?{minorityRegion:l.minority.regionId}:{})};}),
    listCorrespondences:correspondences,candidateTemplates:[],candidateGeneration:{...ros.candidateGeneration,plurinominalCandidacyCountShares:[...ros.candidateGeneration.plurinominalCandidacyCountShares] as Scenario['candidateGeneration']['plurinominalCandidacyCountShares']},
    localShareOverrides:s.references.filter(r=>r.scope.kind!=='national'&&r.channel!=='senato').flatMap(r=>{
      const m=s.memberships.find(m=>m.camera.includes(r.subject.id)||m.newComponentId===r.subject.id);
      const list=r.subject.kind==='list'?names.get(r.subject.id):m?names.get(m.listId):undefined;
      return list&&r.scope.kind!=='national'?[{id:r.id,scope:r.scope.kind,locationCode:r.scope.code,list,startingShare:r.mean*100}]:[];
    }) };
}
