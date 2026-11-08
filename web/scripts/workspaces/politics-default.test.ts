import { describe, expect, it } from 'vitest';
import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { readFile } from 'node:fs/promises';
import { authoredPoliticsDefault, createDefaultCommonPoliticsScenario } from '../../src/lib/politics/scenario/defaults';
import { declaredAssets, verifyAssetBytes } from '../../src/lib/politics/scenario/dependencies';
import { REFORM, ROSATELLUM } from '../../src/lib/politics/scenario/schema';
import { parsePoliticsScenario } from '../../src/lib/politics/scenario/validation';
import { legacyDefault, reformDefaultInputs, releasedDefault, validateAuthoredPolitics } from './politics-default';
import { decodeState } from '../../src/lib/politics/reform-2026/vote-share/state-codec';
import type { ReleaseCatalog, ScenarioState } from '../../src/lib/politics/reform-2026/vote-share/release';
import type { ReformAssemblyContext } from '../../src/lib/politics/reform-2026/generation-types';
import { loadExecutionData } from '../../src/lib/politics/execution/assets';
import { requireFastScenario } from '../../src/lib/politics/reform-2026/vote-share/scenario';

const root = fileURLToPath(new URL('../../../', import.meta.url)), s = authoredPoliticsDefault;
describe('single authored politics default', () => {
  it('materializes the approved law/model, run and candidature choices', () => {
    expect(s.id).toBe('politics-default-v1'); expect(s.name).toBe('Politiche — scenario nazionale');
    expect([s.electionDate,s.referenceDate,s.renewal]).toEqual(['2027-09-25','2026-10-19','joint']);
    expect(s.selection).toEqual({ scrutiny:{id:REFORM,version:'1.0.0'},generatorsByLaw:{[ROSATELLUM]:{id:'M01',version:'2.0.0'},[REFORM]:{id:'M02',version:'2.0.0'}} });
    expect(s.execution).toEqual({simulations:10,fittingSeed:'fast-share-default-fit-v1',simulationSeed:'fast-share-default-v1',workers:{kind:'auto',policy:'model-aware-v1'}});
    expect(s.lawSettings[ROSATELLUM].candidateGeneration).toEqual({uninominalToPlurinominalShare:0.35,plurinominalCandidacyCountShares:[0.875,0.0625,0.025,0.0125,0.025]});
    expect(s.lawSettings[REFORM].candidateGeneration).toEqual({reuseProbability:0.2,maximumOrdinaryCandidacies:5,bonusPolicy:'reference-proportional-v1'});
    expect(s.modelSettings.M02).toMatchObject({forecast:'bounded-mass-pair-field-v2',forecastScale:1.5,profileRegionCorrelation:0.25,forecastReference:'conditional'});
    expect(s.modelSettings.M02.uncertaintyMultipliers).toHaveLength(48);
    expect(s.modelSettings.M02.uncertaintyMultipliers.every(m=>m.national===1&&m.local===1)).toBe(true);
    expect(s.history.pooling.some(p=>p.id==='historical-rest'&&p.members.length>0)).toBe(true);
    expect(s.modelSettings.M01.compositions.some(c=>c.proxy)).toBe(true);
    expect(s.modelSettings.M01.supplementalPolls.length).toBeGreaterThan(0);
    expect(s.candidates).toEqual([]); expect(s.lawSettings[REFORM].suppliedVotes).toEqual([]);
  });
  it('stores the entire approved palette once and retains explicit chamber references and SDs', () => {
    expect(s.subjects.lists.map(l=>l.color)).toEqual(['#003366','#0088CC','#008040','#2456A6','#5DADE2','#F28C28','#315A8A','#D71920','#F5C400','#4A9F38','#D6348A','#E5B700','#E65B50','#B83B45','#0066A1','#172B50','#B5121B','#FFE000','#800020','#A6192E','#252525','#F2B800','#1976BC','#D52B2E']);
    expect(s.subjects.coalitions.map(c=>c.color)).toEqual(['#2563EB','#DC2626']);
    const camera = s.nonValidReferences.find(r=>r.channel==='camera')!, senato=s.nonValidReferences.find(r=>r.channel==='senato')!;
    expect(camera.mean).not.toBe(senato.mean);
    expect(camera.uncertainty?.sdPercentagePoints).toBe(0);
    expect(s.references.some(r=>r.uncertainty!.sdPercentagePoints>0)).toBe(true);
    for(const law of [ROSATELLUM,REFORM] as const) expect(s.lawSettings[law].filings.map(f=>f.listId)).toEqual(s.subjects.lists.map(l=>l.id));
    expect(s.lawSettings[ROSATELLUM].filings.some(f=>f.locations.some(l=>l.level==='uninominal'))).toBe(true);
    expect(s.lawSettings[ROSATELLUM].filings.flatMap(f=>f.locations).every(l=>l.geographyVersion===s.data.lawAssets[ROSATELLUM].geography.version)).toBe(true);
    const copy = createDefaultCommonPoliticsScenario() as any; copy.subjects.lists[0].color='#111111';
    expect(createDefaultCommonPoliticsScenario().subjects.lists[0].color).toBe('#003366');
  });
  it('rejects missing pins and incomplete baseline selections without default substitution', () => {
    const copy:any=createDefaultCommonPoliticsScenario(); delete copy.data.lawAssets[ROSATELLUM].seats;
    expect(()=>parsePoliticsScenario(copy)).toThrow();
    copy.data=s.data; delete copy.selection.generatorsByLaw[ROSATELLUM];
    expect(()=>parsePoliticsScenario(copy)).toThrow();
  });
});

const hasData = declaredAssets(s).every(p=>existsSync(resolve(root,'web/static',p.uri))) && existsSync(resolve(root,'web/static/data/reform-2026/fast-share/candidate.json'));
describe.skipIf(!hasData)('authored production packaging (requires npm run data:prepare)', () => {
  const asset = async (pin:typeof s.data.historical)=>new Uint8Array(await readFile(resolve(root,'web/static',pin.uri)));
  const dir=resolve(root,'web/static/data/reform-2026/fast-share'),read=async(path:string)=>new Uint8Array(await readFile(resolve(dir,path)));
  const readers={implementationId:'test',asset,sourceChunk:read};
  it('checks every declared pin, all three supported pairs, and fails closed on corrupted/incompatible data', async () => {
    for(const pin of declaredAssets(s)) await verifyAssetBytes(pin,await asset(pin));
    await validateAuthoredPolitics(s,readers);
    await expect(validateAuthoredPolitics(s,{...readers,asset:async()=>new Uint8Array([0])})).rejects.toThrow('CHECKSUM');
    const copy:any=createDefaultCommonPoliticsScenario(); copy.data.lawAssets[ROSATELLUM].models.M01=[];
    await expect(validateAuthoredPolitics(copy,readers)).rejects.toThrow('MODEL_ASSET_CONTRACT');
  });
  it('packages the same authored statistical/legal inputs and colors; edits propagate without another choice set', async () => {
    const data=await loadExecutionData(s,readers),context={seats:data.seats,models:data.parameters.models} as ReformAssemblyContext;
    const input=await reformDefaultInputs(s,readers,context);
    const pointer=JSON.parse(readFileSync(resolve(dir,'candidate.json'),'utf8'));
    const catalog=(await decodeState<ReleaseCatalog>(pointer.catalog,read,'fast-share-catalog')).state;
    const installed=(await decodeState<ScenarioState>(catalog.assets.scenario,read,'fast-share-scenario')).state.scenario;
    const projected=releasedDefault(s,input,installed.data,installed.models.generationImplementationId!);
    expect(requireFastScenario(projected)).toEqual(projected);
    expect(installed).toEqual(projected);
    const copy:any=createDefaultCommonPoliticsScenario(); copy.name='Edited source';copy.subjects.lists[0].color='#112233';copy.execution.simulationSeed='edited';
    const changed=releasedDefault(copy,await reformDefaultInputs(copy,readers,context),installed.data,installed.models.generationImplementationId!);
    expect(changed.presentation.name).toBe('Edited source');expect(changed.seed).toBe('edited');
    expect(changed.presentation.lists.filter(l=>l.id==='fdi').map(l=>l.color)).toEqual(['#112233','#112233']);
    const snapshot=JSON.parse(readFileSync(resolve(root,'web/static/data/v1/politics-static.json'),'utf8'));
    expect(legacyDefault(copy,snapshot,data.source,'test').lists[0].color).toBe('#112233');
  });
});
