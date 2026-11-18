import {test} from 'node:test';
import assert from 'node:assert/strict';
import {resolve,dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {loadData,categories,sum,eligible} from './data';
import {ilr,inverseIlr,score,rake} from './math';
import {prepareM02,makeDesign,m02Generator,endpointGenerator,pushContrast} from './models';
import {prepareM02Votes,type M02Settings} from '../../../../web/src/lib/politics/generation/m02';
import {FAST_SHARE_MODEL,FAST_SHARE_NUMERICS} from '../../../../web/src/lib/politics/reform-2026/vote-share/types';
import type {FastVoteAssets} from '../../../../web/src/lib/politics/reform-2026/vote-share/forecast-types';
import type {SourceGeography} from '../../../../web/src/lib/politics/reform-2026/vote-share/source-types';
const root=resolve(dirname(fileURLToPath(import.meta.url)),'../../../..');
test('generalized Helmert coordinates invert, conserve the simplex, and preserve six dimensions',()=>{
  const q=[.25,.1,.08,.15,.22,.2],p=inverseIlr(ilr(q));assert.equal(p.length,6);q.forEach((v,j)=>assert.ok(Math.abs(v-p[j])<1e-13));
});
test('scores use the predictive mean, 30 percentage points, inclusive coverage and the standard WIS weights',()=>{
  const exact=score([.2,.2,.2,.2],.2);assert.equal(exact.MAE,0);assert.equal(exact.WIS,0);assert.equal(exact.Coverage90,1);
  const outside=score([0,0,0,0],1);assert.equal(outside.error30,1);assert.equal(outside.Coverage90,0);assert.equal(outside.WIS,1);
  assert.equal(score([.25,.25],.5).error30,0);
  assert.ok(score([-.01,0,.01],0).WIS<score([-.2,0,.2],0).WIS);
});
test('IPF uses the supplied forecast weights and checks national margins',()=>{
  const p=[Float64Array.of(.8,.2),Float64Array.of(.3,.7)];rake(p,[2,1],Float64Array.of(.4,.6));
  assert.ok(Math.abs((2*p[0][0]+p[1][0])/3-.4)<1e-8);p.forEach(r=>assert.ok(Math.abs(sum(r)-1)<1e-13));
});
test('a zero chamber contrast preserves probabilities and a nonzero contrast stays on the simplex',()=>{
  const p=Float64Array.of(.4,.2,.4),q=pushContrast(p,new Float64Array(3));q.forEach((v,j)=>assert.ok(Math.abs(v-p[j])<1e-14));
  const r=pushContrast(p,Float64Array.of(.1,-.2,.3));assert.ok(Math.abs(sum(r)-1)<1e-14);assert.ok(r[0]>p[0]);
});
test('current M02 orchestration parity, shared Camera marginal parity, cutoff exclusion and endpoint determinism',async()=>{
  const data=await loadData(root),target=data.events.find(e=>e.id==='camera-2022')!,d=makeDesign(data,target,15,'mixed',4,'2018-age-audit'),seed='adapter-parity-v1';
  assert.ok(eligible(data,target.date,15,'mixed').history.every(e=>e.id!=='camera-2022'&&e.id!=='senato-2022'));
  assert.ok(d.polls.every(p=>p.available<=d.cutoff&&p.end<=d.cutoff));
  const p=prepareM02(data,d,seed),regions=[...new Set(d.rows.map(r=>r.region))],chambers=['camera','senato'] as const;
  const geography:SourceGeography={version:'research-parity-geo',censusDate:'2021-10-09',
    targets:d.rows.map(r=>({code:r.id,name:r.id,region:r.region,province:'',population:1})),
    colleges:chambers.flatMap(ch=>regions.map(r=>({id:ch+'/'+r,chamber:ch,territoryId:r,kind:'ordinary'}))),
    cells:chambers.flatMap(ch=>d.rows.map(r=>({chamber:ch,collegeId:ch+'/'+r.region,unitId:'research-whole-municipality',municipality:r.id,population:1})))};
  const lists=categories.map(id=>({id,name:id,coalitionId:id,protectedMinority:null,componentIds:[id]}));
  const s:M02Settings={generation:{model:'M02',modelVersion:'2.0.0',compositions:{},supplementalPolls:[]},seed,electionDate:d.date,renewal:'joint',
    coverage:geography.colleges.map(c=>({chamber:c.chamber,collegeId:c.id,listIds:categories})),chamberProfileId:'central-v1',referenceDate:d.cutoff,fitSeed:seed+':fit',
    models:{statistical:FAST_SHARE_MODEL,numerical:FAST_SHARE_NUMERICS,references:'fixed-turnout-reference-v1',chambers:'fast-share-paired-v1',rng:'fast-share-forecast-streams-v1',forecast:'bounded-mass-pair-field-v2',forecastScale:1.5,profileRegionCorrelation:.25,forecastReference:'conditional'},
    data:{sourceBundleId:'research-source',fittedStateId:'research-fit',profileStateId:'research-profile',geographyVersion:geography.version},
    components:categories.map(id=>({id,kind:'fitted',provenance:'Observed research family'})),lists:{camera:lists,senato:lists},references:[],nonValidReferences:[]};
  const assets:FastVoteAssets={fit:p.fit,cameraProfile:p.camera,senateAdapter:p.adapter!,modelId:FAST_SHARE_MODEL,numericalVersion:FAST_SHARE_NUMERICS,fitSeed:s.fitSeed,
    profileReferenceDate:d.cutoff,geography,electorates:d.rows.map(r=>({code:r.id,date:d.cutoff,camera:r.electors,senato:r.electors,provenance:'Conditional research electorate'})),sourceBundleId:'research-source',fittedStateId:'research-fit',profileStateId:'research-profile'};
  const production=prepareM02Votes(s,assets,{geography,models:{chambers:{profiles:[{id:'central-v1',...data.chamber}]}}});
  const current=m02Generator(data,d,p,seed,'M02'),shared=m02Generator(data,d,p,seed,'M02-shared');
  for(const draw of [1,2,7]) {
    const actual=production.run(draw),research=current(draw),simple=shared(draw);assert.deepEqual(simple.camera,research.camera);
    for(const ch of chambers)for(const region of regions) {
      const row=actual.counts.get(ch+'/'+region)!,indices=d.rows.flatMap((r,i)=>r.region===region?[i]:[]),field=ch==='camera'?research.camera:research.senato!;
      categories.forEach((id,j)=>assert.equal(sum(indices.map(i=>field[i][j+1])),row.votes.get(id),'active M02 count parity '+ch+'/'+region+'/'+id));
    }
  }
  for(const model of ['M05','M06','M07'] as const) {const g=endpointGenerator(data,d,seed,model);assert.deepEqual(g.generate(1),g.generate(1));}
  // Changing a held-out vote does not change the design or fitted inputs.
  const altered={...target,rows:target.rows.map(r=>({...r,votes:r.votes.map(v=>v+1)}))};assert.deepEqual(makeDesign(data,altered,15,'mixed',4,'2018-age-audit'),d);
});
