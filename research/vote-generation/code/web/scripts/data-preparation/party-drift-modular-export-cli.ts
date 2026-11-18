/** Research alternative: national dynamic fit plus empirical geographic profiles.
 * National likelihood retains original grouped counts. Profiles are estimated
 * features with predictive residuals, not a second independent count likelihood.
 */
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { loadPartyResearchData, prepareAdmissibleCase } from './party-drift-panel-data';
import { coarsenGroups } from './party-drift-panel';
import { basis, chol } from './drift-math';
import { calibrateReferenceOffset } from './party-drift-prior';
import { pollContrastFactor } from './party-drift-likelihood';
import { sha256 } from './sources';
import { membershipFor } from './party-drift-memberships';

const root=fileURLToPath(new URL('../../../',import.meta.url));
const input=await loadPartyResearchData(root),requested=process.argv[2]??'european-2024',horizons=process.argv[3]==='all'?[730,365,90,15]:[Number(process.argv[3]??15)],limit=Number(process.argv[4]??32);
const componentScope=process.argv[5]??'main';if(!['main','all'].includes(componentScope))throw new Error('Unknown component scope');
if(horizons.some(h=>!(h>0))||!(limit>=2))throw new Error('Positive horizon and at least two profile municipalities required');
const targets=requested==='development'?input.history.events.slice(2,6).map(e=>e.id):requested==='confirmation'?input.history.events.slice(6).map(e=>e.id):[requested];
const directories:string[]=[];
async function prepare(target:string,horizon:number){
const panel=prepareAdmissibleCase(input,target,horizon,Infinity,componentScope as 'main'|'all'),ids=panel.panel.ids,J=ids.length+1,D=J-1;
const position=(id:string)=>{const i=ids.indexOf(id);if(i<0)throw new Error('Unknown fitted identity');return i+1;};
const times=[...new Set([...panel.elections.map(e=>e.date),...panel.polls.map(p=>p.date),panel.cutoff])].sort((a,b)=>Date.parse(a)-Date.parse(b));
const H=Array.from({length:J},(_,i)=>Array.from(basis(J).slice(i*D,(i+1)*D)));
const aggregateEvent=(e:{id:string;date:string;kind:'camera'|'european';rows:typeof panel.truth})=>{
  const groups=new Map<string,{members:string[];count:number}>();let electors=0,nonValid=0;
  for(const row of e.rows){electors+=row.electors;nonValid+=row.nonValid;row.groups.forEach((g,i)=>{
    const key=[...g].sort().join('|'),a=groups.get(key)??{members:g,count:0};a.count+=row.counts[i];groups.set(key,a);
  });}
  const unique=[...groups.values()],coarse=coarsenGroups(unique.map(x=>x.members),unique.map(x=>x.count),x=>x);
  const grouping=Array(J).fill(0);grouping[0]=1;
  coarse.forEach((g,i)=>g.members.forEach(id=>grouping[position(id)]=i+2));
  const counts=[nonValid,...coarse.map(x=>x.value)];if(counts.reduce((s,v)=>s+v,0)!==electors)throw new Error('Aggregate count conservation');
  return{id:e.id,date:e.date,kind:e.kind,electors,groups:coarse.map(g=>g.members),counts,grouping};
};
const aggregate=panel.elections.map(aggregateEvent);
const nationalTruth=aggregateEvent({id:panel.targetId,date:panel.targetDate,kind:panel.targetKind,rows:panel.truth});
const periods=[...new Set(panel.polls.map(p=>p.group))];
const polls=panel.polls.map(p=>{
  const grouping=Array(J).fill(0);p.groups.forEach((g,k)=>g.forEach(id=>grouping[position(id)]=k+1));
  const n=p.groups.length,h=basis(n),l=chol(pollContrastFactor(p.shares,p.shares,p.sample).covariance,n-1);
  return{...p,grouping,n,contrast:Array.from({length:J},(_,i)=>Array.from({length:D},(_,j)=>i<n&&j<n-1?h[i*(n-1)+j]:0)),
    L:Array.from({length:D},(_,i)=>Array.from({length:D},(_,j)=>i<n-1&&j<n-1?l[i*(n-1)+j]:Number(i===j)))};
});
const calibration=calibrateReferenceOffset({components:D,cells:[{electors:1,available:Array.from({length:D},(_,i)=>i+1),kind:panel.targetKind}],nationalSd:1.5,localSd:0,kindSd:.5});
const data={J,U:times.length,E:aggregate.length,Q:polls.length,B:periods.length,H,state_offset:calibration.offset,
  dt:times.slice(1).map((x,i)=>(Date.parse(x)-Date.parse(times[i]))/(365.25*86400000)),
  event_time:aggregate.map(e=>times.indexOf(e.date)+1),event_kind:aggregate.map(e=>Number(e.kind==='european')),event_groups:aggregate.map(e=>e.counts.length),
  event_group:aggregate.map(e=>e.grouping),y:aggregate.map(e=>[...e.counts,...Array(J-e.counts.length).fill(0)]),
  poll_time:polls.map(p=>times.indexOf(p.date)+1),poll_kind:polls.map(p=>Number(p.kind==='european')),poll_bias:polls.map(p=>periods.indexOf(p.group)+1),
  poll_groups:polls.map(p=>p.n),poll_group:polls.map(p=>p.grouping),poll_log_shares:polls.map(p=>Array.from({length:J},(_,i)=>i<p.n?Math.log(p.shares[i]):0)),
  poll_H:polls.map(p=>p.contrast),poll_L:polls.map(p=>p.L),concentration:2000,use_kind:1};
const count=Math.min(limit,panel.cities.length),sample=Array.from({length:count},(_,i)=>panel.cities[Math.floor((i+.5)*panel.cities.length/count)]),selected=new Set(sample);
const listTemplates=new Map<string,{id:string;label:string;lastObserved:string;members:string[];sourceId:string}>();
const atomSources=new Map(ids.map(id=>[id,Object.keys(panel.panel.mapping).filter(raw=>panel.panel.mapping[raw]===id)]));
for(const e of input.history.events.filter(e=>e.date<panel.cutoff))for(const label of [...new Set(e.municipalities.flatMap(c=>c.lists.map(l=>l.name)))]){
  const raw=membershipFor(input.policy,e.id,label).members,members=[...new Set(raw.map(id=>panel.panel.mapping[id]))];
  if(members.some(id=>!id||id==='research-other'||(atomSources.get(id)??[]).some(x=>!raw.includes(x))))continue;
  const id=sha256(label).slice(0,12);listTemplates.set(id,{id,label,lastObserved:e.date,members,sourceId:e.sourceId});
}
const profiles={components:ids,cities:sample,cutoff:panel.cutoff,targetDate:panel.targetDate,targetKind:panel.targetKind,
  elections:panel.elections.map(e=>({...e,rows:e.rows.filter(r=>selected.has(r.id))})),truth:panel.truth.filter(r=>selected.has(r.id)),
  polls:panel.polls,nationalTruth,listTemplates:[...listTemplates.values()],
  nationalHistory:aggregate.map(e=>({id:e.id,date:e.date,kind:e.kind,electors:e.electors,groups:e.groups,counts:e.counts})),
  geographyPolicy:'Systematic sample of conditional scenario geography; missing histories remain missing; held-out counts are evaluation only'};
const files=['party-drift-modular-export-cli.ts','party-drift-panel-data.ts','party-drift-panel.ts','party-drift-memberships.ts','party-drift-prior.ts','party-drift-likelihood.ts',
  'stan/modular-party-drift.stan','stan/run_modular.py','stan/modular_geography.py','stan/fast_party_model.py','stan/simplex_gibbs.cpp','stan/run_fast.py','stan/fast_scenario.py'];
const sourceVersions=await Promise.all(files.map(async path=>({path,sha256:sha256(await readFile(resolve(root,'web/scripts/data-preparation',path)))})));
const version=sha256(JSON.stringify({inputs:input.versions,sourceVersions,target,horizon,limit,componentScope})).slice(0,12),dir=resolve(root,'data/derived/party-drift/modular-'+version);
await mkdir(dir,{recursive:true});
const bytes=JSON.stringify(data)+'\n',profileBytes=JSON.stringify(profiles)+'\n';
await writeFile(resolve(dir,'data.json'),bytes);await writeFile(resolve(dir,'profiles.json'),profileBytes);
await writeFile(resolve(dir,'manifest.json'),JSON.stringify({version,modelSource:'modular-party-drift.stan',modelHash:sourceVersions.find(x=>x.path.endsWith('.stan'))!.sha256,
  sourceVersions,inputs:input.versions,dataHash:sha256(Buffer.from(bytes)),profileHash:sha256(Buffer.from(profileBytes)),target,horizon,cutoff:panel.cutoff,componentScope,
  components:ids,nationalCountRows:aggregate.length,nationalTrainingMunicipalCells:panel.elections.reduce((s,e)=>s+e.rows.length,0),
  profileCities:sample.length,fullCities:panel.totalCities,calibration,scope:'Research modular empirical-Bayes alternative; national Dirichlet-multinomial dynamic model, geographic predictive residuals; not the former joint posterior',
  policy:{chains:4,warmup:512,sampling:512,adaptDelta:.95,maxTreedepth:12,seed:20261027,maxRhat:1.05,minEss:400}},null,2)+'\n');
console.log(JSON.stringify({directory:dir,J,U:times.length,countRows:aggregate.length,polls:polls.length,profileCities:sample.length,fullCities:panel.totalCities}));
directories.push(dir);
}
for(const target of targets)for(const horizon of horizons)await prepare(target,horizon);
if(directories.length>1){
  const version=sha256(JSON.stringify(directories)).slice(0,12),path=resolve(root,'data/derived/party-drift/modular-index-'+version+'.json');
  await writeFile(path,JSON.stringify({phase:requested,directories},null,2)+'\n');console.log(JSON.stringify({index:path,cases:directories.length}));
}
