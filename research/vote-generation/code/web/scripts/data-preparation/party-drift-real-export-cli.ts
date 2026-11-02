/** Real-data stage-0 pilot, conditional on versioned reference-prior scales.
 * Not a stage-1 winner or a production parameter bundle. */
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { basis, chol } from './drift-math';
import { loadPartyResearchData, prepareAdmissibleCase } from './party-drift-panel-data';
import { calibrateReferenceOffset } from './party-drift-prior';
import { pollContrastFactor } from './party-drift-likelihood';
import { componentResolution } from './party-drift-identifiability';
import { sha256 } from './sources';

const root=fileURLToPath(new URL('../../../',import.meta.url)),input=await loadPartyResearchData(root);
const args=process.argv.slice(2),cities=Number(args[0]??16),target=args[1]??'european-2024',horizon=Number(args[2]??15);
const variant=args[3]??'original';
if(!['original','collapsed','fixed'].includes(variant))throw new Error('Unknown parameterization');
const modelSource=variant!=='original'?'collapsed-party-drift.stan':'real-party-drift.stan';
if(!Number.isInteger(cities)||cities<2)throw new Error('Municipality pilot size must be an integer >=2');
const panel=prepareAdmissibleCase(input,target,horizon,cities),ids=panel.panel.ids,J=ids.length+1,D=J-1,C=panel.cities.length,T=panel.elections.length;
const index=(s:string)=>{const i=ids.indexOf(s);if(i<0)throw new Error('Unknown fitted component');return i+1;};
const resolution=componentResolution(ids,[...panel.elections.map(e=>({id:e.id,groups:e.rows.flatMap(c=>c.groups)})),...panel.polls.map(p=>({id:p.id,groups:p.groups}))]);
if(!resolution.resolved)throw new Error('Actual projected observations no longer resolve the reduced panel');
const times=[...new Set([...panel.elections.map(e=>e.date),...panel.polls.map(p=>p.date),panel.cutoff])].sort((a,b)=>Date.parse(a)-Date.parse(b));
const ref=Date.parse(panel.cutoff),years=(t:string)=>(ref-Date.parse(t))/(365.25*86400000),r=panel.elections.map(e=>years(e.date));
const H=Array.from({length:J},(_,i)=>Array.from(basis(J).slice(i*D,(i+1)*D)));
const cells:{c:number;t:number;group:number[];counts:number[];groups:number;electors:number;sourceLabels:string[][]}[]=[];
const initial=Array.from({length:C},()=>Array.from({length:T},()=>Array(D).fill(0)));
for(let t=0;t<T;t++)for(const row of panel.elections[t].rows){
  const c=panel.cities.indexOf(row.id),group=Array(J).fill(0);group[0]=1;
  row.groups.forEach((g,k)=>g.forEach(id=>{if(group[index(id)])throw new Error('Duplicate ballot component');group[index(id)]=k+2;}));
  const counts=[row.nonValid,...row.counts],offered=[1,...group.slice(1).filter(x=>x>0)];
  if(counts.reduce((s,v)=>s+v,0)!==row.electors)throw new Error('Non-conserving pilot source counts');
  row.groups.forEach((g,k)=>{const first=g.map(index).sort((a,b)=>a-b)[0];initial[c][t][first-1]=Math.log((row.counts[k]+.5)/(row.nonValid+.5));});
  cells.push({c:c+1,t:t+1,group,counts:[...counts,...Array(J-counts.length).fill(0)],groups:counts.length,electors:row.electors,sourceLabels:row.sourceLabels});
}
const biasGroups=[...new Set(panel.polls.map(p=>p.group))],Q=panel.polls.length;
const poll_data=panel.polls.map(p=>{
  const group=Array(J).fill(0);p.groups.forEach((g,k)=>g.forEach(id=>{if(group[index(id)])throw new Error('Duplicate poll component');group[index(id)]=k+1;}));
  const n=p.groups.length,h=basis(n),cov=pollContrastFactor(p.shares,p.shares,p.sample).covariance,l=chol(cov,n-1);
  return{time:times.indexOf(p.date)+1,group,groups:n,bias:biasGroups.indexOf(p.group)+1,
    logShares:Array.from({length:J},(_,i)=>i<n?Math.log(p.shares[i]):0),
    H:Array.from({length:J},(_,i)=>Array.from({length:D},(_,j)=>i<n&&j<n-1?h[i*(n-1)+j]:0)),
    L:Array.from({length:D},(_,i)=>Array.from({length:D},(_,j)=>i<n-1&&j<n-1?l[i*(n-1)+j]:Number(i===j)))};
});
const futureHistogram=new Map<number,number>();for(const c of panel.truth){const k=new Set(c.groups.flat()).size;futureHistogram.set(k,(futureHistogram.get(k)??0)+c.electors);}
// Marginal expectation is linear in municipalities. One representative cell per
// available-component count suffices for common independent reference scales.
const calibration=calibrateReferenceOffset({components:D,cells:[...futureHistogram].map(([k,electors])=>({electors,available:Array.from({length:k},(_,i)=>i+1)})),nationalSd:1.5,localSd:.5,kindSd:0});
const data={J,C,T,U:times.length,Q,N:cells.length,B:biasGroups.length,H,state_offset:calibration.offset,fit_scales:Number(variant!=='fixed'),fixed_sigma_n:.35,fixed_sigma_l:.25,
  dt:times.slice(1).map((t,i)=>(Date.parse(t)-Date.parse(times[i]))/(365.25*86400000)),
  count_time:panel.elections.map(e=>times.indexOf(e.date)+1),backward_cov:r.map(a=>r.map(b=>Math.min(a,b))),
  backward_all:times.map(a=>times.map(b=>Math.min(years(a),years(b)))),
  backward_cross:times.map(a=>r.map(b=>Math.min(years(a),b))),
  cell_c:cells.map(c=>c.c),cell_t:cells.map(c=>c.t),cell_group:cells.map(c=>c.group),cell_groups:cells.map(c=>c.groups),y:cells.map(c=>c.counts),
  poll_time:poll_data.map(p=>p.time),poll_group:poll_data.map(p=>p.group),poll_groups:poll_data.map(p=>p.groups),poll_bias:poll_data.map(p=>p.bias),
  poll_log_shares:poll_data.map(p=>p.logShares),poll_H:poll_data.map(p=>p.H),poll_L:poll_data.map(p=>p.L),
  // Every city/election gets a coordinate system; missing cells keep identity.
  coord_group:Array.from({length:C},(_,c)=>Array.from({length:T},(_,t)=>cells.find(x=>x.c===c+1&&x.t===t+1)?.group??Array(J).fill(0)))};
// Counts provide initialization and a diagonal metric ONLY. They remain the
// original grouped likelihood; these equal splits never become observations.
const mean=panel.elections.map((_,t)=>{
  const rows=cells.filter(x=>x.t===t+1),scores=rows.map(row=>{
    const v=initial[row.c-1][t],s=[0,...v];
    for(let g=2;g<=J;g++){
      const members=row.group.flatMap((x,i)=>x===g?[i]:[]);if(!members.length)continue;
      const first=members[0],z=Math.log(members.length);for(const j of members)s[j]=v[first-1]-z;
    }
    s[0]-=calibration.offset;
    return Array.from({length:D},(_,j)=>s.reduce((sum,x,i)=>sum+x*H[i][j],0));
  });return Array.from({length:D},(_,j)=>scores.reduce((sum,s)=>sum+s[j],0)/scores.length);
});
const national=times.map(time=>{
  const x=Date.parse(time),next=panel.elections.findIndex(e=>Date.parse(e.date)>=x);
  if(next<=0)return [...mean[next===0?0:T-1]];
  const a=Date.parse(panel.elections[next-1].date),b=Date.parse(panel.elections[next].date),w=(x-a)/(b-a);
  return mean[next].map((m,j)=>mean[next-1][j]*(1-w)+m*w);
});
const nationalRaw=national.map((v,i)=>v.map((x,j)=>variant!=='original'?0:i===times.length-1?x/1.5:(x-national[i+1][j])/(.35*Math.sqrt(data.dt[i]))));
const metric=[];
// Stan's unconstrained serializer visits each array element, then the matrix
// column/row. This DIFFERS from CSV column order (which interleaves cities).
for(let c=0;c<C;c++)for(let j=0;j<D;j++)for(let t=0;t<T;t++){
  const row=cells.find(x=>x.c===c+1&&x.t===t+1),g=row?.group[j+1]??0;
  const first=g>0&&row!.group.indexOf(g)===j+1;
  metric.push(first?1/(row!.counts[g-1]+.5)+1/(row!.counts[0]+.5):1);
  if(!row){const s=H.map(h=>h.reduce((sum,a,k)=>sum+a*mean[t][k],0));initial[c][t][j]=s[j+1]-s[0]-calibration.offset;}
}
metric.push(...Array((times.length+biasGroups.length)*D).fill(1),.25,.25);
const sources=['party-drift-real-export-cli.ts','party-drift-panel-data.ts','party-drift-panel.ts','party-drift-prior.ts','party-drift-likelihood.ts','stan/'+modelSource,'stan/run_real.py','stan/check_real_density.py'];
const sourceVersions=await Promise.all(sources.map(async path=>({path,sha256:sha256(await readFile(resolve(root,'web/scripts/data-preparation',path)))})));
const version=sha256(JSON.stringify({inputs:input.versions,sourceVersions,target,horizon,cities,variant})).slice(0,12),directory=resolve(root,'data/derived/party-drift/stan-real-'+version);
await mkdir(directory,{recursive:true});
const bytes=JSON.stringify(data)+'\n';await writeFile(resolve(directory,'data.json'),bytes);
await writeFile(resolve(directory,'initial.json'),JSON.stringify({coordinate:initial,log_sigma_n:Math.log(.35),log_sigma_l:Math.log(.25),national_raw:nationalRaw,bias_raw:Array.from({length:biasGroups.length},()=>Array(D).fill(0))})+'\n');
await writeFile(resolve(directory,'metric.json'),JSON.stringify({inv_metric:metric})+'\n');
const auxiliaryHashes=await Promise.all(['initial.json','metric.json'].map(async path=>({path,sha256:sha256(await readFile(resolve(directory,path)))})));
await writeFile(resolve(directory,'manifest.json'),JSON.stringify({version,variant,modelSource,modelHash:sourceVersions.find(s=>s.path.endsWith('.stan'))!.sha256,sourceVersions,inputs:input.versions,dataHash:sha256(Buffer.from(bytes)),auxiliaryHashes,
  target,horizon,cutoff:panel.cutoff,components:ids,cities:panel.cities,fullCities:panel.totalCities,countCells:cells.length,nationalTimes:times.length,polls:Q,calibration,resolution,
  scope:'Real-data common-volatility literal-link conditional-scale stage-0 pilot; no fit acceptance for stages 1-7',
  policy:{chains:4,warmup:1000,sampling:1000,adaptDelta:.95,maxTreedepth:12,seed:20261026,maxRhat:1.05,minEss:400}},null,2)+'\n');
console.log(JSON.stringify({directory,C,T,J,Q,cells:cells.length,parameters:(C*T+times.length+biasGroups.length)*D+2,fullCities:panel.totalCities}));
