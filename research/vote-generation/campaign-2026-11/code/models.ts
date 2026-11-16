import {categories as ids, totals, sum, years, eligible, type StudyData, type Event, type Row, type Poll} from './data';
import {fitReference} from '../../../../web/src/lib/politics/reform-2026/vote-share/information';
import {fitProfiles, type MunicipalObservation} from '../../../../web/src/lib/politics/reform-2026/vote-share/profiles';
import {prepareSenateAdapter, adaptSenateReference, type PairedHistory} from '../../../../web/src/lib/politics/reform-2026/vote-share/senate-adapter';
import {forecastRandom} from '../../../../web/src/lib/politics/reform-2026/vote-share/forecast-random';
import {referenceNoise} from '../../../../web/src/lib/politics/reform-2026/vote-share/reference-noise';
import {movementScales} from '../../../../web/src/lib/politics/reform-2026/vote-share/transition';
import {feasibleField} from '../../../../web/src/lib/politics/reform-2026/vote-share/feasible-field';
import {multinomial, apportion} from '../../../../web/src/lib/politics/reform-2026/generation-math';
import {generatePairedMunicipalVotes, preparePairedVoteReferences} from '../../../../web/src/lib/politics/generation/m01';
import {refreshM01Polls} from '../../../../web/src/lib/politics/generation/m01-polls';
import type {M01Data,M01Settings} from '../../../../web/src/lib/politics/generation/types';
import type {PhysicalCell,GeographicProfile,SenateAdapter} from '../../../../web/src/lib/politics/reform-2026/vote-share/forecast-types';
import type {NationalElection,NationalSource} from '../../../../web/src/lib/politics/reform-2026/vote-share/types';
import {logistic,logit,normalize,softmax,ilr,inverseIlr,matrix,outerAdd,factor,rake} from './math';

export const modelIds=['M01','M02','M02-shared','M05','M06','M07'] as const;
export type Model=typeof modelIds[number];
export interface Design {cutoff:string; date:string; horizon:number; regime:string; rows:Row[]; history:Event[]; polls:Poll[]; baseline:Event; kind:'camera'|'european'; pairMode:'none'|'2018-age-audit'|'2022-reconstruction'}
export interface Draw {camera:Float64Array[]; senato?:Float64Array[]}
export function makeDesign(data:StudyData,target:Event,horizon:number,regime:string,limit=0,pairMode:Design['pairMode']='none'):Design {
  const {cutoff,history,polls}=eligible(data,target.date,horizon,regime);
  const baseline=history.at(-1)!, truth=new Set(target.rows.map(r=>r.id));
  // A fixed geography/matching domain is allowed; future votes/electorates are
  // not exposed to the generator. Counts are conditional on baseline electorates.
  let rows=baseline.rows.filter(r=>truth.has(r.id));
  if(pairMode!=='none') {const s=data.events.find(e=>e.id==='senato-2022')!;const paired=new Set(s.rows.map(r=>r.id));rows=rows.filter(r=>paired.has(r.id));}
  if(limit>0&&rows.length>limit)rows=rows.filter((_,i)=>i%Math.ceil(rows.length/limit)===0);
  return {cutoff,date:target.date,horizon,regime,rows,history,polls,baseline,kind:target.kind as 'camera'|'european',pairMode};
}
const observation=(r:Row,offered:boolean[]):MunicipalObservation=>({id:r.id,region:r.region,electors:r.electors,nonValid:r.electors-sum(r.votes),groups:ids.filter((_,j)=>offered[j]).map(id=>[id]),counts:r.votes.filter((_,j)=>offered[j])});
export function national(e:Event):NationalElection {const t=totals(e.rows);return {id:e.id,date:e.date,context:e.kind as 'camera'|'european',electors:t.electors,groups:ids.filter((_,j)=>e.offered[j]).map(id=>[id]),counts:[t.electors-sum(t.votes),...t.votes.filter((_,j)=>e.offered[j])]};}
function pairHistory(data:StudyData,mode:Design['pairMode']):PairedHistory|null {
  if(mode==='none')return null;
  const year=mode==='2018-age-audit'?'2018':'2022',c=data.events.find(e=>e.id==='camera-'+year)!,s=data.events.find(e=>e.id==='senato-'+year)!;
  const senate=new Map(s.rows.map(r=>[r.id,r])),rows:PairedHistory['rows']=[];
  for(const r of c.rows) {const q=senate.get(r.id);if(!q)continue;
    // Adapter's equal-electorate check is waived ONLY for this age-confounded
    // research sensitivity. Rescale all outcomes together, conserving counts.
    const counts=apportion(r.electors,[q.electors-sum(q.votes),...q.votes],forecastRandom('age-audit-denominator',0,r.id).uniform);
    const sr={...q,electors:r.electors,votes:counts.slice(1)};
    rows.push({municipality:r.id,camera:observation(r,c.offered),senato:observation(sr,s.offered)});
  }
  return {date:c.date,availableDate:c.available,minimumAge:18,sourceVersions:[data.source.id,mode+':research-only-age/electorate-waiver'],rows};
}
export function makeContrast(data:StudyData,d:Design,profile:GeographicProfile):SenateAdapter|null {
  const pair=pairHistory(data,d.pairMode); if(!pair)return null;
  if(d.pairMode==='2018-age-audit'&&pair.availableDate>d.cutoff)throw new Error('FUTURE_PAIR_LEAK');
  // 2022 reconstruction is deliberately in-sample and labelled separately.
  return prepareSenateAdapter(profile,d.pairMode==='2022-reconstruction'?'2022-09-26':d.cutoff,pair);
}
export function endpointContrast(data:StudyData,d:Design) {
  const camera=fitProfiles({components:ids,referenceDate:d.cutoff,targetContext:d.kind,nationalElections:[],elections:[],
    targets:d.rows.map(r=>({id:r.id,region:r.region,electors:r.electors}))});
  const contrast=makeContrast(data,d,camera);
  if(contrast)Object.assign(contrast,{researchCameraMeans:camera.means});
  return contrast;
}
export function prepareM02(data:StudyData,d:Design,seed:string) {
  const elections=d.history.map(national),source:NationalSource={components:ids,referenceDate:d.cutoff,elections,
    polls:d.polls.map(p=>({id:p.id,date:p.date,context:'camera',groups:p.groups,shares:p.shares,sample:p.sample,errorGroup:p.errorGroup,availableDate:p.available,fieldworkEnd:p.end}))};
  const fit=fitReference(source,seed+':fit');
  if(!fit.political.diagnostics.passed||!fit.participation.diagnostics.passed)throw new Error('FIT_NOT_ACCEPTED');
  const camera=fitProfiles({components:ids,referenceDate:d.cutoff,targetContext:d.kind,nationalElections:elections,
    elections:d.history.map(e=>({id:e.id,date:e.date,context:e.kind as 'camera'|'european',rows:e.rows.map(r=>observation(r,e.offered))})),targets:d.rows.map(r=>({id:r.id,region:r.region,electors:r.electors}))});
  const adapter=makeContrast(data,d,camera);
  return {fit,camera,adapter};
}
export type PreparedM02=ReturnType<typeof prepareM02>;
export function cellsFor(d:Design,ch:'camera'|'senato'):PhysicalCell[] {return d.rows.map(r=>({id:JSON.stringify([ch,r.id,'research-whole-municipality',ch+'/'+r.region]),municipality:r.id,region:r.region,province:'',collegeId:ch+'/'+r.region,chamber:ch,electors:r.electors}));}
/** Active M02 2.0.0 kernels on research families and municipal cells. National
 * coarsening, conditional profile field, stream addresses and count law match
 * the current implementation; legal assembly is outside this experiment. */
export function m02Generator(data:StudyData,d:Design,p:PreparedM02,seed:string,mode:Model|'M02-row-sync'|'M02-rho1') {
  const rho=mode==='M02-rho1'?1:data.chamber.sharedVariance+(1-data.chamber.sharedVariance)*data.chamber.residualCorrelation;
  const n=ids.length+1,k=ids.length,position=new Map(p.camera.municipalityIds.map((id,i)=>[id,i])),cs=cellsFor(d,'camera'),ss=cellsFor(d,'senato');
  const gaps=d.rows.map(r=>{const i=position.get(r.id)!*n;return Float64Array.from({length:n},(_,j)=>p.adapter ? (j===0?p.adapter.nonValidLogOdds:p.adapter.nationalLogRatios[j-1])+p.adapter.profile.means[i+j]-p.camera.means[i+j]:0);});
  return (draw:number):Draw=>{
    const noise=referenceNoise(seed,draw,n,rho,.25);
    const field=(ch:'camera'|'senato')=>{
      const sync=mode==='M02-row-sync',rowCh=sync?'camera':ch;
      const row=forecastRandom(seed,draw,'posterior-political-row',rowCh,rho).integer(p.fit.political.chains*p.fit.political.drawsPerChain);
      const row0=forecastRandom(seed,draw,'posterior-non-valid-row',rowCh,rho).integer(p.fit.participation.chains*p.fit.participation.drawsPerChain);
      const contextOffset=d.kind==='european'?k:0,nonValidOffset=d.kind==='european'?2:0;
      let q=p.fit.political.values.slice(row*2*k+contextOffset,row*2*k+contextOffset+k),a=p.fit.participation.values[row0*4+nonValidOffset];
      if(ch==='senato'&&p.adapter) {const adapted=adaptSenateReference(q,a,p.adapter,1);q=adapted.political;a=adapted.nonValid;}
      const b=Float64Array.from([a,...q.map(v=>(1-a)*v)]);
      const prof=ch==='camera'?p.camera:p.adapter?.profile??p.camera;
      const reference=d.rows.map(r=>{const at=position.get(r.id)!*n,z=noise(ch,r.id,r.region);return softmax(b.map((v,j)=>Math.log(v)+prof.means[at+j]+prof.profileSd[at+j]*z[j]));});
      const scale=movementScales(p.fit.audit.scales.political,p.fit.audit.scales.participation,forecastRandom(seed,draw,'movement-scales',ch,rho));
      return feasibleField({model:'bounded-mass-pair-field-v2',reference,cells:ch==='camera'?cs:ss,politicalScales:new Float64Array(k).fill(scale.political),
        localScales:prof.futureScales,participationScale:scale.nonValid,discrepancy:1.5*p.fit.audit.electionDiscrepancy,
        horizon:years(d.date,d.cutoff),seed,simulationId:draw,chamber:ch,correlation:rho,scaleMultiplier:1.5}).probabilities as Float64Array[];
    };
    const c=field('camera');
    const s=d.pairMode==='none'?undefined:mode==='M02-shared'?c.map((v,i)=>pushContrast(v,gaps[i])):field('senato');
    return {camera:countField(c,d,seed,draw,'camera',rho),senato:s?countField(s,d,seed,draw,'senato',rho):undefined};
  };
}
/** A single common political/turnout realization followed by the observed
 * (shrunk) ballot contrast. This removes duplicate posterior and movement paths.
 * Political contrast acts on valid shares; turnout contrast on non-valid odds. */
export function pushContrast(v:Float64Array,gap:Float64Array) {
  const a=logistic(logit(v[0])+gap[0]),q=normalize(v.slice(1).map((x,j)=>x*Math.exp(gap[j+1])));
  return Float64Array.from([a,...q.map(x=>(1-a)*x)]);
}
export function countField(field:Float64Array[],d:Design,seed:string,draw:number,ch:'camera'|'senato',rho=0) {
  return field.map((p,i)=>Float64Array.from(multinomial(d.rows[i].electors,[...p],forecastRandom(seed,draw,'multinomial:'+JSON.stringify([ch,d.rows[i].id,'research-whole-municipality',ch+'/'+d.rows[i].region]),ch,rho).uniform)));
}
export function m01Generator(data:StudyData,d:Design,seed:string) {
  const baseline=d.history.filter(e=>e.kind==='camera').at(-1)!;if(!baseline)throw new Error('NO_CAMERA_BASELINE');
  const base=new Map(baseline.rows.map(r=>[r.id,r])),regions=[...new Set(d.rows.map(r=>r.region))];
  const regional=new Map(regions.map(region=>{const t=totals(baseline.rows.filter(r=>r.region===region));return [region,{id:'regional-proxy:'+region,region,...t}];}));
  const profiles=[{...data.chamber,id:'central-v1'}],geography={version:'research-regional-filing-v1',colleges:['camera','senato'].flatMap(ch=>regions.map(r=>({id:ch+'/'+r,chamber:ch as 'camera'|'senato',territoryId:r,kind:'ordinary'})))};
  const rows=d.rows.map(r=>{const b=base.get(r.id)??regional.get(r.region)!;
    const counts=apportion(r.electors,[b.electors-sum(b.votes),...b.votes],forecastRandom(seed,0,'baseline:'+r.id).uniform);
    const v={electors:r.electors,votes:ids.map((listId,j)=>({listId,votes:counts[j+1]}))};return {code:r.id,regionId:r.region,provinceId:'',name:r.id,camera:v,senato:structuredClone(v)};});
  // The historical Senate baseline remains explicit and age-confounded in the
  // paired out-of-time sensitivity; do not silently label it 18+ evidence.
  if(d.pairMode!=='none') {
    const pair=pairHistory(data,d.pairMode)!,paired=new Map(pair.rows.map(r=>[r.municipality,r]));
    for(const r of rows) {const p=paired.get(r.code);if(!p)continue;const sv=sum(p.senato.counts),cv=sum(p.camera.counts),q=ids.map(id=>{const j=p.senato.groups.findIndex(g=>g.includes(id));return j<0?0:p.senato.counts[j]/sv;});
      const valid=r.camera.votes.reduce((a,b)=>a+b.votes,0)*sv/p.senato.electors/(cv/p.camera.electors);
      const counts=apportion(r.camera.electors,[Math.max(0,r.camera.electors-valid),...q.map(x=>x*valid)].map(x=>Math.round(x*1e6)),forecastRandom(seed,0,'senate-baseline:'+r.code).uniform);
      r.senato={electors:r.camera.electors,votes:ids.map((listId,j)=>({listId,votes:counts[j+1]}))};}
  }
  const snapshot:M01Data={version:'research-M01-2.0.0',historicalDate:baseline.date,municipalities:rows,
    memberships:rows.flatMap(r=>(['camera','senato'] as const).map(ch=>({chamber:ch,municipalityCode:r.code,collegeId:ch+'/'+r.regionId,population:1}))),geography,
    models:{version:'research',chambers:{parameterVersion:'current-pinned',profiles,sigmaPerSqrtDay:data.sigma}}};
  const lists=ids.map(id=>({id,name:id,coalitionId:id,protectedMinority:null,componentIds:[id]}));
  const scenario:M01Settings={seed,electionDate:d.date,projectionDate:d.cutoff,renewal:d.pairMode==='none'?'camera':'joint',lists:{camera:lists,senato:lists},
    coverage:geography.colleges.map(c=>({chamber:c.chamber,collegeId:c.id,listIds:ids})),correspondences:(['camera','senato'] as const).flatMap(ch=>ids.map(id=>({chamber:ch,pastListId:id,futureListId:id,factor:1}))),
    shares:[],abstention:[],projectionPolicy:'preserve-historical-turnout-v1',chamberProfileId:'central-v1'};
  const audit=refreshM01Polls(scenario,snapshot,d.polls.map(p=>({id:p.id,date:p.date,available:p.available,end:p.end,context:'camera',
    sample:p.sample,source:'Pinned historical poll '+p.id,enabled:true,groups:p.groups.map((members,j)=>({label:members.join('|'),members,share:p.shares[j]}))})),d.cutoff,lists,{},id=>id);
  const prepared=preparePairedVoteReferences(scenario,snapshot),pos=new Map(rows.map((r,i)=>[r.code,i]));
  return {audit,generate:(draw:number):Draw=>{
    const output=generatePairedMunicipalVotes(scenario,snapshot,prepared.units,draw),camera:Float64Array[]=Array(rows.length),senato:Float64Array[]=[];
    for(const r of output) {const a=Float64Array.from([r.abstention,...ids.map(id=>r.votes.find(v=>v.listId===id)?.votes??0)]);(r.chamber==='camera'?camera:senato)[pos.get(r.municipalityCode)!]=a;}
    return {camera,senato:scenario.renewal==='joint'?senato:undefined};
  }};
}

function central(d:Pick<Design,'history'|'polls'|'kind'>) {
  const b=d.history.at(-1)!,t=totals(b.rows),historical=normalize(t.votes),latest=d.polls.toSorted((a,b)=>a.available.localeCompare(b.available)||a.end.localeCompare(b.end)||a.id.localeCompare(b.id)).at(-1);
  const q=Float64Array.from(historical);
  if(latest) for(const [j,g] of latest.groups.entries()) {const mass=sum(g.map(id=>q[ids.indexOf(id)]));for(const id of g)q[ids.indexOf(id)]=latest.shares[j]*(mass?q[ids.indexOf(id)]/mass:1/g.length);}
  const same=d.history.filter(e=>e.kind===d.kind).at(-1)??b,st=totals(same.rows);
  return {q:normalize(q),valid:sum(st.votes)/st.electors};
}
const state=(votes:number[],electors:number)=>[...ilr(normalize(votes)),logit(sum(votes)/electors)];
const nationalState=(e:Event)=>{const t=totals(e.rows);return state(t.votes,t.electors);};
interface Block {years:number;region:Map<string,number[]>;local:Map<string,number[]>}
export function endpointFit(data:StudyData,d:Design) {
  const dim=ids.length,N=matrix(dim),R=matrix(dim),L=matrix(dim),blocks:Block[]=[],training:string[]=[];
  for(let j=0;j<dim;j++){N[j][j]=j===dim-1?.2**2:.2**2+.35**2*d.horizon/365.25;R[j][j]=.12**2;L[j][j]=.18**2;}
  for(const e of d.history) {
    const input=eligibleOrNull(data,e,d.horizon,d.regime);if(!input)continue;
    const c=central({...input,kind:e.kind as 'camera'|'european'}),before=input.history.at(-1)!,error=nationalState(e).map((v,j)=>v-[...ilr(c.q),logit(c.valid)][j]);
    outerAdd(N,error,1);training.push(e.id);
    const previous=new Map(before.rows.map(r=>[r.id,r])),shift=nationalState(e).map((v,j)=>v-nationalState(before)[j]),local=new Map<string,number[]>(),regions=new Map<string,{x:number[];weight:number}>(),regionFor=new Map<string,string>();
    for(const r of e.rows) {const p=previous.get(r.id);if(!p)continue;const a=state(p.votes,p.electors),z=state(r.votes,r.electors).map((v,j)=>v-a[j]-shift[j]),weight=sum(r.votes);
      local.set(r.id,z);regionFor.set(r.id,r.region);const bucket=regions.get(r.region)??{x:Array(dim).fill(0),weight:0};bucket.weight+=weight;z.forEach((v,j)=>bucket.x[j]+=v*weight);regions.set(r.region,bucket);}
    const region=new Map([...regions].map(([id,r])=>[id,r.x.map(v=>v/r.weight)])),duration=Math.max(.25,years(e.date,before.date));
    for(const [id,z] of local)local.set(id,z.map((v,j)=>v-region.get(regionFor.get(id)!)![j]));
    for(const z of region.values())outerAdd(R,z,1/region.size/duration);
    for(const z of local.values())outerAdd(L,z,1/local.size/duration);
    blocks.push({years:duration,region,local});
  }
  const denom=training.length+1;
  return {national:factor(N,denom),region:factor(R,denom),local:factor(L,denom),df:training.length+3,blocks,training};
}
function eligibleOrNull(data:StudyData,e:Event,horizon:number,regime:string) {try{return eligible(data,e.date,horizon,regime);}catch{return null;}}
export function endpointGenerator(data:StudyData,d:Design,seed:string,model:Model,contrast:SenateAdapter|null=null) {
  const fit=endpointFit(data,d),centre=central(d),mean=[...ilr(centre.q),logit(centre.valid)],age=Math.max(.25,years(d.date,d.baseline.date)),baseNational=nationalState(d.baseline);
  const histories=d.history.map(e=>({e,rows:new Map(e.rows.map(r=>[r.id,r])),national:nationalState(e)}));
  const profiles=d.rows.map(r=>{
    if(model!=='M07')return state(r.votes,r.electors);
    const parts=histories.flatMap(({e,rows,national})=>{const p=rows.get(r.id);return p?[{w:2**(-years(d.baseline.date,e.date)/8),x:state(p.votes,p.electors).map((v,j)=>v-national[j]+baseNational[j])}]:[];});
    return mean.map((_,j)=>sum(parts.map(p=>p.w*p.x[j]))/sum(parts.map(p=>p.w)));
  });
  const regions=[...new Set(d.rows.map(r=>r.region))],contrastPos=new Map(contrast?.profile.municipalityIds.map((id,i)=>[id,i])??[]);
  return {fit,generate:(draw:number):Draw=>{
    const rng=forecastRandom(seed,draw,'endpoint'),normal=()=>rng.normal(),gaussian=(l:number[][])=>{const z=l.map(normal);return l.map(row=>sum(row.map((v,j)=>v*z[j])));},tail=()=>Math.sqrt((fit.df-2)/sum(Array.from({length:fit.df},()=>normal()**2)));
    const block=fit.blocks.length?fit.blocks[rng.integer(fit.blocks.length)]:null,sign=rng.uniform()<.5?-1:1;
    const innovation=(l:number[][],historic?:number[])=>{let z=gaussian(l).map(v=>v*Math.sqrt(age));if(model==='M06'){const t=tail();z=z.map(v=>v*t);}if(model==='M07'&&block)z=z.map((v,j)=>.35*v+(historic?.[j]??0)*sign*Math.sqrt(age/block.years));return z;};
    let national=gaussian(fit.national);if(model==='M06'){const t=tail();national=national.map(v=>v*t);}
    const q=inverseIlr(mean.slice(0,-1).map((v,j)=>v+national[j])),validTarget=logistic(mean.at(-1)!+national.at(-1)!);
    const regional=new Map(regions.map(r=>[r,innovation(fit.region,block?.region.get(r))]));
    const shares:Float64Array[]=[],logits:number[]=[];
    d.rows.forEach((r,i)=>{const local=innovation(fit.local,block?.local.get(r.id)),regionalZ=regional.get(r.region)!,x=profiles[i].map((v,j)=>v+local[j]+regionalZ[j]);shares.push(inverseIlr(x.slice(0,-1)));logits.push(x.at(-1)!);});
    let low=-64,high=64;const electors=sum(d.rows.map(r=>r.electors));
    for(let it=0;it<48;it++){const mid=(low+high)/2,rate=sum(d.rows.map((r,i)=>r.electors*logistic(logits[i]+mid)))/electors;if(rate<validTarget)low=mid;else high=mid;}
    const valid=logits.map(v=>logistic(v+(low+high)/2));rake(shares,d.rows.map((r,i)=>r.electors*valid[i]),q);
    const field=shares.map((p,i)=>Float64Array.from([1-valid[i],...p.map(v=>valid[i]*v)]));
    const senate=contrast?field.map((p,i)=>{
      const at=contrastPos.get(d.rows[i].id)!*(ids.length+1),gap=Float64Array.from([contrast.nonValidLogOdds,...contrast.nationalLogRatios]);
      // Remove Camera profile to retain only the deterministic Senate contrast.
      // profile delta is supplied separately by the caller below.
      const extra=(contrast as SenateAdapter & {researchCameraMeans?:Float64Array}).researchCameraMeans;
      if(extra)gap.forEach((v,j)=>gap[j]=v+contrast.profile.means[at+j]-extra[at+j]);
      return pushContrast(p,gap);
    }):undefined;
    return {camera:countField(field,d,seed,draw,'camera'),senato:senate?countField(senate,d,seed,draw,'senato'):undefined};
  }};
}
