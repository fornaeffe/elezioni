import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { loadGenerationHistory } from './generation-history';
import { loadMembershipPolicy, membershipFor } from './party-drift-memberships';
import { prepareSelectedPolls } from './polls';
import { makeAdmissiblePanel, coarsenGroups } from './party-drift-panel';
import { POLL_POLICY } from '../../src/lib/polls/forecast';
import { sha256 } from './sources';

export interface PanelPoll { id:string; surveyId:string; date:string; available:string; end:string; kind:'camera'|'european';
  groups:string[][]; shares:number[]; other:number|null; sample:number; source:string; group:string; }
export function reviewedEarlyShares(original:{id:string;shares:number[]},refined?:{id:string;shares:number[]}){
  if(!refined)return{groups:[['pd'],['m5s'],['lega']],shares:original.shares.slice(0,3),other:original.shares[3]};
  if(refined.id!==original.id||refined.shares.length!==6||refined.shares.some(x=>!Number.isFinite(x)||x<0)||
    Math.abs(refined.shares.reduce((s,x)=>s+x,0)-1)>1e-8||original.shares.slice(0,3).some((x,i)=>Math.abs(x-refined.shares[i])>1e-8)||
    Math.abs(original.shares[3]-refined.shares.slice(3).reduce((s,x)=>s+x,0))>1e-8)throw new Error('Inconsistent reviewed early poll view: '+original.id);
  return{groups:[['pd'],['m5s'],['lega'],['fdi'],['fi-pdl']],shares:refined.shares.slice(0,5),other:refined.shares[5]};
}
export async function loadPartyResearchData(root:string) {
  const history=await loadGenerationHistory(root), inputs=await loadMembershipPolicy(root);
  const refinementBytes=await readFile(resolve(root,'data/sources/party-drift/poll-refinements.json'));
  const refinements=JSON.parse(refinementBytes.toString('utf8')) as {questions:{id:string;sourceFile:string;sourceSha256:string;answers:{percentage:number;members?:string[];sourceLabel?:string;other?:boolean}[]}[]};
  for(const q of refinements.questions){
    if(Math.abs(q.answers.reduce((s,a)=>s+a.percentage,0)-100)>1e-8)throw new Error('Refined poll percentages do not conserve: '+q.id);
    let pdf:Buffer|undefined;try{pdf=await readFile(resolve(root,q.sourceFile));}catch(error){if((error as NodeJS.ErrnoException).code!=='ENOENT')throw error;}
    if(pdf&&sha256(pdf)!==q.sourceSha256)throw new Error('Refined poll source PDF changed: '+q.id);
  }
  const versions=[...history.sourceVersions,...inputs.versions,'poll-refinements:'+sha256(refinementBytes)],polls:PanelPoll[]=[],excluded:{id:string;reason:string}[]=[];
  const entity=(id:string,date:string):string[]|undefined=>{
    const named:Record<string,string[]>={pd:date>='2019-01-01'?['pd','art1']:['pd'],fdi:['fdi'],fi:['fi-pdl'],pdl:['fi-pdl'],lega:['lega'],m5s:['m5s'],sel:['sel-si'],udc:['udc'],fare:['fare'],'scelta-civica':['scelta-civica'],'la-destra':['la-destra'],
      'piu-europa':date>='2017-01-01'&&date<'2019-01-01'?['piu-europa','centro-democratico']:['piu-europa'],
      'azione-italia-viva':['azione','italia-viva'],azione:['azione'],'italia-viva':['italia-viva'],'noi-moderati':['noi-residual','udc','scelta-civica'],'noi-italia':['noi-residual','udc','scelta-civica'],
      avs:date>='2022-01-01'&&date<'2023-01-01'?['verdi','sel-si','possible']:['verdi','sel-si'],leu:['sel-si','possible','art1'],insieme:['psi-civici','verdi'],pap:['prc','pci-pdci','pap-residual'],
      'rivoluzione-civile':['prc','pci-pdci','verdi','idv'],'impegno-civico':['impegno-residual','centro-democratico'],'unione-popolare':['prc','pap-residual'],svp:['svp-patt'],italexit:['italexit'],
      fli:membershipFor(inputs.policy,'camera-2013',"FUTURO E LIBERTA'").members,'civica-popolare':membershipFor(inputs.policy,'camera-2018','CIVICA POPOLARE LORENZIN').members};return named[id];
  };
  for(const directory of ['data/sources/polls/history','data/sources/polls']){
    const archive=await prepareSelectedPolls(root,resolve(root,directory));versions.push(...archive.bundle.sourceVersions);
    for(const q of archive.bundle.questions){
      if(q.source.publicationDate>'2024-06-08')continue;
      if(q.ballot.kind==='conditional'){excluded.push({id:q.id,reason:'Conditional ballot without baseline assumptions'});continue;}
      if(q.normalization.method==='missing-support-residual'){excluded.push({id:q.id,reason:'Earlier view invents missing political support; original offered choices require review'});continue;}
      const refined=refinements.questions.find(r=>r.id===q.id),groups:string[][]=[],shares:number[]=[];let other:number|null=null,unresolved=false;
      if(refined)for(const a of refined.answers){if(a.other){other=(other??0)+a.percentage/100;continue;}const members=a.members??(a.sourceLabel?membershipFor(inputs.policy,'european-2024',a.sourceLabel).members:undefined);if(!members)unresolved=true;else{groups.push(members);shares.push(a.percentage/100);}}
      else for(const a of q.support){if(!a.entityIds.length){other=(other??0)+a.share;continue;}if(a.entityIds.some(id=>!entity(id,q.source.fieldworkEnd)))unresolved=true;else{groups.push([...new Set(a.entityIds.flatMap(id=>entity(id,q.source.fieldworkEnd)!))]);shares.push(a.share);}}
      if(unresolved){excluded.push({id:q.id,reason:'Unmapped explicit political identity'});continue;}
      const kind=/european|europe[ae]|europee/i.test(q.text+' '+q.source.title)?'european':'camera';
      const nonresponse=q.answers.find(a=>a.kind==='nonresponse')?.percentage,decided=nonresponse===undefined?POLL_POLICY.unknownDecidedFraction:1-nonresponse/100;
      polls.push({id:q.id,surveyId:q.independenceGroup,date:new Date((Date.parse(q.source.fieldworkStart)+Date.parse(q.source.fieldworkEnd))/2).toISOString(),available:q.source.publicationDate,end:q.source.fieldworkEnd,kind,
        groups,shares,other,sample:Math.min(POLL_POLICY.maximumEffectiveSample,(q.questionSampleSize??q.source.sampleSize*decided)/POLL_POLICY.designEffect),source:q.source.url,
        group:q.source.fieldworkEnd.slice(0,4)+'-H'+(Number(q.source.fieldworkEnd.slice(5,7))<=6?1:2)+'-'+kind});
    }
  }
  const earlyBytes=await readFile(resolve(root,'data/sources/polls/history/generation-horizon-observations.json'));versions.push('early:'+sha256(earlyBytes));
  const sixBytes=await readFile(resolve(root,'data/sources/polls/history/generation-six-category-observations.json'));
  const six=JSON.parse(sixBytes.toString('utf8')) as {panel:string[];earlyObservations:{id:string;shares:number[];publicationDate:string;fieldworkStart:string;fieldworkEnd:string}[];inputPins:{path:string;sha256:string}[]};
  if(six.panel.join(',')!=='pd,m5s,lega,fdi,fi-pdl,other')throw new Error('Unknown reviewed early poll panel');
  for(const pin of six.inputPins){if(!['archive.json','manifest.json','elections-manifest.json','generation-horizon-observations.json','generation-horizon-manifest.json'].includes(pin.path)||
    sha256(await readFile(resolve(root,'data/sources/polls/history',pin.path)))!==pin.sha256)throw new Error('Reviewed early poll input pin changed');}
  versions.push('reviewed-early-six:'+sha256(sixBytes));
  for(const p of JSON.parse(earlyBytes.toString('utf8')).observations){
    const refined=six.earlyObservations.find(x=>x.id===p.id);
    if(refined&&['publicationDate','fieldworkStart','fieldworkEnd'].some(key=>refined[key as keyof typeof refined]!==p[key]))throw new Error('Reviewed early poll dates changed: '+p.id);
    polls.push({id:p.id,surveyId:p.id,date:new Date((Date.parse(p.fieldworkStart)+Date.parse(p.fieldworkEnd))/2).toISOString(),available:p.publicationDate,end:p.fieldworkEnd,kind:'camera',
      ...reviewedEarlyShares(p,refined),sample:Math.min(POLL_POLICY.maximumEffectiveSample,p.sampleSize*POLL_POLICY.unknownDecidedFraction/POLL_POLICY.designEffect),source:p.sourceUrl,group:p.fieldworkEnd.slice(0,4)+'-H'+(Number(p.fieldworkEnd.slice(5,7))<=6?1:2)+'-camera'});
  }
  if(new Set(polls.map(p=>p.surveyId)).size!==polls.length)throw new Error('Duplicate survey evidence in panel input');
  return{history,policy:inputs.policy,versions,polls,excluded};
}
export function prepareAdmissibleCase(input:Awaited<ReturnType<typeof loadPartyResearchData>>,targetId:string,horizon:number,maximumCities=Infinity,componentScope:'main'|'all'='main'){
  const target=input.history.events.find(e=>e.id===targetId);if(!target)throw new Error('Unknown target');
  const targetDate=targetId==='european-2024'?'2024-06-08':target.date,cutoff=new Date(Date.parse(targetDate)-horizon*86400000).toISOString().slice(0,10);
  const elections=input.history.events.filter(e=>e.date<cutoff),polls=input.polls.filter(p=>p.available<=cutoff&&p.end<=cutoff&&Date.parse(p.date)<=Date.parse(cutoff));
  const usable=(c:typeof target.municipalities[number])=>c.regionId!=='02'&&c.matchable&&c.electors>0&&c.validVotes<=c.electors;
  const sourceGroups=(event:typeof target)=>[...new Set(event.municipalities.filter(usable).flatMap(c=>c.lists.map(l=>l.name)))].map(label=>membershipFor(input.policy,event.id,label).members);
  const information=[...elections.map(e=>({id:e.id,groups:sourceGroups(e)})),...polls.map(p=>({id:p.id,groups:p.groups}))];
  const basis=[...new Set([...information.flatMap(r=>r.groups.flat()),...sourceGroups(target).flat()])].sort(),explicit=new Set(information.flatMap(r=>r.groups.flat()));
  const panel=makeAdmissiblePanel(basis,information,explicit,componentScope==='all'?basis.filter(id=>explicit.has(id)):undefined,sourceGroups(target));
  if(!panel.audit?.resolved)throw new Error('Research panel failed structural resolution');
  const mapping=(id:string)=>panel.mapping[id]??'research-other';
  const latest=elections.at(-1)!;if(!latest)throw new Error('No training election');
  // Future geography/electorates are conditional scenario inputs. New or renamed
  // municipalities get missing historical cells, never invented zero counts.
  const allCities=[...new Set([...elections,target].flatMap(e=>e.municipalities.filter(usable).map(c=>c.id)))].sort();
  const count=Math.min(maximumCities,allCities.length),cities=Array.from({length:count},(_,i)=>allCities[Math.floor((i+.5)*allCities.length/count)]),citySet=new Set(cities);
  const project=(e:typeof target)=>e.municipalities.filter(c=>citySet.has(c.id)&&c.regionId!=='02'&&c.matchable&&c.electors>0&&c.validVotes<=c.electors).map(c=>{
    const projected=coarsenGroups(c.lists.map(l=>membershipFor(input.policy,e.id,l.name).members),c.lists.map(l=>l.votes),mapping);
    return{id:c.id,region:c.regionId,electors:c.electors,nonValid:c.electors-c.validVotes,groups:projected.map(g=>g.members),counts:projected.map(g=>g.value),sourceLabels:projected.map(g=>g.originalIndices.map(i=>c.lists[i].name))};
  });
  const projectedElections=elections.map(e=>({id:e.id,date:e.date,kind:e.kind,rows:project(e)}));
  const supportStart=new Map<string,number>();
  for(const e of elections)for(const id of sourceGroups(e).flat())supportStart.set(id,Math.min(supportStart.get(id)??Infinity,Date.parse(e.date)));
  for(const p of polls)for(const id of p.groups.flat())supportStart.set(id,Math.min(supportStart.get(id)??Infinity,Date.parse(p.date)));
  const projectedPolls=polls.map(p=>{
    const groups=p.groups.map(g=>[...g]),shares=[...p.shares];
    // Construct Other on the original basis BEFORE pooling. A named minor list
    // and unnamed minor support may both map to the same research residual.
    if(p.other!==null){const named=groups.flat(),complement=basis.filter(id=>(supportStart.get(id)??Infinity)<=Date.parse(p.date)&&!named.includes(id));if(!complement.length&&p.other>0)throw new Error('Positive Other has empty original complement: '+p.id);if(complement.length){groups.push(complement);shares.push(p.other);}}
    const projected=coarsenGroups(groups,shares,mapping),total=projected.reduce((s,g)=>s+g.value,0);
    return{...p,groups:projected.map(g=>g.members),shares:projected.map(g=>g.value/total)};
  }).filter(p=>p.groups.length>1);
  if(projectedPolls.some(p=>p.shares.some(s=>s<=0)))throw new Error('Printed-zero poll needs a censored likelihood before fitting');
  return{targetId,targetDate,targetKind:target.kind,horizon,cutoff,panel,cities,totalCities:allCities.length,elections:projectedElections,polls:projectedPolls,truth:project(target),
    conditionalBallot:true,scope:'Cutoff-specific admissible research panel; original lists/counts retained; no individual calibration claimed inside pooled outcomes'};
}
