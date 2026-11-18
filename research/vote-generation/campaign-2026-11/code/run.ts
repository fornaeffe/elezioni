/** Run from web/: node --import tsx ../research/vote-generation/campaign-2026-11/code/run.ts ... */
import {readFile,writeFile,mkdir} from 'node:fs/promises';
import {resolve,dirname} from 'node:path';
import {createHash} from 'node:crypto';
import {fileURLToPath} from 'node:url';
import {loadData,categories,sum,mappings,totals,type Row,type Mapping} from './data';
import {makeDesign,modelIds,prepareM02,m02Generator,m01Generator,endpointGenerator,endpointContrast,type Model,type Draw,type Design} from './models';
import {score,average} from './math';
import {implementationKey} from '../../../../web/scripts/data-preparation/fast-share-release-io';
const root=resolve(dirname(fileURLToPath(import.meta.url)),'../../../..');
const options=Object.fromEntries(process.argv.slice(2).map(s=>{const at=s.indexOf('=');return at<0?[s.replace(/^--/,''),'true']:[s.slice(0,at).replace(/^--/,''),s.slice(at+1)];}));
const model=(options.model??'M02') as Model,draws=Number(options.draws??256),horizon=Number(options.horizon??15),limit=Number(options.limit??0),regime=options.regime??'mixed',targetId=options.target??'camera-2022',mode=(options.mode??'none') as Design['pairMode'];
if(![...modelIds,'M02-row-sync','M02-rho1'].includes(model)||!Number.isSafeInteger(draws)||draws<2||!Number.isSafeInteger(horizon)||horizon<0||!['mixed','camera-only'].includes(regime)||!['none','2018-age-audit','2022-reconstruction'].includes(mode))throw new Error('INVALID_CAMPAIGN_ARGUMENTS');
const started=performance.now(),data=await loadData(root),loadedMs=performance.now()-started,target=data.events.find(e=>e.id===targetId)!;
if(!target)throw new Error('UNKNOWN_TARGET');
const design=makeDesign(data,target,horizon,regime,limit,mode);
const seed='chamber-campaign-v1:'+targetId+':'+horizon+':'+regime+':'+mode;
const output=resolve(root,options.output??'data/derived/vote-generation-campaign-2026-11');await mkdir(output,{recursive:true});
const prep=performance.now();let generate:(draw:number)=>Draw;let diagnostics:unknown;
if(model==='M01') {const p=m01Generator(data,design,seed);generate=p.generate;diagnostics={polls:p.audit};}
else if(model.startsWith('M02')) {const p=prepareM02(data,design,seed);generate=m02Generator(data,design,p,seed,model);diagnostics={political:p.fit.political.diagnostics,participation:p.fit.participation.diagnostics,scales:p.fit.audit,adapter:p.adapter?.assumptions};}
else {
  const contrast=mode==='none'?null:endpointContrast(data,design);
  const p=endpointGenerator(data,design,seed,model,contrast);generate=p.generate;diagnostics={training:p.fit.training,df:p.fit.df,blocks:p.fit.blocks.length};
}
const preparationMs=performance.now()-prep;
const map=mappings(design.rows,data.geography),truth=new Map(target.rows.map(r=>[r.id,r])),senateEvent=data.events.find(e=>e.id==='senato-2022')!,senateTruth=new Map(senateEvent.rows.map(r=>[r.id,r]));
interface Area {id:string; truth:number[]; senateTruth?:number[]; camera:number[][]; senate:number[][]; difference:number[][]}
const aggregateTruth=(mapping:Mapping,source:Map<string,Row>)=>{
  const votes=categories.map((_,j)=>sum(mapping.weights.map(w=>source.get(design.rows[w.row].id)!.votes[j]*w.weight)));return votes.map(v=>v/sum(votes));
};
const areas=Object.fromEntries(Object.entries(map).map(([level,entries])=>[level,entries.map(m=>({id:m.id,truth:aggregateTruth(m,truth),senateTruth:mode==='none'?undefined:aggregateTruth(m,senateTruth),camera:categories.map(()=>[]),senate:categories.map(()=>[]),difference:categories.map(()=>[])}))])) as Record<string,Area[]>;
const aggregateDraw=(mapping:Mapping,field:Float64Array[])=>{const votes=categories.map((_,j)=>sum(mapping.weights.map(w=>field[w.row][j+1]*w.weight)));return votes.map(v=>v/sum(votes));};
let invalidRows=0;const generationStart=performance.now();let generationMs=0,aggregationMs=0;
for(let draw=1;draw<=draws;draw++) {
  const before=performance.now(),sample=generate(draw);generationMs+=performance.now()-before;
  for(const field of [sample.camera,...(sample.senato?[sample.senato]:[])])field.forEach((r,i)=>{if(r.some(x=>!Number.isFinite(x)||x<0||!Number.isInteger(x))||sum(r)!==design.rows[i].electors)invalidRows++;});
  const agg=performance.now();
  for(const [level,entries] of Object.entries(map))entries.forEach((m,i)=>{
    const a=areas[level][i],c=aggregateDraw(m,sample.camera),s=sample.senato?aggregateDraw(m,sample.senato):undefined;
    c.forEach((v,j)=>{a.camera[j].push(v);if(s){a.senate[j].push(s[j]);a.difference[j].push(s[j]-v);}});
  });aggregationMs+=performance.now()-agg;
  if(draw%64===0)console.log(JSON.stringify({target:targetId,horizon,regime,model,mode,draw,draws,seconds:(performance.now()-generationStart)/1000}));
}
if(invalidRows)throw new Error('INVALID_GENERATED_ROWS:'+invalidRows);
const summarize=(entries:Area[],ch:'camera'|'senate')=>average(entries.flatMap(a=>categories.map((_,j)=>score(a[ch][j],(ch==='camera'?a.truth:a.senateTruth!)[j]))));
function difference(entries:Area[]) {
  const scores=entries.flatMap(a=>categories.map((_,j)=>score(a.difference[j],a.senateTruth![j]-a.truth[j])));
  const actualRms=Math.sqrt(sum(scores.map(r=>r.truth**2))/scores.length),perDraw=Array.from({length:draws},(_,i)=>Math.sqrt(sum(entries.flatMap(a=>a.difference.map(v=>v[i]**2)))/scores.length));
  const actualLargest=Math.max(...scores.map(r=>Math.abs(r.truth))),maxima=Array.from({length:draws},(_,i)=>Math.max(...entries.flatMap(a=>a.difference.map(v=>Math.abs(v[i])))));
  const result={...average(scores),observedRms:actualRms,predictiveRms:sum(perDraw)/draws,rmsRatio:actualRms?sum(perDraw)/draws/actualRms:null,
    rmsPIT:sum(perDraw.map(v=>Number(v<=actualRms)))/draws,observedLargestGap:actualLargest,meanLargestGap:sum(maxima)/draws};
  return result;
}
const summary=Object.fromEntries(Object.entries(areas).map(([level,entries])=>[level,{units:entries.length,camera:summarize(entries,'camera'),
  ...(mode!=='none'?{senato:summarize(entries,'senate'),difference:difference(entries)}:{})}]));
const inputHash=createHash('sha256').update(JSON.stringify({sourceId:data.source.id,design})).digest('hex');
const codeIdentity=await implementationKey(root,fileURLToPath(import.meta.url));
const report={schemaVersion:1,campaign:'chamber-campaign-v1',codeIdentity,model,modelVersion:model==='M01'||model==='M02'?'2.0.0':'research-v1',target:targetId,horizon,regime,mode,draws,seed,inputHash,
  cutoff:design.cutoff,baseline:design.baseline.id,history:design.history.map(e=>e.id),polls:design.polls.map(p=>p.id),municipalities:design.rows.length,
  domain:{forecastElectors:sum(design.rows.map(r=>r.electors)),targetValid:sum(design.rows.map(r=>sum(truth.get(r.id)!.votes))),allTargetValid:sum(target.rows.map(r=>sum(r.votes))),excludedRegions:['02','04'],geography:'current-source-census-reconstruction'},
  scope:mode==='2022-reconstruction'?'IN-SAMPLE 2022 Senate contrast reconstruction; not a held-out joint forecast':mode==='2018-age-audit'?'Time-held-out 2022 with 2018 25+ Senate contrast; demographic-confounded research waiver':'Time-held-out single-chamber family forecasts; research preparation replaces unavailable historical common-18+ pair',
  audit:data.audit,diagnostics,summary,resources:{node:process.version,platform:process.platform,loadedMs,preparationMs,generationMs,aggregationMs,elapsedMs:performance.now()-started,
    generationMsPerDraw:generationMs/draws,fitPlus1000GenerationSeconds:(preparationMs+generationMs/draws*1000)/1000,maxRssMiB:process.resourceUsage().maxRSS/1024,memory:process.memoryUsage(),
    note:'Isolated Node process high-water RSS includes source loading, fit, generation and score buffers. No browser, scrutiny, worker replication, or exact simultaneous-phase peak claim.'},failures:invalidRows};
const name=[targetId,horizon,regime,mode,model,draws,limit||'full'].join('-');
await writeFile(resolve(output,name+'.json'),JSON.stringify(report,null,2)+'\n');
await writeFile(resolve(output,name+'-details.json'),JSON.stringify(areas));
console.log(JSON.stringify({completed:name,summary,resources:report.resources}));
