/** Isolated timing first, then accuracy jobs in bounded process parallelism. */
import {spawn} from 'node:child_process';
import {readFile,writeFile,mkdir} from 'node:fs/promises';
import {createWriteStream} from 'node:fs';
import {dirname,resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {availableParallelism,freemem} from 'node:os';
import {implementationKey} from '../../../../web/scripts/data-preparation/fast-share-release-io';
const root=resolve(dirname(fileURLToPath(import.meta.url)),'../../../..'),web=resolve(root,'web'),out=resolve(root,'data/derived/vote-generation-campaign-2026-11');
await mkdir(out,{recursive:true});
type Job={model:string;target:string;horizon:number;regime:string;mode:string;draws:number};
const jobs:Job[]=[];
// Pair first: directly investigate the fault that motivated this campaign.
for(const model of ['M01','M02','M02-shared','M05','M06','M07'])jobs.push({model,target:'camera-2022',horizon:15,regime:'mixed',mode:'2018-age-audit',draws:512});
for(const target of ['camera-2018','camera-2022'])for(const horizon of [15,365])for(const model of ['M01','M02','M05','M06','M07'])
  jobs.push({model,target,horizon,regime:'mixed',mode:'none',draws:256});
for(const target of ['camera-2018','camera-2022'])for(const model of ['M01','M02','M05','M06','M07'])
  jobs.push({model,target,horizon:15,regime:'camera-only',mode:'none',draws:256});
for(const model of ['M02','M02-row-sync','M02-rho1','M02-shared'])jobs.push({model,target:'camera-2022',horizon:15,regime:'mixed',mode:'2022-reconstruction',draws:256});
const nameOf=(j:Job)=>[j.target,j.horizon,j.regime,j.mode,j.model,j.draws,'full'].join('-');
const resume=process.argv.includes('--resume'),codeIdentity=await implementationKey(root,resolve(dirname(fileURLToPath(import.meta.url)),'run.ts'));
const accuracyConcurrency=Math.max(1,Math.min(8,Math.floor(availableParallelism()/2),Math.floor(freemem()/2**30)));
const ledger:{job:Job;status:string;milliseconds:number;file:string;measurementContext:string;resumed?:boolean}[]=[],start=performance.now();
let previous:any={jobs:[]};
if(resume)try{previous=JSON.parse(await readFile(resolve(out,'execution.json'),'utf8'));}catch{}
let writeQueue=Promise.resolve();
function save() {
  const ordered=[...ledger].sort((a,b)=>jobs.findIndex(j=>nameOf(j)===nameOf(a.job))-jobs.findIndex(j=>nameOf(j)===nameOf(b.job)));
  const body=JSON.stringify({elapsedMs:performance.now()-start,priorElapsedMs:(previous.elapsedMs??0)+(previous.priorElapsedMs??0),accuracyConcurrency,planned:jobs.length,jobs:ordered},null,2);
  writeQueue=writeQueue.then(()=>writeFile(resolve(out,'execution.json'),body));return writeQueue;
}
async function run(job:Job,measurementContext:string) {
  const name=[job.target,job.horizon,job.regime,job.mode,job.model,job.draws,'full'].join('-'),file=resolve(out,name+'.json'),before=performance.now();
  if(resume)try {
    const r=JSON.parse(await readFile(file,'utf8'));
    if(r.codeIdentity!==codeIdentity||r.failures||r.draws!==job.draws||r.model!==job.model||r.mode!==job.mode)throw new Error('INCOMPATIBLE_RESUME');
    const old=previous.jobs.find((r:any)=>r.file===name+'.json');
    if(!old||old.status!=='passed')throw new Error('NO_PASSED_LEDGER');
    ledger.push({job,status:'passed',milliseconds:old.milliseconds,file:name+'.json',measurementContext:old.measurementContext??'sequential-isolated',resumed:true});await save();
    console.log(JSON.stringify({reused:name}));return;
  } catch(error) {if((error as NodeJS.ErrnoException).code!=='ENOENT')console.log(JSON.stringify({rerunning:name,reason:String(error)}));}
  console.log(JSON.stringify({starting:name,completed:ledger.length,total:jobs.length,measurementContext}));
  const log=createWriteStream(resolve(out,name+'.log'));
  try {
    const code=await new Promise<number|null>((accept,reject)=>{
      const child=spawn(process.execPath,['--import','tsx',resolve(dirname(fileURLToPath(import.meta.url)),'run.ts'),...Object.entries(job).map(([k,v])=>'--'+k+'='+v)],{cwd:web,windowsHide:true,stdio:['ignore','pipe','pipe']});
      child.stdout.pipe(log,{end:false});child.stderr.pipe(log,{end:false});child.on('error',reject);child.on('exit',accept);
    });
    log.end();if(code!==0)throw new Error('CASE_FAILED:'+name+'; exit='+code);
    const report=JSON.parse(await readFile(file,'utf8'));if(report.failures||report.draws!==job.draws||report.codeIdentity!==codeIdentity)throw new Error('INVALID_CASE_RECEIPT:'+name);
    ledger.push({job,status:'passed',milliseconds:performance.now()-before,file:name+'.json',measurementContext});
  } catch(error) {
    log.end();ledger.push({job,status:String(error),milliseconds:performance.now()-before,file:name+'.json',measurementContext});
    await save();throw error;
  }
  await save();
  console.log(JSON.stringify({completed:name,seconds:(performance.now()-before)/1000,totalCompleted:ledger.length,total:jobs.length}));
}
for(const job of jobs.slice(0,6))await run(job,'sequential-isolated');
const accuracy=jobs.slice(6);
for(let i=0;i<accuracy.length;i+=accuracyConcurrency) {
  const outcomes=await Promise.allSettled(accuracy.slice(i,i+accuracyConcurrency).map(job=>run(job,'parallel-accuracy-'+accuracyConcurrency+'-not-a-speed-benchmark')));
  const failures=outcomes.filter(r=>r.status==='rejected');if(failures.length)throw new AggregateError(failures,'ACCURACY_CASE_FAILURES');
}
for(const script of ['diagnose.ts','report.ts']) {
  console.log(JSON.stringify({starting:script}));
  const code=await new Promise<number|null>((accept,reject)=>{
    const child=spawn(process.execPath,['--import','tsx',resolve(dirname(fileURLToPath(import.meta.url)),script)],{cwd:web,windowsHide:true,stdio:['ignore','inherit','inherit']});
    child.on('error',reject);child.on('exit',accept);
  });
  if(code!==0)throw new Error('POSTPROCESS_FAILED:'+script);
}
