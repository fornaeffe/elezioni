/** Diagnose actual released M02 assets, independently of the reduced-family fit. */
import {readFile,writeFile,mkdir} from 'node:fs/promises';
import {resolve,dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {loadCandidate} from '../../../../web/src/lib/politics/reform-2026/vote-share/release';
import {readBytes,implementationKey} from '../../../../web/scripts/data-preparation/fast-share-release-io';
import {forecastRandom} from '../../../../web/src/lib/politics/reform-2026/vote-share/forecast-random';
import {prepareM02Votes} from '../../../../web/src/lib/politics/generation/m02';
import {sum} from './data';
const root=resolve(dirname(fileURLToPath(import.meta.url)),'../../../..'),dir=resolve(root,'web/static/data/reform-2026/fast-share'),read=readBytes(dir);
const loaded=await loadCandidate(JSON.parse(await readFile(resolve(dir,'candidate.json'),'utf8')),read,{validateGeneration:false});
const fit=loaded.assets.fit,k=fit.componentIds.length,samples=fit.political.values,count=fit.political.chains*fit.political.drawsPerChain;
const correlation=(a:number[],b:number[])=>{const ma=sum(a)/a.length,mb=sum(b)/b.length,ab=sum(a.map((v,i)=>(v-ma)*(b[i]-mb))),aa=sum(a.map(v=>(v-ma)**2)),bb=sum(b.map(v=>(v-mb)**2));return ab/Math.sqrt(aa*bb);};
const candidates=['fdi','fi-pdl','lega','m5s','pd'].filter(id=>fit.componentIds.includes(id));
const order=Array.from({length:count},(_,i)=>i).sort((a,b)=>samples[a*2*k+fit.componentIds.indexOf('fdi')]-samples[b*2*k+fit.componentIds.indexOf('fdi')]);
const rows=[];
for(const rho of [.5,.85,.98,1]) {
  const camera:number[]=[],senate:number[]=[];
  for(let draw=1;draw<=8192;draw++) {camera.push(forecastRandom('posterior-row-diagnosis-v1',draw,'posterior-political-row','camera',rho).integer(count));senate.push(forecastRandom('posterior-row-diagnosis-v1',draw,'posterior-political-row','senato',rho).integer(count));}
  rows.push({requestedLatentRho:rho,indexCorrelation:correlation(camera,senate),sameRowRate:sum(camera.map((r,i)=>Number(r===senate[i])))/camera.length,
    coordinates:candidates.map(id=>{const j=fit.componentIds.indexOf(id);return {id,storedOrderCorrelation:correlation(camera.map(r=>samples[r*2*k+j]),senate.map(r=>samples[r*2*k+j])),
      sortedByFdiOrderCorrelation:correlation(camera.map(r=>samples[order[r]*2*k+j]),senate.map(r=>samples[order[r]*2*k+j]))};})});
}
const forecasts=[];
for(const {chamberProfileId,zeroHorizon} of [{chamberProfileId:'central-v1',zeroHorizon:false},{chamberProfileId:'stronger-dependence-v1',zeroHorizon:false},{chamberProfileId:'central-v1',zeroHorizon:true}]) {
  const prepared=prepareM02Votes({...loaded.scenario,chamberProfileId,electionDate:zeroHorizon?loaded.scenario.referenceDate:loaded.scenario.electionDate},loaded.assets,{geography:loaded.context.seats.geography,models:loaded.context.models},{copyAssets:false});
  const delta=(values:Partial<Record<'camera'|'senato',number[]>>)=>{const c=values.camera!,s=values.senato!;return c.slice(1).map((v,j)=>s[j+1]/(1-s[0])-v/(1-c[0]));};
  const reference:number[][]=[],future:number[][]=[];const start=performance.now();
  for(let draw=1;draw<=32;draw++) {const {audit}=prepared.run(draw);reference.push(delta(audit.countryReference));future.push(delta(audit.countryForecast));if(draw%8===0)console.log(JSON.stringify({diagnosticProfile:chamberProfileId,draw}));}
  forecasts.push({chamberProfileId,zeroHorizon,draws:32,generationMs:performance.now()-start,
    referenceRms:sum(reference.map(r=>Math.sqrt(sum(r.map(v=>v*v))/r.length)))/reference.length,
    forecastRms:sum(future.map(r=>Math.sqrt(sum(r.map(v=>v*v))/r.length)))/future.length,
    referenceMeanMax:sum(reference.map(r=>Math.max(...r.map(Math.abs))))/reference.length,
    forecastMeanMax:sum(future.map(r=>Math.max(...r.map(Math.abs))))/future.length});
}
const report={schemaVersion:1,kind:'actual-release-chamber-diagnosis',codeIdentity:await implementationKey(root,fileURLToPath(import.meta.url)),catalogId:loaded.catalog.sourceId,
  sourceId:loaded.assets.sourceBundleId,fitStateId:loaded.assets.fittedStateId,referenceDate:loaded.scenario.referenceDate,electionDate:loaded.scenario.electionDate,
  posteriorRows:count,components:fit.componentIds,referenceCounts:{common:loaded.scenario.references.filter(r=>r.channel==='common').length,chamberSpecific:loaded.scenario.references.filter(r=>r.channel!=='common').length},posteriorCoupling:rows,actualDefaultForecasts:forecasts,
  interpretation:'Reordering whole posterior rows preserves their empirical joint marginal distribution. A changed inter-chamber correlation after reordering reveals dependence on arbitrary row order, not posterior geometry. Forecast diagnostics are descriptive, not historical accuracy scores.'};
const out=resolve(root,'data/derived/vote-generation-campaign-2026-11');await mkdir(out,{recursive:true});await writeFile(resolve(out,'diagnosis.json'),JSON.stringify(report,null,2));
console.log(JSON.stringify(report));
