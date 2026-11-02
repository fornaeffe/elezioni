import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';
import { loadPartyResearchData, prepareAdmissibleCase } from './party-drift-panel-data';
import { sha256 } from './sources';

const root=fileURLToPath(new URL('../../../',import.meta.url));
const input=await loadPartyResearchData(root);
const sourceFiles=['party-drift-panel-cli.ts','party-drift-panel-data.ts','party-drift-panel.ts','party-drift-identifiability.ts','party-drift-memberships.ts'];
const sourceVersions=await Promise.all(sourceFiles.map(async path=>({path,sha256:sha256(await readFile(resolve(root,'web/scripts/data-preparation',path)))})));
const version=sha256(JSON.stringify({inputs:input.versions,sourceVersions})).slice(0,12),directory=resolve(root,'data/derived/party-drift/admissible-panels-'+version);
await mkdir(directory,{recursive:true});
const results=[];
for(const target of input.history.events.slice(2))for(const horizon of [730,365,90,15]){
  const panel=prepareAdmissibleCase(input,target.id,horizon);
  for(const e of panel.elections)for(const c of e.rows){
    if(c.counts.reduce((s,v)=>s+v,0)+c.nonValid!==c.electors)throw new Error('Panel changed electorate conservation');
    if(new Set(c.groups.flat()).size!==c.groups.flat().length)throw new Error('Panel ballot is not a partition');
  }
  const row={target:target.id,horizon,cutoff:panel.cutoff,components:panel.panel.ids,panel:panel.panel,
    municipalities:panel.totalCities,municipalElectionCells:panel.elections.reduce((s,e)=>s+e.rows.length,0),
    polls:panel.polls.map(p=>({id:p.id,date:p.date,available:p.available,groups:p.groups,shares:p.shares})),
    electionCells:panel.elections.map(e=>({id:e.id,date:e.date,rows:e.rows.length,groupRange:[Math.min(...e.rows.map(c=>c.groups.length)),Math.max(...e.rows.map(c=>c.groups.length))]})),
    scope:panel.scope};results.push(row);
  console.log(JSON.stringify({target:target.id,horizon,components:row.components.length,cities:row.municipalities,polls:row.polls.length,resolved:row.panel.audit?.resolved}));
}
await writeFile(resolve(directory,'panels.json'),JSON.stringify({version,sourceVersions,inputs:input.versions,excludedPolls:input.excluded,
  authorization:'Luca selected an admissible research panel on 2026-10-26; conditional on known future ballot memberships; no held-out results select the partition',results},null,2)+'\n');
console.log(JSON.stringify({directory,cases:results.length}));
