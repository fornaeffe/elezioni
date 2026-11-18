/** Compare restored laws with the pinned Git parent on the same current scenario; version metadata is separate. */
import { readFile, writeFile, mkdir, realpath, symlink } from 'node:fs/promises';
import { execFileSync } from 'node:child_process';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { unzipSync } from 'fflate';
import { resolve, dirname, relative, isAbsolute } from 'node:path';
import { createHash } from 'node:crypto';
import { loadCandidate, loadCandidateSource } from '../../src/lib/politics/reform-2026/vote-share/release.ts';
import { compileLegacy } from '../../src/lib/politics/reform-2026/vote-share/legacy-adapter.ts';
import { prepareScenarioGeneration } from '../../src/lib/politics/reform-2026/vote-share/unified-generation.ts';
import { readBytes } from '../data-preparation/fast-share-release-io.ts';
const root = fileURLToPath(new URL('../../../', import.meta.url)),
 output = resolve(root, 'data/derived/model-rollback'),
 baselineCommit = '1c19d54d4ccd0e0f3a05940c3cd28fa5bcab0a1b',
 baselineRoot = resolve(output, 'pre-correction');
await mkdir(baselineRoot, { recursive: true });
const archive = execFileSync('git', ['archive', '--format=zip', baselineCommit, 'web/src/lib'],
 { cwd: root, maxBuffer: 64 * 1024 * 1024 });
for (const [name, data] of Object.entries(unzipSync(archive))) {
 const destination = resolve(baselineRoot, name), within = relative(baselineRoot, destination);
 if (within.startsWith('..') || isAbsolute(within)) throw new Error('BASELINE_PATH_OUTSIDE_OUTPUT');
 if (name.endsWith('/')) continue;
 await mkdir(dirname(destination), { recursive: true });
 await writeFile(destination, data);
}
const modules = resolve(root, 'web/node_modules'), link = resolve(baselineRoot, 'web/node_modules');
try {
 if (await realpath(link) !== await realpath(modules)) throw new Error('UNEXPECTED_BASELINE_MODULES');
} catch (error) {
 if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw error;
 await symlink(modules, link, process.platform === 'win32' ? 'junction' : 'dir');
}
const { compileLegacy: baselineCompile } = await import(pathToFileURL(resolve(baselineRoot,
 'web/src/lib/politics/reform-2026/vote-share/legacy-adapter.ts')).href),
 { prepareScenarioGeneration: baselinePrepare } = await import(pathToFileURL(resolve(baselineRoot,
 'web/src/lib/politics/reform-2026/vote-share/unified-generation.ts')).href);
const dir=resolve('static/data/reform-2026/fast-share'), read=readBytes(dir),
 pointer=JSON.parse(await readFile(resolve(dir,'candidate.json'),'utf8')),
 loaded=await loadCandidate(pointer,read), source=await loadCandidateSource(loaded.catalog,read);
const normalized=(value:unknown):unknown=>typeof value==='string'&&value.startsWith('unified-legacy-input-v1-')?'unified-legacy-input-v1-[public-selector-version-dependent]':Array.isArray(value)?value.map(normalized):value&&typeof value==='object'?
 Object.fromEntries(Object.entries(value).filter(([key])=>key!=='parameterVersion'&&key!=='modelVersion').sort(([a],[b])=>a.localeCompare(b)).map(([key,item])=>[key,normalized(item)])):value;
const digest=(value:unknown)=>createHash('sha256').update(JSON.stringify(normalized(value))).digest('hex');
const results=[];
for(const model of ['M02','M01'] as const){
 const scenario=structuredClone(loaded.scenario);scenario.generation!.model=model;
 const old=structuredClone(scenario) as any;old.generation.model=model==='M02'?'selected':'legacy-poll-informed-v1';delete old.generation.modelVersion;
 const legacy=model==='M01'?await compileLegacy(scenario,source,read,loaded.assets,loaded.context):undefined,
  previous=model==='M01'?await baselineCompile(old,source,read,loaded.assets,loaded.context):undefined,
  current=prepareScenarioGeneration(scenario,loaded.assets,loaded.context,legacy),
  baseline=baselinePrepare(old,loaded.assets,loaded.context,previous);
 for(const id of [1,2,7]){
  const a=current.run(id),b=baseline.run(id),currentHash=digest(a.input),baselineHash=digest(b.input);
  if(currentHash!==baselineHash){await writeFile(resolve('../data/derived/model-rollback/parity-failure.json'),JSON.stringify({model,id,current:normalized(a.input),baseline:normalized(b.input)}));throw new Error('RESTORATION_PARITY_FAILED:'+model+'/'+id);}
  results.push({model,version:'2.0.0',id,currentHash,baselineHash,trace:a.trace.modelVersion});
 }
}
const report={status:'passed',baselineCommit,catalog:pointer.catalogId,excludedMetadata:['parameterVersion','modelVersion','unified-legacy-input-v1 hashes include public selector/version'],results};
await writeFile(resolve('../data/derived/model-rollback/parity.json'),JSON.stringify(report,null,2)+'\n');
console.log(JSON.stringify(report));
