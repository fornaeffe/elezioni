// FV-01 evidence tooling only. No production imports, fitting, moves or deletion.
import { createHash } from 'node:crypto';
import { createReadStream } from 'node:fs';
import { readFile, readdir, stat, mkdir, writeFile } from 'node:fs/promises';
import { resolve, dirname, relative, sep } from 'node:path';
import { fileURLToPath } from 'node:url';
import { execFileSync } from 'node:child_process';
import { gunzipSync } from 'node:zlib';
import assert from 'node:assert/strict';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const posix = p => p.replaceAll('\\', '/');
const local = p => {
  const value = resolve(root, p);
  if (!value.startsWith(resolve(root) + sep)) {
    throw new Error('Path escapes repository: ' + p);
  }
  return value;
};
const json = async p => JSON.parse(await readFile(local(p), 'utf8'));
const digest = bytes => createHash('sha256').update(bytes).digest('hex');
async function hashFile(p) {
  const h = createHash('sha256');
  for await (const chunk of createReadStream(local(p))) h.update(chunk);
  return h.digest('hex');
}
async function walk(p) {
  const result = [];
  for (const entry of await readdir(local(p), { withFileTypes: true })) {
    if (entry.isSymbolicLink()) throw new Error('Unexpected symlink: ' + p + '/' + entry.name);
    const name = p + '/' + entry.name;
    if (entry.isDirectory()) result.push(...await walk(name));
    else if (entry.isFile()) result.push(name);
  }
  return result.sort();
}
async function immutable(p, bytes) {
  await mkdir(dirname(local(p)), { recursive: true });
  let previous;
  try { previous = await readFile(local(p)); }
  catch (e) { if (e.code !== 'ENOENT') throw e; }
  if (previous && !previous.equals(Buffer.from(bytes))) throw new Error('Frozen output differs: ' + p);
  if (!previous) await writeFile(local(p), bytes);
}
const serialize = value => JSON.stringify(value, null, 2) + '\n';
const archiveDocs = [
  'correspondence-model', 'vote-generation-research', 'vote-generation-methods',
  'vote-generation-drift-model', 'vote-generation-drift-results', 'vote-generation-drift-testing',
  'vote-generation-party-drift-stage0', 'vote-generation-party-drift-restricted',
  'vote-generation-party-memberships', 'vote-generation-fast-research', 'polls-uncertainty', 'polls-calibration'
].map(p => 'docs/' + p + '.md');
const derivedRoots = ['generation', 'drift', 'party-drift'].map(p => 'data/derived/' + p);
const manifestPath = 'research/vote-generation/fv01/evidence-manifest.json';

async function freeze() {
  const revision = execFileSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8' }).trim();
  const tracked = execFileSync('git', ['ls-files', '-z'], { cwd: root, encoding: 'utf8' }).split('\0').filter(Boolean);
  const sourceFiles = tracked.filter(p => archiveDocs.includes(p) ||
    /^web\/scripts\/data-preparation\/(generation-|drift-|party-drift-|poll-(calibration|uncertainty|references))/.test(p) ||
    p.startsWith('web/scripts/data-preparation/stan/') || p.startsWith('data/sources/party-drift/') ||
    /^data\/sources\/polls\/.*\.json$/.test(p) || p === 'data/reform-model-source-manifest.json');
  const generated = [...(await Promise.all(derivedRoots.map(walk))).flat(),
    ...await walk('data/derived/polls'),
    ...(await walk('web/scripts/data-preparation/stan')).filter(p => !tracked.includes(p))].sort();
  const evidence = [];
  const recipe = p => p.includes('/polls/') ? (p.includes('poll-uncertainty-') ? 'docs/polls-uncertainty.md' : 'docs/polls-calibration.md') :
    p.includes('/generation/') ? 'docs/vote-generation-research.md' :
    p.includes('/drift/') ? 'docs/vote-generation-drift-testing.md' : 'docs/vote-generation-fast-research.md#reproduction-and-file-map';

  async function preserve(p, mode = 'copy', omitted = []) {
    const bytes = await readFile(local(p));
    let kept = bytes;
    if (p.endsWith('/cases.json')) {
      const value = JSON.parse(bytes);
      omitted = (value.cases ?? []).flatMap((row, i) => Array.isArray(row.forecast?.scores?.rows) ? ['cases.' + i + '.forecast.scores.rows'] : []);
      mode = 'json-extract';
    }
    if (mode === 'json-extract') {
      const value = JSON.parse(bytes);
      for (const key of omitted) {
        const path = key.split('.'); let parent = value;
        for (const part of path.slice(0, -1)) parent = parent?.[part];
        if (parent && Object.hasOwn(parent, path.at(-1))) delete parent[path.at(-1)];
      }
      kept = Buffer.from(JSON.stringify(value) + '\n');
    }
    const dest = 'research/vote-generation/fv01/evidence/' + p;
    await immutable(dest, kept);
    evidence.push({ originalPath: p, originalBytes: bytes.length, originalSha256: digest(bytes),
      retainedPath: dest, retainedBytes: kept.length, retainedSha256: digest(kept),
      mode, omittedPaths: omitted, reproductionGuide: recipe(p) });
  }
  for (const p of generated) {
    const name = p.split('/').at(-1);
    const size = (await stat(local(p))).size;
    const smallEvidence = /(?:summary|error|validate-|profile-)/.test(name) && p.endsWith('.json') && size <= 100000;
    const fastCases = /fast-(development|confirmation)[^/]*\/(summary|cases)\.json$/.test(p);
    const endpoint = /(?:report-[^/]+|camera-six-category-v1-[^/]+)\.json$/.test(p) && size < 1000000;
    const special = /\/(fast-development-frozen-20261027|fast-export-check-20261027|reduced-posterior-check)\.json$/.test(p) ||
      /modular-041250483558\/(manifest|fast-kind-fit)\.json$/.test(p);
    const pollResearch = /^data\/derived\/polls\/poll-(calibration|uncertainty)-.*\.json$/.test(p);
    if (p.endsWith('modular-041250483558/benchmark.json')) await preserve(p, 'json-extract', ['scores.rows']);
    else if (smallEvidence || fastCases || endpoint || special || pollResearch) await preserve(p);
  }

  // Record direct textual import consumers; dynamic/file-path consumers still
  // require FV-09 review, so this inventory is never a deletion authorization.
  const consumers = new Map();
  for (const p of tracked.filter(p => /\.(ts|svelte|mjs|py)$/.test(p))) {
    const text = await readFile(local(p), 'utf8');
    for (const match of text.matchAll(/(?:from\s*|import\s*\(|import\s*)['"](\.[^'"]+)['"]/g)) {
      const base = posix(relative(root, resolve(dirname(local(p)), match[1])));
      for (const candidate of [base, base + '.ts', base + '.mjs', base + '/index.ts']) {
        if (!sourceFiles.includes(candidate)) continue;
        consumers.set(candidate, [...new Set([...(consumers.get(candidate) ?? []), p])]);
      }
    }
  }
  const evidenceBySource = new Map(evidence.map(x => [x.originalPath, x]));
  const promote = /(?:generation-history|party-drift-(memberships|panel|identifiability)|poll-calibration-sources|poll-references)/;
  const inventory = [];
  for (const p of [...sourceFiles, ...generated].sort()) {
    const kept = evidenceBySource.get(p);
    const isSource = sourceFiles.includes(p);
    const disposition = p.startsWith('data/sources/') || p === 'data/reform-model-source-manifest.json' ? 'keep-source-review-promotion' :
      isSource ? (promote.test(p) ? 'split-promote-then-archive' : 'archive-after-consumer-audit') :
      p.startsWith('data/derived/polls/') && !/^poll-(calibration|uncertainty)-/.test(p.split('/').at(-1)) ? 'mixed-active-poll-artifact-keep-review-required' :
      kept ? 'evidence-preserved-original-disposable-after-fv09-audit' :
      /\.(json|npz|csv|gz|log|txt|pyc)$/.test(p) ? 'regenerable-delete-candidate-after-fv09-audit' : 'manual-review-keep';
    inventory.push({ path: p, bytes: (await stat(local(p))).size, sha256: await hashFile(p),
      category: isSource ? 'tracked-source' : 'generated-research', disposition,
      directImportConsumers: (consumers.get(p) ?? []).sort(),
      retainedEvidence: kept?.retainedPath ?? null,
      regenerationGuide: isSource ? null : recipe(p), deletionAuthorized: false });
  }
  const inventoryBytes = inventory.map(x => JSON.stringify(x)).join('\n') + '\n';
  await immutable('research/vote-generation/fv01/inventory.jsonl', inventoryBytes);

  const catalogPath = 'web/src/lib/politics/reform-2026/data/generation-catalog.generated.json';
  const catalog = await json(catalogPath);
  const defaultPath = 'web/src/lib/politics/reform-2026/data/' + catalog.defaultsPath;
  const defaults = await json(defaultPath);
  const snapshotPath = 'web/static/' + catalog.path;
  const snapshotBytes = await readFile(local(snapshotPath));
  assert.equal(digest(snapshotBytes), catalog.sha256);
  const snapshot = JSON.parse(gunzipSync(snapshotBytes));
  const policy = await json('data/sources/party-drift/memberships-policy.json');
  const proposed = (item, ch) => {
    if (item.id.startsWith('group-')) return { id: item.id, name: item.name,
      proposedComponentIds: ['opaque:' + ch + ':' + item.id], status: 'special-source-group-review-required' };
    const rule = policy.eventOverrides.find(x => x.event === 'camera-2022' && x.label === item.name) ??
      policy.joints.find(x => x.label === item.name) ?? policy.aliases.find(x => x.labels?.includes(item.name));
    return { id: item.id, name: item.name, proposedComponentIds: rule?.members ?? (rule?.id ? [rule.id] :
      ['atomic-' + digest(item.name.trim().normalize('NFKC')).slice(0, 10)]),
      status: rule ? 'provisional-research-membership-not-approved-default' : 'opaque-source-label-review-required' };
  };
  const colleges = snapshot.seats.geography.colleges;
  const physical = colleges.filter(x => x.kind !== 'taa-proportional');
  const domain = {
    schemaVersion: 1, status: 'coverage-inventory-not-a-generated-default',
    catalog: { path: catalogPath, sha256: await hashFile(catalogPath), ...catalog },
    sourceDefault: { path: defaultPath, sha256: await hashFile(defaultPath),
      referenceDate: defaults.projectionDate, electionDate: defaults.electionDate },
    sourceSnapshot: { path: snapshotPath, sha256: digest(snapshotBytes) },
    lists: Object.fromEntries(['camera', 'senato'].map(ch => [ch, defaults.lists[ch].map(x => proposed(x, ch))])),
    componentIds: [...new Set(['camera', 'senato'].flatMap(ch => defaults.lists[ch].flatMap(x => proposed(x, ch).proposedComponentIds)))].sort(),
    territories: snapshot.seats.geography.territories,
    physicalColleges: physical,
    derivedViews: colleges.filter(x => x.kind === 'taa-proportional'),
    municipalityColumns: ['code', 'regionId', 'provinceId'],
    municipalities: snapshot.municipalities.map(x => [x.code, x.regionId, x.provinceId]),
    cellCoverage: physical.map(c => ({ chamber: c.chamber, collegeId: c.id,
      municipalityIds: [...new Set(snapshot.memberships.filter(m => m.chamber === c.chamber && m.collegeId === c.id).map(m => m.municipalityCode))].sort() })),
    reviewGates: ['Review provisional memberships and cutoff-specific distinguishing information',
      'Do not alias special candidate groups to ordinary lists by matching label',
      'Prepare all physical territories including Valle d Aosta and split municipalities',
      'Senate 2022 common-age baseline and separately labelled 2018 age audit',
      'No current poll vintage or new prefitted/default acceptance is asserted']
  };
  const domainPath = 'research/vote-generation/fv01/default-domain.json';
  await immutable(domainPath, serialize(domain));
  const files = [domainPath, 'research/vote-generation/fv01/inventory.jsonl'];
  const manifest = { schemaVersion: 1, frozenOn: '2026-10-28', sourceRevision: revision,
    sourceRevisionRole: 'Base HEAD, not a new commit; exact working-tree hashes are in inventory',
    modifiedTrackedSources: execFileSync('git', ['diff', 'HEAD', '--name-only'], { cwd: root, encoding: 'utf8' }).split('\n').filter(p => sourceFiles.includes(p)),
    scope: 'FV-01 only; source locations unchanged and no deletion authorized',
    authority: { selectedModel: 'fast-share-kind-common-conservative-v1',
      listReferenceTurnout: 'User explicitly selected fixed turnout; non-valid intervention is separate' },
    interpretation: { fast: 'Corrected confirmation after loader/optimizer defects, not untouched holdout',
      older: 'Retained failures and diagnostic-only artifacts are not accepted production posteriors' },
    environment: { node: process.version, platform: process.platform,
      researchBenchmark: 'i7-14700KF, WSL Ubuntu, one BLAS/OMP thread; see retained original diagnostics' },
    evidence, inventory: { path: files[1], sha256: await hashFile(files[1]), entries: inventory.length,
      directConsumerAudit: 'Textual relative imports only; dynamic/path consumers require FV-09 review',
      totals: [...derivedRoots, 'data/derived/polls', 'web/scripts/data-preparation/stan'].map(path => ({ path, files: inventory.filter(x => x.path.startsWith(path + '/')).length,
        bytes: inventory.filter(x => x.path.startsWith(path + '/')).reduce((s, x) => s + x.bytes, 0) })) },
    defaultDomain: { path: domainPath, sha256: await hashFile(domainPath), municipalities: domain.municipalities.length,
      physicalColleges: physical.length, derivedViews: domain.derivedViews.length, componentsToReview: domain.componentIds.length },
    contractFixtures: { path: 'research/vote-generation/fv01/contract-fixtures.json',
      sha256: await hashFile('research/vote-generation/fv01/contract-fixtures.json') },
    reproduction: { verification: 'node research/vote-generation/fv01/freeze.mjs --check',
      capture: 'node research/vote-generation/fv01/freeze.mjs --write (original ignored artifacts required)',
      snapshotCreation: 'Preserved existing evidence only; no model or backtest was run',
      historicalCommands: 'Use indexed historical reports at the pinned source revision; keep original seeds/source hashes' }
  };
  await immutable(manifestPath, serialize(manifest));
  console.log(JSON.stringify({ evidenceFiles: evidence.length, retainedBytes: evidence.reduce((s, x) => s + x.retainedBytes, 0),
    inventoryEntries: inventory.length, municipalities: domain.municipalities.length, physicalColleges: physical.length,
    proposedComponents: domain.componentIds.length }));
}

async function check(checkSources = false) {
  const m = await json(manifestPath);
  for (const entry of m.evidence) {
    const bytes = await readFile(local(entry.retainedPath));
    assert.equal(bytes.length, entry.retainedBytes, entry.retainedPath);
    assert.equal(digest(bytes), entry.retainedSha256, entry.retainedPath);
    if (entry.mode === 'copy') assert.equal(entry.retainedSha256, entry.originalSha256);
  }
  assert.equal(await hashFile(m.inventory.path), m.inventory.sha256);
  assert.equal(await hashFile(m.defaultDomain.path), m.defaultDomain.sha256);
  assert.equal(await hashFile(m.contractFixtures.path), m.contractFixtures.sha256);
  const rows = (await readFile(local(m.inventory.path), 'utf8')).trim().split('\n').map(JSON.parse);
  assert.equal(rows.length, m.inventory.entries);
  assert.equal(new Set(rows.map(x => x.path)).size, rows.length);
  assert(rows.every(x => x.deletionAuthorized === false && /^[a-f0-9]{64}$/.test(x.sha256)));
  if (checkSources) {
    for (const entry of rows) {
      assert.equal((await stat(local(entry.path))).size, entry.bytes, entry.path);
      assert.equal(await hashFile(entry.path), entry.sha256, entry.path);
    }
    for (const entry of m.evidence.filter(x => x.mode === 'json-extract')) {
      const original = await json(entry.originalPath);
      for (const key of entry.omittedPaths) {
        const path = key.split('.'); let parent = original;
        for (const part of path.slice(0, -1)) parent = parent?.[part];
        assert(parent && Object.hasOwn(parent, path.at(-1)), 'Omitted source path missing: ' + key);
        delete parent[path.at(-1)];
      }
      assert.deepEqual(await json(entry.retainedPath), original, entry.retainedPath);
    }
  }
  const d = await json(m.defaultDomain.path);
  assert.equal(d.municipalities.length, m.defaultDomain.municipalities);
  const codes = new Set(d.municipalities.map(x => x[0]));
  assert.equal(codes.size, d.municipalities.length);
  for (const ch of ['camera', 'senato']) {
    const covered = new Set(d.cellCoverage.filter(x => x.chamber === ch).flatMap(x => x.municipalityIds));
    assert.deepEqual(covered, codes, ch + ' physical coverage');
  }
  assert(d.cellCoverage.every(x => x.municipalityIds.length > 0));
  const report = await readFile(local('docs/vote-generation-fast-research.md'), 'utf8');
  const equations = report.slice(report.indexOf('## National reference estimation'), report.indexOf('## Scenario contract and browser boundary')).replaceAll('\r\n', '\n');
  const model = (await readFile(local('docs/vote-generation-fast-model.md'), 'utf8')).replaceAll('\r\n', '\n');
  assert.equal(model.slice(model.indexOf('## National reference estimation')), equations, 'unchanged extracted numerical contract');
  const fixtures = await json(m.contractFixtures.path);
  const close = (actual, expected) => assert(Math.abs(actual - expected) < 1e-12, `${actual} differs from ${expected}`);
  const sum = values => values.reduce((s, x) => s + x, 0);
  const base = fixtures.workedReferences.base;
  for (const name of ['hardA40', 'newN10', 'nonValid40']) {
    const example = fixtures.workedReferences[name];
    const a = name === 'nonValid40' ? 0.4 : base.nonValid;
    close(example.electorProbabilities[0], a);
    close(sum(example.validShares), 1);
    example.validShares.forEach((p, i) => close((1 - a) * p, example.electorProbabilities[i + 1]));
    close(sum(example.electorProbabilities), 1);
  }
  const removed = fixtures.workedReferences.choiceRemoval;
  const surviving = removed.baseElectorProbabilities.filter((_, i) => i !== removed.removePoliticalIndex + 1);
  surviving.forEach((p, i) => close(p / sum(surviving), removed.expectedRemainingElectorProbabilities[i]));
  for (const name of ['groupedObservation', 'pollOther']) {
    const e = fixtures.workedReferences[name];
    const groups = e.groups ?? [...e.namedGroups, e.otherMembers];
    const offered = e.offered ?? e.validShares.map((_, i) => i);
    const total = sum(offered.map(i => e.validShares[i]));
    groups.forEach((g, i) => close(sum(g.map(j => e.validShares[j])) / total, e.expectedShares[i]));
  }
  const soft = fixtures.workedReferences.singleSoft;
  close(soft.softBSdPercentagePoints / 100, soft.expectedSdFraction);
  close(1 - soft.hardA, soft.softBUpper);
  const scoped = fixtures.workedReferences.localPrecedence;
  close((scoped.region1A + scoped.nationalA) / 2, scoped.expectedEffectiveNationalA);
  const anchor = fixtures.workedReferences.commonAnchor;
  const logit = x => Math.log(x / (1 - x));
  const logistic = x => 1 / (1 + Math.exp(-x));
  const delta = Math.max(-1, Math.min(1, logit(anchor.senatoBaseline) - logit(anchor.cameraBaseline))) * 0.5 * anchor.gapMultiplier;
  const derived = [logistic(logit(anchor.anchor) - delta / 2), logistic(logit(anchor.anchor) + delta / 2)];
  close((logit(derived[0]) + logit(derived[1])) / 2, logit(anchor.anchor));
  assert.notEqual(derived[0], derived[1]);
  const full = fixtures.workedReferences.fullComplementarySoft;
  close(sum(full.means), 1); close(full.sdPercentagePoints[0], full.sdPercentagePoints[1]);
  assert.notEqual(fixtures.invalidReferences[0].sdPercentagePoints[0], fixtures.invalidReferences[0].sdPercentagePoints[1]);
  for (const e of fixtures.quadratureCases) {
    assert.equal(e.Q.length, e.components * e.blocks);
    e.Q.forEach((row, i) => row.forEach((x, j) => close(x, e.Q[j][i])));
    if (!e.mode) continue;
    const gradient = e.Q.map((row, i) => sum(row.map((q, j) => q * e.mode[j])) - e.b[i]);
    for (let block = 0; block < e.blocks; block++) {
      const indexes = Array.from({ length: e.components }, (_, j) => block * e.components + j);
      close(sum(indexes.map(i => e.mode[i])), 1);
      const free = indexes.filter(i => e.mode[i] > 0);
      free.forEach(i => close(gradient[i], gradient[free[0]]));
      indexes.filter(i => e.mode[i] === 0).forEach(i => assert(gradient[i] >= gradient[free[0]]));
    }
  }
  console.log(JSON.stringify({ passed: true, retainedEvidenceFiles: m.evidence.length, inventoryEntries: rows.length,
    physicalCoverage: 'both chambers complete against frozen target', unchangedNumericalExtraction: true,
    workedContractArithmetic: true, independentFixtureMatrices: fixtures.quadratureCases.length,
    verifiedOriginalInventoryEntries: checkSources ? rows.length : 0,
    numericalFitOrJointReferenceCalibrationRun: false }));
}

const mode = process.argv[2];
if (mode === '--write') { await freeze(); await check(); }
else if (mode === '--check') await check();
else if (mode === '--check-sources') await check(true);
else throw new Error('Use --write to capture, --check to verify retained evidence, or --check-sources to also verify original local artifacts.');
