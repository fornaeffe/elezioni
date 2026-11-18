import { describe, it, expect } from 'vitest';
import { originalShareFixture, sourceSelection } from '../../../../../tests/fixtures/fast-share-source-input';
import { encodeSource, decodeSourceRows, decodeAsset, compressAsset, canonicalJson, sourceKey, validateSourceEnvelope } from './source-codec';
import { compileSource, observableGroups, resolvedBasis } from './source-compiler';
import { fitProfiles } from './profiles';
import { fitReference } from './information';
const prepare = async () => { const input = originalShareFixture(), encoded = await encodeSource(input); return { input, ...encoded, read: async (p: string) => encoded.assets.get(p)! }; };
describe('FV-03 original compact sources and editable fitting boundary', () => {
  it('retains exact elector/direct/attributed counts, observed zeros and missing histories', async () => {
    const { input, envelope, read } = await prepare();
    for (const e of envelope.payload.events) expect(await decodeSourceRows(envelope, e.id, await read(e.chunk.path))).toEqual(input.events.find(x => x.id === e.id)!.rows);
    const result = await compileSource(envelope, read, sourceSelection());
    const first = result.national.elections.find(e => e.id === 'e0')!;
    expect(first.electors).toBe(1100); expect(first.counts).toEqual([340,10,320,430]);
    expect(result.profiles.elections[0].rows.find(r => r.id === '001')!.counts).toEqual([0,300,400]);
    expect(result.profiles.elections.every(e => !e.rows.some(r => r.id === '002'))).toBe(true);
    const profiles = fitProfiles(result.profiles);
    expect(profiles.municipalityIds).toEqual(['001','002']);
    expect([...profiles.fallback.slice(4)]).toEqual([2,2,2,2]);
  });
  it('produces deterministic immutable bytes for reordered source sets', async () => {
    const a = await prepare(), input = originalShareFixture(); input.units.reverse(); input.choices.reverse(); input.events.reverse(); input.events.forEach(e => e.rows.reverse());
    const b = await encodeSource(input);
    expect(b.envelope.id).toBe(a.envelope.id);
    for (const [p, bytes] of a.assets) expect(b.assets.get(p)).toEqual(bytes);
    const zipped = await compressAsset('metadata', new TextEncoder().encode(canonicalJson(a.envelope)));
    expect(JSON.parse(new TextDecoder().decode(await decodeAsset(zipped.descriptor, zipped.compressed)))).toEqual(a.envelope);
  });
  it('detects tampered bytes, shape changes, oversized values and broken geography', async () => {
    const { envelope, read } = await prepare(), e = envelope.payload.events[0], corrupt = (await read(e.chunk.path)).slice(); corrupt[20] ^= 1;
    await expect(decodeSourceRows(envelope, e.id, corrupt)).rejects.toThrow('INTEGRITY');
    const layout = structuredClone(envelope); layout.payload.events[0].chunk.columns.count.offset++;
    await expect(decodeSourceRows(layout, e.id, await read(e.chunk.path))).rejects.toThrow('layout');
    const bad = originalShareFixture(); bad.events[0].rows[0].choices[0].count = 2 ** 32;
    await expect(encodeSource(bad)).rejects.toThrow('UINT32');
    const geography = originalShareFixture(); geography.geography.cells.pop();
    await expect(encodeSource(geography)).rejects.toThrow('geography');
    const unknown = structuredClone(envelope); unknown.schemaVersion = 2 as 1;
    await expect(validateSourceEnvelope(unknown)).rejects.toThrow('version');
    await expect(sourceKey('x', { n: NaN })).rejects.toThrow('Nonfinite');
  });
  it('constructs Other before pooling named and unnamed minor support', async () => {
    const { envelope, read } = await prepare(), selection = sourceSelection(); selection.components = ['a','minor']; selection.componentMap = { b: 'minor', c: 'minor' };
    const result = await compileSource(envelope, read, selection);
    expect(result.national.polls[0].shares).toEqual([0.5,0.5]);
    expect(result.national.polls[0].groups).toEqual([['a'],['minor']]);
    expect(result.national.elections[0].counts).toEqual([340,10,750]);
  });
  it('rebuilds changed memberships and dates, while source counts remain immutable', async () => {
    const { envelope, read } = await prepare(), before = canonicalJson(envelope), selection = sourceSelection();
    const a = await compileSource(envelope, read, selection);
    const edited = structuredClone(selection); edited.sourceMemberships = { '0-a': { members: ['b'], provenance: 'Reviewed hypothetical correspondence' }, '0-b': { members: ['a'], provenance: 'Corresponding disjoint reassignment' } };
    const b = await compileSource(envelope, read, edited);
    expect(b.nationalInputKey).not.toBe(a.nationalInputKey); expect(b.profileInputKey).not.toBe(a.profileInputKey);
    expect(b.national.elections[0].counts.reduce((n, x) => n + x, 0)).toBe(1100);
    const later = await compileSource(envelope, read, { ...selection, referenceDate: '2017-01-02' });
    expect(later.nationalInputKey).not.toBe(a.nationalInputKey); expect(later.profileInputKey).not.toBe(a.profileInputKey);
    expect(canonicalJson(envelope)).toBe(before);
  });
  it('keeps citation/display edits out of numerical keys', async () => {
    const a = await prepare(), original = originalShareFixture(); original.choices[0].label = 'Corrected display label'; original.choices[0].citations.push('another citation');
    original.citations.push({ id: 'another citation', url: 'https://example.org/correction' });
    const b = await encodeSource(original), x = await compileSource(a.envelope, a.read, sourceSelection()), y = await compileSource(b.envelope, async p => b.assets.get(p)!, sourceSelection());
    expect(y.nationalInputKey).toBe(x.nationalInputKey); expect(y.profileInputKey).toBe(x.profileInputKey); expect(y.provenanceId).not.toBe(x.provenanceId);
  });
  it('rejects unavailable/unknown/duplicate/unsupported observations and opaque splits', async () => {
    const a = await prepare(), s = sourceSelection();
    await expect(compileSource(a.envelope, a.read, { ...s, referenceDate: '2016-01-02' })).rejects.toThrow('unavailable');
    await expect(compileSource(a.envelope, a.read, { ...s, electionIds: ['missing'] })).rejects.toThrow('Unknown');
    await expect(compileSource(a.envelope, a.read, { ...s, pollIds: ['q1','q1'] })).rejects.toThrow('Duplicate');
    await expect(compileSource(a.envelope, a.read, { ...s, sourceMemberships: { '0-c': { members: ['a','c'], provenance: 'Unsupported guessed split' } } })).rejects.toThrow('opaque split');
    await expect(compileSource(a.envelope, a.read, { ...s, sourceMemberships: { '0-a': { members: ['a','b'], provenance: 'Colliding correspondence' } } })).rejects.toThrow('competes');
    const input = originalShareFixture(); input.events[0].context = 'senato'; input.events[0].minimumAge = 25; input.events[0].role = 'age-audit';
    const older = await encodeSource(input);
    await expect(compileSource(older.envelope, async p => older.assets.get(p)!, s)).rejects.toThrow('audit-only');
  });
  it('admits an election on its availability day, without moving observation time or admitting it the day before', async () => {
    const input = originalShareFixture(); input.events[3].availableDate = '2016-01-02';
    input.events[3].availabilityBasis = 'Assumed available by end of first civil day after final polling day 2016-01-01';
    const encoded = await encodeSource(input), read = async (p: string) => encoded.assets.get(p)!;
    await expect(compileSource(encoded.envelope, read, { ...sourceSelection(), referenceDate: '2016-01-01' })).rejects.toThrow('unavailable');
    const result = await compileSource(encoded.envelope, read, { ...sourceSelection(), referenceDate: '2016-01-02' });
    expect(result.national.elections.find(e => e.id === 'e3')!.date).toBe('2016-01-01');
    expect(result.national.referenceDate).toBe('2016-01-02');
  });
  it('checks distinguishing information and retains zero-denominator rows without local use', async () => {
    expect(resolvedBasis(['a','b','c'], [[['a','b'],['c']]])).toBe(false);
    expect(resolvedBasis(['a','b'], [[['a']]])).toBe(false);
    expect(resolvedBasis(['a','b','c'], [[['a'],['b']],[['b'],['c']]])).toBe(true);
    expect(observableGroups([['a','b'],['b','c']], [20,30])).toEqual([{ members: ['a','b','c'], count: 50 }]);
    const input = originalShareFixture(); input.events[0].rows[1].electors = 0;
    const encoded = await encodeSource(input), result = await compileSource(encoded.envelope, async p => encoded.assets.get(p)!, sourceSelection());
    expect(result.excludedLocal).toEqual([{ event: 'e0', unit: 'X', reason: 'Invalid original denominator; retained nationally, no local likelihood' }]);
    expect(result.national.elections[0].counts).toEqual([240,10,320,430]);
  });
  it('runs the same numerical engine on compiled original observations', async () => {
    const { envelope, read } = await prepare(), compiled = await compileSource(envelope, read, sourceSelection());
    const fitted = fitReference(compiled.national, 'fv03-reduced-prefit');
    expect(fitted.political.diagnostics.passed).toBe(true); expect(fitted.participation.diagnostics.passed).toBe(true);
  });
});
