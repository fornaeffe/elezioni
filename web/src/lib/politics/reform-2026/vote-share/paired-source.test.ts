import { describe, it, expect } from 'vitest';
import { originalShareFixture, sourceSelection } from '../../../../../tests/fixtures/fast-share-source-input';
import { encodeSource } from './source-codec';
import { compilePairedHistory } from './paired-source';
function input() {
  const s = originalShareFixture(),
    camera = s.events[2],
    senato = {
      ...structuredClone(camera),
      id: 'senato-pair',
      context: 'senato' as const,
      role: 'paired-adaptation' as const
    };
  const choices = s.choices
    .filter((c) => c.eventId === camera.id)
    .map((c) => ({ ...c, id: 'senato-' + c.id, eventId: senato.id }));
  senato.rows.forEach((r) => r.choices.forEach((c) => (c.id = 'senato-' + c.id)));
  s.events.push(senato);
  s.choices.push(...choices);
  return s;
}
describe('FV-04 paired original source preparation', () => {
  it('retains original grouped counts, pooled observability and explicit exclusions', async () => {
    const source = input(),
      selection = sourceSelection();
    selection.components = ['a', 'minor'];
    selection.componentMap = { b: 'minor', c: 'minor' };
    const encoded = await encodeSource(source),
      read = async (p: string) => encoded.assets.get(p)!;
    const result = await compilePairedHistory(encoded.envelope, read, selection, {
      camera: 'e2',
      senato: 'senato-pair',
      electoratePolicy: 'equal-only'
    });
    expect(result.history.rows).toHaveLength(1);
    expect(result.history.rows[0].camera.counts).toEqual([100, 700]);
    expect(result.history.rows[0].senato.counts).toEqual([100, 700]);
    expect(result.history.rows[0].camera.nonValid).toBe(200);
    expect(result.audit.excluded).toHaveLength(2);
    expect(result.audit.conditionalElectors).toBe(1000);
    expect(source.events[2].rows[0].choices.map((c) => c.count)).toEqual([100, 300, 400]);
  });
  it('rejects unavailable/25+ history and inconsistent original denominators without repair', async () => {
    const source = input();
    source.events.at(-1)!.minimumAge = 25;
    source.events.at(-1)!.role = 'age-audit';
    let encoded = await encodeSource(source);
    await expect(
      compilePairedHistory(encoded.envelope, async (p) => encoded.assets.get(p)!, sourceSelection(), {
        camera: 'e2',
        senato: 'senato-pair',
        electoratePolicy: 'equal-only'
      })
    ).rejects.toThrow('18+');
    const mismatch = input();
    mismatch.events.at(-1)!.rows[0].electors++;
    encoded = await encodeSource(mismatch);
    await expect(
      compilePairedHistory(encoded.envelope, async (p) => encoded.assets.get(p)!, sourceSelection(), {
        camera: 'e2',
        senato: 'senato-pair',
        electoratePolicy: 'equal-only'
      })
    ).rejects.toThrow('No eligible');
  });
});
