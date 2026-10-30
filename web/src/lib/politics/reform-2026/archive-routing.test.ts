import { describe, it, expect } from 'vitest';
import { gzipSync } from 'fflate';
import { archiveWorkflow, scenarioWorkflow } from './archive-routing';
const scenario = (schemaVersion: number) => ({
  kind: 'politics-reform-scenario',
  schemaVersion,
});
describe('original-generator archive dispatch', () => {
  it('routes v1-v3 and v4 without converting scenario models', async () => {
    for (const version of [1, 2, 3, 4])
      for (const compressed of [false, true]) {
        const text = JSON.stringify(scenario(version)),
          data = compressed ? gzipSync(new TextEncoder().encode(text)) : text;
        expect(
          await archiveWorkflow(
            new File(
              [data as BlobPart],
              compressed ? 'scenario.json.gz' : 'scenario.json',
            ),
          ),
        ).toBe(version === 4 ? 'fast' : 'installed');
        const root = JSON.stringify({
          schema_version: version === 4 ? 3 : 2,
          scenario: scenario(version),
          result: { tables: [] },
        });
        expect(await archiveWorkflow(new File([root], 'results.json'))).toBe(
          version === 4 ? 'fast' : 'installed',
        );
      }
  });
  it('recognizes fitted sessions and rejects unknown/missing versions', async () => {
    expect(
      await archiveWorkflow(
        new File(
          ['{"kind":"politics-reform-session","schemaVersion":1}'],
          'session.json',
        ),
      ),
    ).toBe('fast');
    for (const value of [
      scenario(5),
      scenario(0),
      { kind: 'politics-reform-scenario', schemaVersion: '3' },
      {},
    ])
      expect(() => scenarioWorkflow(value)).toThrow();
    await expect(
      archiveWorkflow(new File(['{"result":{}}'], 'file.json')),
    ).rejects.toThrow();
  });
  it('stops before malformed or arbitrarily large result details', async () => {
    expect(
      await archiveWorkflow(
        new File(
          ['{"scenario":' + JSON.stringify(scenario(3)) + ',"result":INVALID'],
          'large.json',
        ),
      ),
    ).toBe('installed');
  });
});
