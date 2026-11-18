import { describe, it, expect } from 'vitest';
import { gzipSync } from 'fflate';
import { archiveWorkflow, scenarioWorkflow } from './archive-routing';
const scenario = (schemaVersion: number) => ({
  kind: 'politics-reform-scenario',
  schemaVersion,
});
describe('latest-only archive dispatch', () => {
  it('accepts current v5 and rejects older archives without converting their models',async()=>{
    for(const version of [1,2,3,4,5]) for(const compressed of [false,true]) {
      const text=JSON.stringify(scenario(version)),data=compressed?gzipSync(new TextEncoder().encode(text)):text;
      const file=new File([data as BlobPart],compressed?'scenario.json.gz':'scenario.json');
      if(version===5) expect(await archiveWorkflow(file)).toBe('fast');
      else await expect(archiveWorkflow(file)).rejects.toThrow('Unsupported');
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
      scenario(6),
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
          ['{"scenario":' + JSON.stringify(scenario(5)) + ',"result":INVALID'],
          'large.json',
        ),
      ),
    ).toBe('fast');
  });
});
