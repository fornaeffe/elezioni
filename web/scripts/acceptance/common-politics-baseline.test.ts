import { readFileSync } from 'node:fs';
import { expect, it } from 'vitest';
import { collectCommonPoliticsBaseline } from './common-politics-baseline';

it('replays unchanged M01 CP-01 evidence; M02 2.0.0 remains historical evidence', async () => {
  const expected = JSON.parse(readFileSync(new URL('../../tests/fixtures/common-politics-baseline.json', import.meta.url), 'utf8'));
  const current = await collectCommonPoliticsBaseline();
  expect(current.records.filter(r => r.model === 'M01')).toEqual(expected.records.filter((r: { model: string }) => r.model === 'M01'));
}, 60_000);
