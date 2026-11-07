import { readFileSync } from 'node:fs';
import { expect, it } from 'vitest';
import { collectCommonPoliticsBaseline } from './common-politics-baseline';

it('replays CP-01 retained M01/M02 generation and candidature evidence', async () => {
  const expected = JSON.parse(readFileSync(new URL('../../tests/fixtures/common-politics-baseline.json', import.meta.url), 'utf8'));
  expect(await collectCommonPoliticsBaseline()).toEqual(expected);
}, 60_000);
