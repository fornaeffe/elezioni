import { existsSync, readFileSync } from 'node:fs';
import { basename } from 'node:path';
import { describe, test } from 'vitest';

export function hasGeneratedFixture(path: string): boolean {
  return existsSync(path);
}

export function loadGeneratedJsonFixture<T>(path: string): T {
  return JSON.parse(readFileSync(path, 'utf8')) as T;
}

export function describeWithGeneratedFixtures(name: string, paths: readonly string[], callback: () => void): void {
  const missing = paths.filter((path) => !existsSync(path));

  if (missing.length > 0) {
    const suffix = ` (missing generated fixtures: ${missing.map((path) => basename(path)).join(', ')})`;
    describe(`${name}${suffix}`, () => {
      test.skip('requires generated fixture files', () => {});
    });
    return;
  }

  describe(name, callback);
}
