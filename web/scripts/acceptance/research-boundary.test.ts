import { it, expect } from 'vitest';
import { readdirSync, readFileSync } from 'node:fs';
import { resolve, relative } from 'node:path';
import { fileURLToPath } from 'node:url';

it('keeps archived research outside active application and preparation imports', () => {
  const root = fileURLToPath(new URL('../../../', import.meta.url));
  const violations: string[] = [];
  for (const directory of ['web/src', 'web/scripts', 'web/tests']) {
    for (const entry of readdirSync(resolve(root, directory), { recursive: true })) {
      const name = String(entry);
      if (!/\.(?:ts|svelte|js|mjs)$/.test(name)) continue;
      const path = resolve(root, directory, name);
      const text = readFileSync(path, 'utf8');
      for (const match of text.matchAll(/(?:from\s*|import\s*\(|import\s*|require\s*\(|new URL\s*\()\s*['"]([^'"]+)['"]/g)) {
        const specifier = match[1];
        if (!specifier.startsWith('.')) continue;
        const target = relative(root, resolve(path, '..', specifier)).replaceAll('\\', '/');
        if (target.startsWith('research/')) violations.push(`${relative(root, path)} -> ${specifier}`);
      }
    }
  }
  expect(violations).toEqual([]);
});
