/** Keep local debug bridges out of the distributable without touching their sources. */
import { execFileSync } from 'node:child_process';
import { rm, readdir } from 'node:fs/promises';
import { resolve } from 'node:path';
const web = resolve(import.meta.dirname, '../..');
execFileSync(process.execPath, [resolve(web, 'node_modules/vite/bin/vite.js'), 'build'], { cwd: web, stdio: 'inherit' });
const directory = resolve(web, 'build/data/v1');
for (const name of await readdir(directory)) {
  if (name.includes('debug')) await rm(resolve(directory, name));
}
