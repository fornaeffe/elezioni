import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig } from 'vitest/config';
import { resolve } from 'node:path';
import { implementationKey } from './scripts/data-preparation/fast-share-release-io';

export default defineConfig(async () => ({
  define: {
    __COMMON_POLITICS_IMPLEMENTATION__: JSON.stringify(await implementationKey(resolve('..'), 'web/src/lib/workers/politics-compute.ts')),
    __FAST_REFORM_CANDIDATE__: process.env.REFORM_FAST_CANDIDATE !== '0',
    __FAST_REFORM_IMPLEMENTATIONS__: JSON.stringify(
      Object.fromEntries(
        await Promise.all(
          Object.entries({
            generation: 'unified-generation',
            fitting: 'information',
            profiles: 'senate-adapter',
          }).map(async ([key, entry]) => [
            key,
            await implementationKey(
              resolve('..'),
              `web/src/lib/politics/reform-2026/vote-share/${entry}.ts`,
            ),
          ]),
        ),
      ),
    ),
  },
  plugins: [sveltekit()],
  test: {
    include: ['src/**/*.test.ts', 'scripts/**/*.test.ts'],
    environment: 'node',
  },
}));
