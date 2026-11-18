import { defineConfig } from '@playwright/test';
import general from './playwright.config';
export default defineConfig(general, { testMatch: 'base-smoke.spec.ts', testIgnore: [], workers: 1, fullyParallel: false });
