import { defineConfig } from '@playwright/test';
import general from './playwright.config';

// Production fitting belongs to explicit release acceptance.
export default defineConfig(general, { grepInvert: /@production/ });
