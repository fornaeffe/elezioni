import { expect, test, type Locator } from '@playwright/test';
import { installEditorPreparation } from '../fixtures/editor-preparation';

const base = process.env.BASE_PATH ?? '';

test('shows the workflow selector on the landing page', async ({ page }) => {
  await page.goto(`${base}/`);

  await expect(page.getByRole('heading', { name: 'Scegli il flusso di simulazione' })).toBeVisible();
  await expect(page.getByRole('link', { name: /politiche/i })).toHaveAttribute('href', `${base}/politics`);
  await expect(page.getByRole('link', { name: /emilia-romagna/i })).toHaveAttribute('href', `${base}/emilia-romagna`);
});

function contentEdges(target: Locator) {
  return target.evaluate(element => {
    const { left, right } = element.getBoundingClientRect();
    const { paddingLeft, paddingRight } = getComputedStyle(element);
    return { left: left + parseFloat(paddingLeft), right: right - parseFloat(paddingRight) };
  });
}

for (const width of [390, 1024, 1600]) {
  test(`shares the page frame at ${width}px`, async ({ page }) => {
    await installEditorPreparation(page);
    await page.setViewportSize({ width, height: 900 });
    await page.goto(`${base}/`);

    const main = page.getByRole('main');
    const header = page.getByRole('banner');
    const footer = page.getByRole('contentinfo');
    await expect(main.getByRole('heading', { level: 1 })).toBeVisible();
    await expect(main).toHaveCount(1);
    await expect(header).toHaveCount(0);
    expect(await contentEdges(main.locator('.selector'))).toEqual(await contentEdges(main));
    await expect(footer).toBeInViewport({ ratio: 1 });
    expect(await page.evaluate(() => document.documentElement.scrollHeight)).toBe(900);

    for (const [route, sections] of [
      ['/politics/', ['.common-politics-editor', '.sessions']],
      ['/politics/editor/', ['.common-politics-editor', '.sessions']],
      ['/emilia-romagna/', ['.workspace-shell nav', '.regional-content']],
    ] as const) {
      await page.goto(`${base}${route}`);
      await expect(main.getByRole('heading', { level: 1 })).toBeVisible();
      await expect(main).toHaveCount(1);
      const frame = await contentEdges(main);
      expect(await contentEdges(header)).toEqual(frame);
      expect(await contentEdges(footer.locator('.page-container'))).toEqual(frame);
      for (const section of sections) {
        expect(await contentEdges(main.locator(section))).toEqual(frame);
      }
      expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(width);
    }

    await header.getByRole('link', { name: 'Home' }).click();
    await expect(page).toHaveURL(`${base}/`);
    await expect(header).toHaveCount(0);
    await expect(footer).toBeInViewport({ ratio: 1 });
  });
}

test('the full landing page can scroll on a short screen', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 280 });
  await page.goto(`${base}/`);

  const footer = page.getByRole('contentinfo');
  await expect(page.getByRole('heading', { level: 1 })).toBeInViewport({ ratio: 1 });
  await expect(footer).not.toBeInViewport();
  await page.keyboard.press('End');
  await expect(footer.getByRole('link')).toBeInViewport({ ratio: 1 });
});
