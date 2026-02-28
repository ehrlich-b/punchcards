import { chromium } from 'playwright';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

(async () => {
    const browser = await chromium.launch();
    const page = await browser.newPage({ viewport: { width: 1600, height: 900 } });
    await page.goto('file://' + join(__dirname, 'index.html'));
    await page.waitForTimeout(1000);

    const buf = await page.evaluate(() => {
        const text = 'HELLO, WORLD. IBM 029 CARD PUNCH. ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789 +-*/=()';
        const card = createCard(text);

        // Temporarily patch CARD_SPEC to disable shadow for transparent render
        const origShadow = CARD_SPEC.shadowColor;
        CARD_SPEC.shadowColor = 'transparent';

        const canvas = document.createElement('canvas');
        renderCard(canvas, card, 1200);

        CARD_SPEC.shadowColor = origShadow;
        return canvas.toDataURL('image/png');
    });

    // Strip data URL prefix, write binary
    const base64 = buf.replace(/^data:image\/png;base64,/, '');
    const fs = await import('fs');
    fs.writeFileSync(join(__dirname, 'card-showcase.png'), Buffer.from(base64, 'base64'));
    console.log('Saved card-showcase.png');

    await browser.close();
})();
