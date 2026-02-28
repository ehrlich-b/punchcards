import { chromium } from 'playwright';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

(async () => {
    const browser = await chromium.launch();
    const page = await browser.newPage({ viewport: { width: 1280, height: 720 } });
    await page.goto('file://' + join(__dirname, 'index.html'));
    await page.waitForTimeout(1500);

    // Load the Fibonacci program and run it for a nice screenshot
    await page.evaluate(() => {
        const fib = PROGRAM_LIBRARY.find(p => p.name === 'Fibonacci');
        if (fib) {
            suppressPunchCount = true;
            suppressCursorDispose = true;
            editor.setValue(fib.source);
            suppressCursorDispose = false;
            suppressPunchCount = false;
        }
    });
    await page.waitForTimeout(500);

    // Run the program
    await page.click('#run-btn');
    await page.waitForTimeout(500);

    await page.screenshot({ path: join(__dirname, 'screenshot-ide.png') });
    console.log('Saved screenshot-ide.png');

    await browser.close();
})();
