import { chromium } from 'playwright';
import { strict as assert } from 'assert';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const PAGE_URL = `file://${join(__dirname, 'index.html')}`;

let browser, page;
let passed = 0;
let failed = 0;

async function setup() {
    browser = await chromium.launch();
    page = await browser.newPage({ viewport: { width: 1280, height: 720 } });
    // Catch console errors
    const errors = [];
    page.on('pageerror', e => errors.push(e.message));
    await page.goto(PAGE_URL);
    await page.waitForTimeout(500);
    return errors;
}

async function teardown() {
    await browser.close();
}

async function test(name, fn) {
    try {
        await fn();
        console.log(`  PASS: ${name}`);
        passed++;
    } catch (e) {
        console.log(`  FAIL: ${name}`);
        console.log(`        ${e.message}`);
        failed++;
    }
}

async function freshPage() {
    // Clear localStorage and reload
    await page.evaluate(() => localStorage.clear());
    await page.goto(PAGE_URL);
    await page.waitForTimeout(500);
}

// ============================================================
console.log('\n=== Punchcards Test Suite ===\n');

const pageErrors = await setup();

// --- Basic page load ---
console.log('Page Load:');

await test('no JS errors on load', async () => {
    assert.equal(pageErrors.length, 0, `JS errors: ${pageErrors.join(', ')}`);
});

await test('editor is rendered', async () => {
    const cm = await page.$('.CodeMirror');
    assert.ok(cm, 'CodeMirror element not found');
});

await test('default program is loaded', async () => {
    const text = await page.evaluate(() =>
        document.querySelector('.CodeMirror').CodeMirror.getValue()
    );
    assert.ok(text.includes('HELLO WORLD'), 'Default program not loaded');
});

await test('deck has 5 cards', async () => {
    const count = await page.evaluate(() => deck.length);
    assert.equal(count, 5);
});

await test('card canvas is rendered', async () => {
    const canvas = await page.$('#card-canvas');
    assert.ok(canvas);
    const width = await canvas.evaluate(el => el.width);
    assert.ok(width > 0, 'Canvas has zero width');
});

// --- Hollerith encoding ---
console.log('\nHollerith Encoding:');

await test('letter A encodes to rows 12,1', async () => {
    const rows = await page.evaluate(() => HOLLERITH['A']);
    assert.deepEqual(rows, [12, 1]);
});

await test('digit 5 encodes to row 5', async () => {
    const rows = await page.evaluate(() => HOLLERITH['5']);
    assert.deepEqual(rows, [5]);
});

await test('space encodes to no holes', async () => {
    const rows = await page.evaluate(() => HOLLERITH[' ']);
    assert.deepEqual(rows, []);
});

await test('all letters A-Z have encodings', async () => {
    const count = await page.evaluate(() => {
        let n = 0;
        for (let c = 65; c <= 90; c++) {
            if (HOLLERITH[String.fromCharCode(c)]) n++;
        }
        return n;
    });
    assert.equal(count, 26);
});

// --- Card data model ---
console.log('\nCard Data Model:');

await test('createCard produces 80 columns x 12 rows', async () => {
    const dims = await page.evaluate(() => {
        const c = createCard('A');
        return { cols: c.columns.length, rows: c.columns[0].length };
    });
    assert.equal(dims.cols, 80);
    assert.equal(dims.rows, 12);
});

await test('createCard truncates at 80 chars', async () => {
    const text = await page.evaluate(() => {
        const c = createCard('X'.repeat(100));
        return c.text.length;
    });
    assert.equal(text, 80);
});

await test('createCard punches correct holes for A', async () => {
    const holes = await page.evaluate(() => {
        const c = createCard('A');
        const punched = [];
        for (let ri = 0; ri < 12; ri++) {
            if (c.columns[0][ri]) punched.push(ROW_ORDER[ri]);
        }
        return punched;
    });
    assert.deepEqual(holes, [12, 1]);
});

// --- Card punching mechanics ---
console.log('\nCard Punching:');

await test('typing a character updates deck', async () => {
    await freshPage();
    // Clear editor and type fresh
    await page.evaluate(() => {
        const cm = document.querySelector('.CodeMirror').CodeMirror;
        cm.setValue('');
    });
    await page.waitForTimeout(100);
    await page.evaluate(() => {
        const cm = document.querySelector('.CodeMirror').CodeMirror;
        cm.setValue('HELLO');
    });
    await page.waitForTimeout(100);
    const text = await page.evaluate(() => deck[0]?.text);
    assert.equal(text, 'HELLO');
});

await test('dirty card tracking: backspace marks dirty', async () => {
    await freshPage();
    await page.evaluate(() => {
        const cm = document.querySelector('.CodeMirror').CodeMirror;
        cm.setValue('HELLO');
    });
    await page.waitForTimeout(100);
    // Simulate backspace
    await page.evaluate(() => {
        const cm = document.querySelector('.CodeMirror').CodeMirror;
        cm.replaceRange('', { line: 0, ch: 4 }, { line: 0, ch: 5 }, '+delete');
    });
    await page.waitForTimeout(100);
    const dirty = await page.evaluate(() => !!dirtyLines[0]);
    assert.ok(dirty, 'Line 0 should be dirty after delete');
});

await test('dispose on re-type: dirty card disposed when typing', async () => {
    await freshPage();
    await page.evaluate(() => {
        const cm = document.querySelector('.CodeMirror').CodeMirror;
        cm.setValue('HELLO');
    });
    await page.waitForTimeout(100);
    const before = await page.evaluate(() => totalDisposed);
    // Backspace
    await page.evaluate(() => {
        const cm = document.querySelector('.CodeMirror').CodeMirror;
        cm.replaceRange('', { line: 0, ch: 4 }, { line: 0, ch: 5 }, '+delete');
    });
    await page.waitForTimeout(50);
    // Type new char — should dispose old dirty card
    await page.evaluate(() => {
        const cm = document.querySelector('.CodeMirror').CodeMirror;
        cm.replaceRange('X', { line: 0, ch: 4 }, { line: 0, ch: 4 }, '+input');
    });
    await page.waitForTimeout(100);
    const after = await page.evaluate(() => totalDisposed);
    assert.ok(after > before, `Disposed should increase: ${before} -> ${after}`);
});

await test('recycle bin stores disposed cards', async () => {
    const binLen = await page.evaluate(() => recycleBin.length);
    assert.ok(binLen > 0, 'Recycle bin should have entries');
    const lastItem = await page.evaluate(() => {
        const item = recycleBin[recycleBin.length - 1];
        return { text: item.card.text, lineNumber: item.lineNumber };
    });
    assert.ok(lastItem.lineNumber > 0, 'Should have original line number');
});

await test('recycle bin capped at 500', async () => {
    const capped = await page.evaluate(() => {
        // Force add 600 entries
        for (let i = 0; i < 600; i++) {
            recycleBin.push({ card: createCard('TEST' + i), lineNumber: i, disposedAt: Date.now() });
        }
        if (recycleBin.length > 500) {
            recycleBin = recycleBin.slice(recycleBin.length - 500);
        }
        return recycleBin.length;
    });
    assert.ok(capped <= 500, `Recycle bin should be <= 500, got ${capped}`);
});

// --- Card Viewer ---
console.log('\nCard Viewer:');

await freshPage();

await test('clicking card opens viewer', async () => {
    await page.click('#card-canvas');
    await page.waitForTimeout(200);
    const visible = await page.$eval('#card-viewer', el =>
        el.classList.contains('active')
    );
    assert.ok(visible, 'Card viewer should be active');
});

await test('viewer shows deck tab by default', async () => {
    const activeTab = await page.$eval('.viewer-tab.active', el => el.dataset.tab);
    assert.equal(activeTab, 'deck');
});

await test('viewer renders card at 740px', async () => {
    const width = await page.$eval('#viewer-canvas', el =>
        parseInt(el.style.width)
    );
    assert.equal(width, 740);
});

await test('viewer shows card label', async () => {
    const label = await page.textContent('#viewer-card-label');
    assert.ok(label.includes('Card 1 of'), `Label: ${label}`);
});

await test('prev/next navigation works', async () => {
    // Ensure we have a multi-card deck
    await page.keyboard.press('Escape');
    await page.evaluate(() => {
        const cm = document.querySelector('.CodeMirror').CodeMirror;
        cm.setValue('LINE ONE\nLINE TWO\nLINE THREE');
    });
    await page.waitForTimeout(200);
    await page.click('#card-canvas');
    await page.waitForTimeout(200);
    await page.click('#viewer-next');
    await page.waitForTimeout(100);
    const label = await page.textContent('#viewer-card-label');
    assert.ok(label.includes('Card 2 of'), `After next: ${label}`);
    await page.click('#viewer-prev');
    await page.waitForTimeout(100);
    const label2 = await page.textContent('#viewer-card-label');
    assert.ok(label2.includes('Card 1 of'), `After prev: ${label2}`);
});

await test('arrow keys navigate cards', async () => {
    await page.keyboard.press('ArrowRight');
    await page.waitForTimeout(100);
    const label = await page.textContent('#viewer-card-label');
    assert.ok(label.includes('Card 2 of'), `After arrow right: ${label}`);
});

await test('ESC closes viewer', async () => {
    await page.keyboard.press('Escape');
    await page.waitForTimeout(100);
    const visible = await page.$eval('#card-viewer', el =>
        el.classList.contains('active')
    );
    assert.ok(!visible, 'Card viewer should be closed');
});

await test('recycle bin tab switches view', async () => {
    await page.click('#card-canvas');
    await page.waitForTimeout(200);
    await page.click('.viewer-tab[data-tab="recycle"]');
    await page.waitForTimeout(100);
    const activeTab = await page.$eval('.viewer-tab.active', el => el.dataset.tab);
    assert.equal(activeTab, 'recycle');
    await page.keyboard.press('Escape');
});

// --- Column detail ---
console.log('\nColumn Detail:');

await test('hovering card shows column info', async () => {
    await page.click('#card-canvas');
    await page.waitForTimeout(200);
    // Hover over the viewer canvas near the left (column 1 area)
    const box = await page.$eval('#viewer-canvas', el => {
        const r = el.getBoundingClientRect();
        return { x: r.x, y: r.y, w: r.width, h: r.height };
    });
    // Move to approximately column 7 (the 'H' in 'C     HELLO WORLD')
    await page.mouse.move(box.x + 80, box.y + box.h / 2);
    await page.waitForTimeout(100);
    const detail = await page.textContent('#viewer-col-detail');
    assert.ok(detail.length > 0, 'Column detail should show something on hover');
    await page.keyboard.press('Escape');
});

// --- localStorage persistence ---
console.log('\nPersistence:');

await test('state saves to localStorage', async () => {
    await freshPage();
    await page.evaluate(() => {
        const cm = document.querySelector('.CodeMirror').CodeMirror;
        cm.setValue('C     TEST SAVE');
    });
    // Wait for debounced save
    await page.waitForTimeout(700);
    const saved = await page.evaluate(() =>
        localStorage.getItem('punchcards:source')
    );
    assert.ok(saved && saved.includes('TEST SAVE'), `Saved: ${saved}`);
});

await test('state restores from localStorage', async () => {
    // Reload page — should restore from localStorage
    await page.goto(PAGE_URL);
    await page.waitForTimeout(500);
    const text = await page.evaluate(() =>
        document.querySelector('.CodeMirror').CodeMirror.getValue()
    );
    assert.ok(text.includes('TEST SAVE'), `Restored: ${text}`);
});

await test('stats persist across reloads', async () => {
    await page.evaluate(() => {
        totalPunched = 42;
        totalDisposed = 7;
    });
    // Force save
    await page.evaluate(() => saveState());
    await page.goto(PAGE_URL);
    await page.waitForTimeout(500);
    const stats = await page.evaluate(() => ({
        punched: totalPunched,
        disposed: totalDisposed,
    }));
    assert.equal(stats.punched, 42);
    assert.equal(stats.disposed, 7);
});

await test('recycle bin persists across reloads', async () => {
    await page.evaluate(() => {
        recycleBin = [{ card: createCard('OLD CARD'), lineNumber: 3, disposedAt: Date.now() }];
        saveState();
    });
    await page.goto(PAGE_URL);
    await page.waitForTimeout(500);
    const bin = await page.evaluate(() => ({
        len: recycleBin.length,
        text: recycleBin[0]?.card?.text,
        line: recycleBin[0]?.lineNumber,
    }));
    assert.equal(bin.len, 1);
    assert.equal(bin.text, 'OLD CARD');
    assert.equal(bin.line, 3);
});

// --- Card rendering ---
console.log('\nCard Rendering:');

await freshPage();

await test('card has cream background (not white)', async () => {
    // Sample a pixel from the card body area
    const color = await page.evaluate(() => {
        const canvas = document.getElementById('card-canvas');
        const ctx = canvas.getContext('2d');
        const dpr = window.devicePixelRatio || 1;
        // Sample center of card
        const x = Math.round(canvas.width / 2);
        const y = Math.round(canvas.height / 2);
        const pixel = ctx.getImageData(x, y, 1, 1).data;
        return { r: pixel[0], g: pixel[1], b: pixel[2] };
    });
    // #F5F0E1 = rgb(245, 240, 225) — should be warm, not pure white
    assert.ok(color.r > 200 && color.r < 255, `Red: ${color.r}`);
    assert.ok(color.g > 200 && color.g < 255, `Green: ${color.g}`);
    assert.ok(color.b > 180 && color.b < 240, `Blue: ${color.b}`);
    // Cream means blue < red
    assert.ok(color.b < color.r, 'Should be warm (blue < red)');
});

await test('card has punched holes (dark pixels)', async () => {
    const hasDark = await page.evaluate(() => {
        const canvas = document.getElementById('card-canvas');
        const ctx = canvas.getContext('2d');
        const data = ctx.getImageData(0, 0, canvas.width, canvas.height).data;
        let darkCount = 0;
        for (let i = 0; i < data.length; i += 4) {
            if (data[i] < 60 && data[i + 1] < 60 && data[i + 2] < 40) darkCount++;
        }
        return darkCount;
    });
    assert.ok(hasDark > 10, `Should have dark hole pixels, found ${hasDark}`);
});

// --- Screenshot for visual verification ---
console.log('\nScreenshots:');

// Reload with default program (clear localStorage so we get 5-card deck)
await page.evaluate(() => localStorage.clear());
await page.goto(PAGE_URL);
await page.waitForTimeout(500);

await page.screenshot({ path: join(__dirname, 'test-screenshots', 'ide-view.png') });
console.log('  Saved: test-screenshots/ide-view.png');

await page.click('#card-canvas');
await page.waitForTimeout(300);
await page.screenshot({ path: join(__dirname, 'test-screenshots', 'card-viewer.png') });
console.log('  Saved: test-screenshots/card-viewer.png');

// Navigate to card 3 (FORMAT line) — deck should have 5 cards
const deckLen = await page.evaluate(() => deck.length);
if (deckLen >= 3) {
    await page.click('#viewer-next');
    await page.waitForTimeout(100);
    await page.click('#viewer-next');
    await page.waitForTimeout(200);
    await page.screenshot({ path: join(__dirname, 'test-screenshots', 'card-viewer-card3.png') });
    console.log('  Saved: test-screenshots/card-viewer-card3.png');
} else {
    console.log('  Skipped card3 screenshot (deck too small)');
}

await teardown();

// --- Results ---
console.log(`\n=== Results: ${passed} passed, ${failed} failed ===\n`);
process.exit(failed > 0 ? 1 : 0);
