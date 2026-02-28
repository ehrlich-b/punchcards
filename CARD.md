# Punch Card Rendering Specification

Pixel-perfect canvas rendering of an IBM 80-column Hollerith punch card, as punched by the IBM 029 keypunch.

## Physical Card Geometry (inches)

All measurements from the IBM 5081 standard card stock.

```
Card width:   7.375"  (187.325 mm)
Card height:  3.250"  ( 82.550 mm)
Aspect ratio: 2.269:1
```

### Corner Treatment

The card is NOT a simple rectangle.

- **Upper-left corner:** Diagonal chamfer (45-degree straight cut). This is the orientation notch — tells you which way the card faces. The cut removes a triangle with legs ~0.375" along the top edge and ~0.375" down the left edge. A straight line connects those two points.
- **Other three corners:** Rounded with radius ~0.125" (1/8 inch).

```
        0.375"
    ┌───╲─────────────────────────────────┐
    │    ╲                                 )  ← 0.125" radius
    │     ╲ 0.375"                         │
    │                                      │
    │                                      │
    │                                      │
    │                                      │
    (                                      )
    └──────────────────────────────────────┘
      ↑                                  ↑
      0.125" radius              0.125" radius
```

### Hole Grid

Holes are arranged in a grid of 80 columns x 12 rows.

**Row order (top to bottom):** 12, 11, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

The top three rows (12, 11, 0) are "zone rows." Rows 0-9 are "digit rows." Row 0 serves double duty.

```
Column spacing (pitch):  0.087" center-to-center
Row spacing (pitch):     0.250" center-to-center
```

### Hole Shape

**Rectangular**, not round. Slightly rounded corners on the holes from the punch die, but visually they read as rectangles. Render them as small rounded rects with a tiny radius (~0.005").

```
Hole width:   0.055"  (horizontal)
Hole height:  0.125"  (vertical)
Aspect ratio: 1:2.27  (tall and narrow)
```

The hole height is exactly half the row pitch — 0.125" hole, 0.125" gap between hole edges.

### Margins (Edge of Card to Center of Outermost Hole)

```
Left margin:   0.251"  (card left edge to center of column 1)
Right margin:  0.251"  (center of column 80 to card right edge)
Top margin:    0.250"  (card top edge to center of row 12)
Bottom margin: 0.250"  (center of row 9 to card bottom edge)
```

**Verification:** 0.251 + (79 x 0.087) + 0.251 = 0.251 + 6.873 + 0.251 = 7.375" (card width, exact).

**Verification:** 0.250 + (11 x 0.250) + 0.250 = 0.250 + 2.750 + 0.250 = 3.250" (card height, exact).

### Print Area

The keypunch prints each character in human-readable form at the top of the card, above the row 12 holes. This sits in the 0.1875" gap between the top edge and the top of the row 12 hole (0.250" to center minus 0.0625" half-height).

Characters are printed in a small monospace dot-matrix style, one per column, aligned with column centers.

## Coordinate System

Origin (0, 0) is the top-left corner of the card (before the chamfer cut). X increases rightward, Y increases downward.

### Key Positions (in inches from origin)

**Chamfer cut vertices:**
```
(0.375, 0.000)  — on top edge
(0.000, 0.375)  — on left edge
```

**Column centers (X coordinates):**
```
Column 1:  x = 0.251"
Column N:  x = 0.251 + (N-1) * 0.087"
Column 80: x = 0.251 + 79 * 0.087 = 7.124"
```

**Row centers (Y coordinates):**
```
Row 12: y = 0.250"
Row 11: y = 0.500"
Row 0:  y = 0.750"
Row 1:  y = 1.000"
Row 2:  y = 1.250"
Row 3:  y = 1.500"
Row 4:  y = 1.750"
Row 5:  y = 2.000"
Row 6:  y = 2.250"
Row 7:  y = 2.500"
Row 8:  y = 2.750"
Row 9:  y = 3.000"
```

**Hole rectangle for column C, row R:**
```
x = columnCenter(C) - 0.0275"    (half hole width)
y = rowCenter(R) - 0.0625"       (half hole height)
w = 0.055"
h = 0.125"
```

**Printed character for column C:**
```
x = columnCenter(C)
y ≈ 0.100"  (vertically centered in the print strip above row 12)
```

## Rendering Pipeline

### Step 1: Choose a Display Width

Pick a CSS pixel width for the card. The height follows from the aspect ratio.

```
displayWidth  = 740  (CSS pixels, a good default)
displayHeight = displayWidth / 2.269 = 326
```

### Step 2: Compute Scale Factor

```
scale = displayWidth / 7.375    (CSS pixels per inch)
      = 740 / 7.375
      = 100.34 px/inch
```

All inch measurements get multiplied by `scale` to become CSS pixel positions.

### Step 3: Handle Device Pixel Ratio (4K / Retina)

This is how you get pixel-perfect rendering on high-DPI screens.

```js
const dpr = window.devicePixelRatio;  // 2 on retina, 2-3 on 4K

// Set canvas backing store to native resolution
canvas.width  = displayWidth * dpr;
canvas.height = displayHeight * dpr;

// Set CSS display size
canvas.style.width  = displayWidth + 'px';
canvas.style.height = displayHeight + 'px';

// Scale drawing context so we can still use CSS-pixel coordinates
ctx.scale(dpr, dpr);
```

Now every drawing call uses CSS-pixel coordinates, but renders at native device pixels. No blur.

### Step 4: Sub-Pixel Alignment

Round all coordinates to the nearest device pixel to avoid anti-aliased fuzz:

```js
function snap(cssPixels) {
    return Math.round(cssPixels * dpr) / dpr;
}
```

Use `snap()` on all x, y, width, height values before drawing.

## Drawing Order

1. **Card body** — filled rounded rect (cream/buff), with the upper-left chamfer. Draw as a `Path2D`:
   - `moveTo(chamferX, 0)` — start after the chamfer on top edge
   - `lineTo(width - radius, 0)` — top edge to upper-right curve
   - `arcTo(...)` — upper-right rounded corner
   - `lineTo(width, height - radius)` — right edge to lower-right curve
   - `arcTo(...)` — lower-right rounded corner
   - `lineTo(radius, height)` — bottom edge to lower-left curve
   - `arcTo(...)` — lower-left rounded corner
   - `lineTo(0, chamferY)` — left edge up to chamfer
   - `lineTo(chamferX, 0)` — diagonal chamfer line
   - `closePath()`
   - Fill cream, stroke with a subtle darker border.

2. **Card shadow** — subtle drop shadow behind the card body (`ctx.shadowBlur`, `ctx.shadowOffsetY`). Set it before filling the card body, clear it after.

3. **Column numbers** — small text at top (every 10th column: 10, 20, ... 80) in light gray, positioned in the print strip area.

4. **Printed characters** — one per column along the top, in the print strip. Small monospace font. Only render for columns that have been punched. Color: dark gray or black.

5. **Holes** — for each punched position, draw a filled rounded rect at the computed (x, y, w, h). The hole color should be darker than the card — you're seeing through the card. Use a dark color (near-black or dark brown) or the background color behind the card.

6. **Row labels** — optionally, tiny "12", "11", "0", "1"..."9" labels on the left margin.

## Color Palette

```
Card body fill:    #F5F0E1  (warm cream/buff)
Card border:       #C4B89A  (slightly darker cream)
Card shadow:       rgba(0, 0, 0, 0.15)
Hole fill:         #2C2416  (dark brown, "seeing through")
Printed text:      #333333  (dark gray)
Column numbers:    #AAAAAA  (light gray)
Row labels:        #AAAAAA  (light gray)
```

## Reference Dimensions at 740px Display Width

For quick implementation. All values in CSS pixels (multiply by DPR for canvas pixels).

```
Card:          740.0 x 326.2
Scale:         100.34 px/inch

Chamfer legs:  37.6 px each (along top and left edges)
Corner radius: 12.5 px

Hole size:     5.5 x 12.5 px
Col pitch:     8.7 px center-to-center
Row pitch:     25.1 px center-to-center

Col 1 center:  25.2 px from left
Col 80 center: 714.8 px from left
Row 12 center: 25.1 px from top
Row 9 center:  301.0 px from top

Print strip:   top 18.8 px of card (above row 12 holes)
```

## Interaction: Column Detail on Hover/Click

In the card viewer, when the user hovers or clicks a column:

1. Highlight the column (subtle vertical stripe).
2. Show the column number, the character, and the encoding:
   ```
   Col 7: 'W'  →  rows 0, 6
   ```
3. The encoding lists which rows have holes punched (zone + digit notation).
