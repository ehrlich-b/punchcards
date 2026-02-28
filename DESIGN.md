# Punchcards — Design Document

## Vision

A browser-based punch card simulator that makes you *feel* like you're programming an IBM System/360 in 1966. You type FORTRAN IV in an editor. As you type, punch cards are physically punched — holes appear, the keypunch chatters. You can inspect individual cards, see the Hollerith encoding, watch your deck grow. Then you run it on the simulated mainframe and get line-printer output.

It should be fun before it's useful.

## Target Machine

**IBM System/360 Model 30** (1965) — the most widely deployed System/360. Input via IBM 2540 card reader, output via IBM 1403 line printer. Programming in FORTRAN IV (FORTRAN 66).

**IBM 029 Keypunch** — the companion card punch machine. Our card encoding follows its 64-character EBCD character set.

## UI Layout

Two views, toggled:

### 1. IDE View (default)
```
+------------------------------------------+
|  [IDE]  [Card Viewer]     Cards: 12  [R] |
+------------------------------------------+
|                    |                      |
|   Code Editor      |   Card Stack        |
|   (CodeMirror 5)   |   (visual deck,     |
|                    |    top card visible) |
|   - line numbers   |                      |
|   - col 72 marker  |   Stats:            |
|   - FORTRAN mode   |   - Cards punched   |
|                    |   - Cards disposed  |
|                    |   - Deck size       |
+------------------------------------------+
|  Output (line printer)                   |
|  > HELLO WORLD!                          |
|  > READY                                 |
+------------------------------------------+
```

- **Left panel:** CodeMirror 5 editor with FORTRAN syntax highlighting, line numbers, column 72 guide line.
- **Right panel:** Visual card stack showing the top card of the deck. Card count and stats below.
- **Bottom panel:** Line printer output. Monospace, green-bar paper aesthetic.
- **[R] button:** Run program (feed deck to System/360).

### 2. Card Viewer
```
+------------------------------------------+
|  [IDE]  [Card Viewer]     Card 7 of 12   |
+------------------------------------------+
|                                          |
|  ┌────────────────────────────────┐      |
|  │ HELLO WORLD PROGRAM           │      |
|  │ ○○ ○○○  ○○○ ○○ ○○  ...       │      |
|  │ ○  ○ ○  ○ ○ ○○ ○   ...       │      |
|  │    ○    ○   ○  ○    ...       │      |
|  │ ...                           │      |
|  └────────────────────────────────┘      |
|                                          |
|  < Prev                      Next >      |
|                                          |
|  Column detail:                          |
|  Col 7: 'W' = rows 6,6 (zone 0 + 6)    |
+------------------------------------------+
```

- Full-size card rendering on canvas.
- Human-readable text printed along top edge (like a real card).
- Hole pattern visible below.
- Navigate between cards with prev/next.
- Hover/click a column to see its encoding detail.

## Card Punching Mechanics

The core fun mechanic. As you type in the editor:

1. **Each line of code = one punch card.** Line 1 = card 1, etc.
2. **Characters are punched in real-time.** Type a character → a hole (or holes) appear on the current card.
3. **Backspace marks the card as dirty, but does NOT immediately dispose it.** You can backspace freely — the card is still the same physical card, you're just acknowledging a mistake. The card is disposed when:
   - You start typing new characters on that line (you've grabbed a fresh card).
   - You navigate away from the line (cursor moves to a different line).
   In both cases: the old card goes to the recycle bin and a new card is punched with the corrected content. This means "hello" + 5 backspaces = 1 disposed card, not 5.
4. **Adding/removing lines** adds/removes cards from the deck.
5. **Card stats** are always visible: cards in deck, total punched, total disposed.

The card rendering should animate briefly when a hole is punched — a quick visual pop.

## Recycle Bin

Disposed cards go to a recycle bin — a rolling buffer of the last 500 disposed cards.

- Each card retains its **content at time of disposal** and its **original line number**.
- The UI has a "Recycle Bin" section/view where you can browse disposed cards.
- Cards shown with their original line number label (e.g., "Line 3 (disposed)").
- Ordered by disposal time (most recent on top).
- Oldest cards roll off when the bin hits 500.
- You can inspect any recycled card in the card viewer just like an active card.

## Hollerith Encoding (IBM 029)

Each column encodes one character as a pattern of holes in 12 rows:

**Digits:** Single punch in the corresponding row (0-9).

**Letters:**
- A-I: Row 12 + rows 1-9
- J-R: Row 11 + rows 1-9
- S-Z: Row 0 + rows 2-9

**Special characters:** Combinations using row 8 as a modifier. See CLAUDE.md or the encoding table in the source.

Space = no holes (blank column).

## FORTRAN IV Implementation — Phased

### Phase 1: Hello World
The minimum to run `WRITE(6,10)` / `FORMAT(12HHELLO WORLD!)` / `STOP` / `END`.

Statements:
- `WRITE(unit, label)` — output to line printer (unit 6)
- `FORMAT(...)` — Hollerith constants (`nHtext`), quoted strings
- `STOP` — halt execution
- `END` — end of program unit
- `C` in column 1 — comment lines (ignored)

Parsing:
- Fixed-format card layout (cols 1-5 label, col 6 continuation, cols 7-72 code)
- Statement label resolution
- Hollerith constant parsing (`nH` followed by exactly n characters)
- FORMAT descriptor parsing (just `nHtext` and `A` descriptors for Phase 1)

### Phase 2: Arithmetic + Loops (Fibonacci, Factorial)
- `INTEGER` / `REAL` declarations
- Assignment statements with arithmetic expressions (`+`, `-`, `*`, `/`)
- `DO label var = start, end` / `CONTINUE`
- Logical `IF (expr) statement`
- `GOTO label`
- FORMAT descriptors: `Iw` (integer), `Fw.d` (real), `wX` (spaces), `/` (newline)
- Implicit typing (I-N = INTEGER, else REAL)
- Integer and real arithmetic, type coercion

### Phase 3: Branches + I/O
- Arithmetic `IF (expr) label1, label2, label3` (three-way branch)
- `READ(unit, label)` — input from card reader (unit 5, prompt user)
- `DIMENSION` — arrays
- Relational operators in logical IF (`.GT.`, `.LT.`, `.EQ.`, `.NE.`, `.GE.`, `.LE.`)
- Logical operators (`.AND.`, `.OR.`, `.NOT.`)
- FORMAT descriptors: `Ew.d` (exponential), repeat counts

### Phase 4: Subroutines
- `SUBROUTINE name(args)` / `CALL name(args)` / `RETURN`
- `FUNCTION name(args)`
- `COMMON` blocks (shared memory)
- `DATA` statement (initialization)

## Visual Design

- **Color palette:** Cream/off-white cards, dark brown/black holes, IBM blue accents.
- **Typography:** Monospace everywhere. The editor, the cards, the output.
- **Card aesthetic:** Rounded corners, slight shadow, the printed text along the top edge in that classic IBM card font.
- **Line printer output:** Green-bar paper look (alternating faint green/white stripes), monospace output.
- **Minimal chrome.** No gradients, no glass effects. Flat, clean, retro.
- **Sound (optional/stretch):** Keypunch clatter on typing, card feed whirr on run.

## Persistence (localStorage)

No backend. Everything lives in localStorage and survives page reload.

**What's saved:**
- The current editor content (source text).
- The active card deck (each card's content and hole pattern).
- The disposed card pile (content, hole pattern, original line number, disposal timestamp).
- Card stats (total punched, total disposed lifetime counters).
- Last line printer output.

**Key structure:**
- `punchcards:source` — editor text (string).
- `punchcards:deck` — active card deck (JSON array of card objects).
- `punchcards:disposed` — recycle bin (JSON array of disposed card objects).
- `punchcards:stats` — lifetime counters (JSON object: `{totalPunched, totalDisposed}`).
- `punchcards:output` — last line printer output (string).

**When to save:**
- Debounced on editor change (e.g., 500ms after last keystroke).
- Immediately on card dispose events.
- On run (save output).
- No explicit "save" button — it just works.

**On load:**
- Restore editor content from `punchcards:source`.
- Rebuild card deck and recycle bin from stored data.
- Restore stats counters.
- Show last output in the line printer panel.
- If nothing in localStorage, start with an empty editor.

**Size considerations:**
- Punch card programs are tiny. An 80-col x 200-line program is ~16KB of source.
- The recycle bin could grow large over time. Cap it at 500 cards and silently drop the oldest when exceeded.
- Total localStorage budget is ~5MB per origin, which is more than enough.

## Canvas Rendering

Cards are rendered pixel-perfect on `<canvas>`, not images. All elements are geometric primitives. See CARD.md for the full spec with exact measurements and pixel coordinates.

**DPR-aware rendering (4K/Retina):**
- Canvas backing store sized at `displayWidth * devicePixelRatio`.
- CSS dimensions set to the logical display size.
- Context scaled by DPR so drawing code uses CSS-pixel coordinates.
- All coordinates snapped to device pixels (`Math.round(val * dpr) / dpr`) to prevent anti-aliased fuzz.

**Card shape:** Not a simple rectangle. Upper-left corner has a diagonal chamfer (orientation notch). Other three corners are rounded. Drawn as a single `Path2D` with `lineTo` for the chamfer and `arcTo` for the rounds.

**Holes:** Rectangular, not circular. Tall and narrow (1:2.27 aspect ratio). Drawn as small rounded rects. The fill color is dark (you're seeing through the card).

**Drawing order:** shadow → card body fill → card border → column numbers → printed characters → holes → row labels.

## Technical Notes

- CodeMirror 5 `change` event drives the card punching logic.
- The FORTRAN interpreter is a tree-walk interpreter: tokenize → parse → AST → execute.
- No optimization needed — FORTRAN IV programs on punch cards are tiny.
- Interpreter runs synchronously (programs are short). If we add READ, we pause for user input via a modal/prompt.

## Example Programs (Ship With)

```fortran
C     HELLO WORLD
      WRITE(6,10)
   10 FORMAT(12HHELLO WORLD!)
      STOP
      END
```

```fortran
C     FIBONACCI SEQUENCE
      INTEGER IA, IB, IC
      IA = 0
      IB = 1
      WRITE(6,10) IA
      WRITE(6,10) IB
      DO 20 I = 1, 13
      IC = IA + IB
      WRITE(6,10) IC
      IA = IB
      IB = IC
   20 CONTINUE
   10 FORMAT(I10)
      STOP
      END
```

```fortran
C     FACTORIAL
      INTEGER N, IFACT
      N = 10
      IFACT = 1
      DO 10 I = 1, N
      IFACT = IFACT * I
   10 CONTINUE
      WRITE(6,20) N, IFACT
   20 FORMAT(2HN=,I3,12H  FACTORIAL=,I10)
      STOP
      END
```
