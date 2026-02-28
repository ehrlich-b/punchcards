# Punchcards — Project Instructions

**Site:** punchcards.ehrlich.dev

## What This Is

A browser-based IBM punch card simulator with a working FORTRAN IV interpreter. Visual, tactile, fun. You type code in an editor, cards get punched in real-time, and you can run the program on a simulated IBM System/360.

## Tech Stack

- **Pure HTML/CSS/JS.** No frameworks, no build step, no npm.
- **CodeMirror 5** for the text editor (CDN, single script tag).
- **Canvas** for punch card rendering.
- Everything in a single `index.html` or a small set of static files.
- Hosted as a static site at punchcards.ehrlich.dev.

## Architecture Rules

- No build tools. No bundlers. No transpilers. Drop files on a server.
- Keep JS in `<script>` tags or at most a few `.js` files loaded via `<script src>`.
- CSS inline or in a single stylesheet. No CSS frameworks.
- The FORTRAN interpreter is pure JS — no WASM, no server-side execution.
- Mobile is not a target. Desktop browser only.

## Code Style

- Vanilla JS. No classes unless they genuinely help (Card, Deck, Interpreter are fine).
- Prefer `const`/`let`, never `var`.
- No TypeScript. No JSDoc unless something is genuinely confusing.
- Match IBM/retro naming where it makes sense (e.g., `punchColumn`, `hollerithEncode`).
- Keep functions short. If a function needs a comment explaining what it does, it's too long or badly named.

## The Punch Card Format

- IBM 80-column Hollerith card (IBM 029 encoding).
- 12 rows x 80 columns per card.
- Rows top-to-bottom: 12, 11, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.
- One character per column, max 3 holes per column.
- FORTRAN column rules: col 1 = comment (C), cols 1-5 = label, col 6 = continuation, cols 7-72 = statement, cols 73-80 = sequence number.

## Persistence

- **No backend.** All state lives in localStorage under `punchcards:*` keys.
- Auto-save on editor change (debounced). Auto-restore on load.
- Recycle bin capped at 500 cards.
- See DESIGN.md for the full key structure.

## FORTRAN IV Subset

We implement a subset of FORTRAN IV (FORTRAN 66) as it ran on the IBM System/360. Target machine is the System/360 Model 30.

Implicit typing: variables I-N are INTEGER, all others are REAL.

I/O units: 5 = card reader (stdin/input), 6 = line printer (stdout/output).

See DESIGN.md for the phased feature list.
