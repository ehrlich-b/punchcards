# Punchcards — TODO

## Milestone 1a: A Card on Screen

- [x] Project skeleton: index.html, basic layout, CodeMirror 5 from CDN
- [x] Hollerith encoding table (JS object mapping chars to 12-bit hole patterns)
- [x] Card data model: 80 cols x 12 rows, text content, encode/decode
- [x] Canvas card renderer:
  - [x] DPR-aware canvas setup (devicePixelRatio scaling, sub-pixel snapping)
  - [x] Card body: Path2D with upper-left diagonal chamfer + 3 rounded corners
  - [x] Hole grid: rectangular holes (0.055" x 0.125") at correct positions
  - [x] Print strip: Keypunch029 dot-matrix font, naturally sized, warm grey
  - [x] Color palette: cream body (#E6E3D7), dark brown holes, subtle border/shadow
  - [x] Row digits, column numbers (middle + bottom), bottom branding
  - [x] Left sidebar: EHRLICH MAINFRAME + <ehrlich.dev> vertical branding, registration notches
- [x] IDE view layout: editor left, card stack right, stats below
- [x] Column 72 guide line in editor
- [x] FORTRAN syntax highlighting mode for CodeMirror (comments, labels, keywords)

## Milestone 1b: The Keypunch Lives

- [x] Card stack model: array of cards, add/remove/dispose tracking
- [x] Wire editor to card punching: each line = one card, typing punches holes
- [x] Dirty card tracking: backspace marks card dirty, no dispose yet
- [x] Dispose on re-type or navigate away: dirty card goes to recycle bin, fresh card punched
- [x] Card stats display: cards in deck, total punched, total disposed
- [x] Punch animation (brief visual pop when hole appears)

## Milestone 1c: Card Viewer + Recycle Bin

- [x] Card viewer: full-size single card, prev/next navigation
- [x] Column hover/click detail in card viewer (show encoding)
- [x] Recycle bin: stores old cards with content-at-disposal and original line number
- [x] Recycle bin viewer: browse disposed cards, inspect any in card viewer
- [x] Recycle bin cap at 500 cards (drop oldest)

## Milestone 1d: Persistence

- [x] localStorage auto-save: source, deck, recycle bin, stats (debounced on change)
- [x] localStorage restore: rebuild full state on page load

## Milestone 2: Hello World (FORTRAN runs)

- [x] Lexer: tokenize FORTRAN IV fixed-format source (handle column rules)
- [x] Parser: build AST for WRITE, FORMAT, STOP, END, comments
- [x] FORMAT descriptor parser: Hollerith constants (nHtext), A descriptors
- [x] Statement label resolution
- [x] Continuation card handling (col 6)
- [x] Interpreter: walk AST and execute
- [x] Line printer output panel (green-bar paper aesthetic)
- [x] Run button: feed deck to interpreter, display output
- [x] Error display: syntax errors with card/column numbers
- [x] Ship "Hello World" as a loadable example program

## Milestone 3: Fibonacci (arithmetic + loops)

- [x] Expression parser: arithmetic with correct precedence (+, -, *, /, **)
- [x] INTEGER / REAL declarations + implicit typing (I-N rule)
- [x] Assignment statements
- [x] DO loops (DO label var = start, end / CONTINUE)
- [x] Logical IF (IF (expr) statement)
- [x] GOTO
- [x] FORMAT descriptors: Iw, Fw.d, wX, /
- [x] WRITE with variable lists
- [x] Type coercion (integer <-> real)
- [x] Ship Fibonacci + Factorial as loadable examples

## Milestone 4: Richer programs

- [x] Arithmetic IF (three-way branch)
- [x] READ(unit, label) with user input prompt
- [x] DIMENSION (arrays)
- [x] Relational operators (.GT., .LT., .EQ., .NE., .GE., .LE.)
- [x] Logical operators (.AND., .OR., .NOT.)
- [x] FORMAT: Ew.d, repeat counts

## Milestone 5: Mandelbrot (release candidate)

- [ ] ASCII Mandelbrot set renderer (drawn with * chars)
- [ ] Ship as loadable example program in library

## Milestone 6: Subroutines (post-release)

- [ ] SUBROUTINE / CALL / RETURN
- [ ] FUNCTION
- [ ] COMMON blocks
- [ ] DATA statement

## Stretch Goals

- [ ] Sound effects (keypunch clatter, card feed whirr)
- [ ] "Drop the deck" Easter egg (scramble cards, re-sort by sequence numbers)
- [ ] Card sorter visualization
- [ ] JCL header cards (//EXEC FORTCLG)
- [ ] Export/import card decks as files (download/upload)
- [ ] Shareable URLs with encoded programs
