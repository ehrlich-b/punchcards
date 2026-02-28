# Punchcards

An IBM 029 keypunch simulator rendering IBM 5081 cards with a FORTRAN IV (FORTRAN 66) interpreter targeting the System/360 Model 30.

**Live:** [punch.ehrlich.dev](https://punch.ehrlich.dev)

![IBM 029 Punch Card](card-showcase.png)

![Punchcards IDE](screenshot-ide.png)

You type FORTRAN in the editor, cards get punched in real-time on the right, and you can run the program on a simulated System/360. Output appears on a line printer at the bottom. The whole thing is a single `index.html` plus `fortran.js` for the interpreter - no frameworks, no build step.

## Run locally

```
python3 -m http.server 8029
```

Then open [http://localhost:8029](http://localhost:8029). Port 8029 for the IBM 029 keypunch.

## What works

The interpreter handles a decent subset of FORTRAN IV (FORTRAN 66): WRITE/FORMAT with Hollerith constants and quoted strings, Iw/Fw.d/Ew.d/wX format descriptors with repeat counts, INTEGER/REAL with implicit typing (I-N rule), assignments, arithmetic expressions, DO loops, logical and arithmetic IF, GOTO, relational operators (.GT. through .LE.), logical operators (.AND./.OR./.NOT.), DIMENSION arrays, and READ from the card reader.

SUBROUTINE/FUNCTION and COMMON/DATA aren't implemented yet.

There's a program library with example programs - Fibonacci, FizzBuzz, a temperature conversion table, an ASCII rocket. A stepping debugger lets you walk through execution one statement at a time.

Cards track state the way real keypunches did. Backspace marks a card dirty; navigating away disposes it to a recycle bin and punches a fresh one. Stats track cards in deck, total punched, and total disposed.
