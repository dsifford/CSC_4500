## About

This repo is an extra credit assingment for `CSC4500`. The goal of this assignment is to create a very simple "compiler" for a basic grammar (defined below). The compiler should be able to validate a given file for the language using a [Pushdown Automata](https://en.wikipedia.org/wiki/Pushdown_automaton).

## Language

### Grammar

```
G = { V , T , S , P }

V = { <program> , <stmtList> , <stmt> , <assign> , <expr> , <term> , <factor>}
T = { ident ∪ {0-9,+,-,*,/,=,;} }
S = { <program> }
```

### Production Rules (EBNF)

```
program = stmtList ;
stmtList = { stmt, separator } ;
stmt = assign ;
assign = ident , "=" , expr ;
expr = term
     | expr , ( "+" | "-" ) , term ;
term = factor
     | term , ( "*" | "/" ) , factor ;
factor = intLit
       | ident ;
intLit = { digit } ;
ident = ( letter | "_" ) , { letter | digit } ;
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
       | "H" | "I" | "J" | "K" | "L" | "M" | "N"
       | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
       | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
       | "c" | "d" | "e" | "f" | "g" | "h" | "i"
       | "j" | "k" | "l" | "m" | "n" | "o" | "p"
       | "q" | "r" | "s" | "t" | "u" | "v" | "w"
       | "x" | "y" | "z" ;
separator = ";" | "\n" | "\r" | "\v" | "\f" ;
```

**Note:** `ident` and `intLit` must not be > 10 characters in length.

## Constraints

I chose to limit myself to only the Haskell standard library, rather than using the various parsing libraries that are available.
