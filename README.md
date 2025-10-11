# COBCALC — Algebraic Expression Evaluator in COBOL

This project was originally inspired by an algebraic expression evaluator I saw in BYTE Magazine in the 1980s—likely written in BASIC or Pascal. I understood the logic and rewrote it in COBOL back in the late 1990s. Life took over (we were raising a family), and the code was set aside. Now, decades later, I’ve resurrected, refined, and open-sourced it as **cobcalc** —possibly the only general-purpose infix math evaluator ever written in pure COBOL. 

A fully functional infix expression parser and evaluator written in **standard COBOL**, supporting:
- Basic arithmetic (`+ - * /`)
- Exponentiation (`^`)
- Parentheses nesting
- Functions: `SQRT` (more coming!)
- Floating-point results

## Example 
(SOLVE AMORT MONTHLY PMT: INT=5% LOAN=$250000 N=30 YRS)

ENTER EXPRESSION (OR END)

(5/1200\*250000\*((1+5/1200)^(30\*12)))/(((1+5/1200)^(30\*12))-1)

ANS=      1342.05405

## Grammar
Here’s the approximate grammar that COBCALC implements:
```expr → term { ("+" | "-") term } 
  term → factor { ("*" | "/") factor } 
  factor → primary { "^" primary } 
  primary → number | "SQRT" "(" expr ")" | "(" expr ")"
```
             
## Build Instructions
Requires **GnuCOBOL 3.1+**:
Requires COBOL-85 for recursion and structured programming (not compatible with COBOL-74 without significant changes)

```sh
cobc -x cobcalc.cob
```

## License 
GNU General Public License v3.0 or later — see LICENSE  









