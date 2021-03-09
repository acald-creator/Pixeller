# Chapter 5

## Typed Calculator app

## Returned Output of the app

```bash
Arith> succ 0
succ 0 : Nat
Arith> succ (succ 0)
succ (succ 0) : Nat
Arith> f false then true else false
"<stdin>" (line 1, column 1):
unexpected "f"
expecting "true", "false", "0", "if" or "("
Arith> iszero (pred (succ (succ0)))
false : Bool
Arith> pred (succ 0)
0 : Nat
Arith> iszero false
Type Mismatch: Bool is not Nat
Arith> if 0 then true else false
Type Mismatch: Nat is not Bool
Arith>
```