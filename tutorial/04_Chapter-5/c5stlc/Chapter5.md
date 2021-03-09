# Chapter 5

## Simply Typed Lambda Calculus app 

## Returned Output of the app

```bash
Happy> (\x : Int . \y : Int . y) 1 2
2
Happy> (\x : (Int -> Int). x) (\x : Int . 1) 2
1
Happy> (\x : Int . x) False
Expecting Int but got Bool
Happy> 1 2
Tried to apply to non-function type: Int
Happy> (\x : Int . (\y : Int . x))
<<closure>>
Happy> 
```