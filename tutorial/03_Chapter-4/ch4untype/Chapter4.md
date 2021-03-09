# Chapter 4

## Untyped lambda

## Returned Output of the app

```bash
Untyped> (\x.x) 1
 => \x . x
 => 1
 => x
1
Untyped> (\x y . y) 1 2
  => \x y . y
  => 1
 => \x y . y 1
 => 2
 => y
2
Untyped> (\x y z. x z (y z)) (\x y . x) (\x y . x)
  => \x y z . (x z (y z))
  => \x y . x
 => \x y z . (x z (y z)) (\x y . x)
 => \x y . x
<<closure>>
Untyped>
```