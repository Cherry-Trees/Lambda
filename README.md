# Lambda

## About

Lambda is a simple, interpreted functional programming language written in Haskell based on simply-typed lambda calculus.
The implementation for such functions uses named De Bruijn indices.
It was developed as an exercise for furthering my understanding of Haskell as well as learning how to implement curried functions.
As of now, it can only be run in the provided REPL environment.

## Features

- Integer and boolean primitives
- Basic arithmetic and logic operations
- Curried functions and function application
- First-class functions
- Simple control flow with if-expressions

## Examples

**Recursion and control flow:**
```
> fact n = if n == 0 then 1 else n * fact (n - 1)
> fact 5
120
```

**Functions as monads:**
```
> return x = \_ -> x
> bind h f = \w -> f (h w) w
> addStuff = bind ((*) 2) (\a -> bind ((+) 10) (\b -> return (a + b)))
> addStuff 6
28
```

**Specifying the De Bruijn index of lambda variables directly:**
```
> f x y x x = x@0
> f 1 2 3 4
4
> g x y x x = x@1
> g 1 2 3 4
3
> h x y x x = x@2
> h 1 2 3 4
1
```

## Requirements

- `ghc`
- `cabal`

## Build

**Get the code:**
```
git clone https://github.com/Cherry-Trees/Lambda
```

**Build and run:**
```
cd Lambda
cabal build
cabal run
```
