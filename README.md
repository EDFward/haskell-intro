# Introduction to Haskell

Learn Haskell by [CIS 194: Introduction to Haskell (Spring 2015)](https://www.seas.upenn.edu/~cis194/) from UPenn. If the URL doesn't work, try [this one](https://www.seas.upenn.edu/~cis194/spring15/).

Following are my notes for each week's lectures and homeworks.

1. [Introduction to Haskell](https://github.com/EDFward/haskell-intro#week-1-introduction-to-haskell)
1. [Polymorphism and Functional Programming](https://github.com/EDFward/haskell-intro#week-2-polymorphism-and-functional-programming)
1. [Algebraic Data Types](https://github.com/EDFward/haskell-intro#week-3-algebraic-data-types)
1. [Type Classes](https://github.com/EDFward/haskell-intro#week-4-type-classes)
1. [IO](https://github.com/EDFward/haskell-intro#week-5-io)
1. [Lazy Evaluation](https://github.com/EDFward/haskell-intro#week-6-lazy-evaluation)
1. [Monads](https://github.com/EDFward/haskell-intro#week-7-monads)
1. [Monads II](https://github.com/EDFward/haskell-intro#week-8-monads-ii)
1. Testing *(omitted)*
1. [GADT](https://github.com/EDFward/haskell-intro#gadt)
1. Unsafe Haskell *(omitted)*
1. [Making Haskell Projects](https://github.com/EDFward/haskell-intro#week-13-making-haskell-projects)

### Week 1 Introduction to Haskell

This lecture mainly focuses on high-level introduction on Haskell's features and helps familiarize students with the syntax. The homework serves the same purpose so it's quite straightforward.

One of the good aspects of writing Haskell programs is that once you make the compiler happy, 90% of the time your implementation would be correct (at lease for simple programs :).

Something worth noting during my implementation of the homework:

- [Haskell function composition (.) and function application ($) idioms: correct use](http://stackoverflow.com/questions/3030675/haskell-function-composition-and-function-application-idioms-correct-us) from Stack Overflow

### Week 2 Polymorphism and Functional Programming

Additional syntax is introduced, including variable binding by `where` and `let`, functions like `map` and `filter`, currying & partial application and point-free style. But more importantly, it mentions *parametric polymorphism* and *type variables*, which for now are easy and straightforward to understand, but will cause headache in later sections...

Other small notes:

- Avoid partial functions like `head` or `tail` since they may crash (or worse, recurse infinitely) on some inputs
- Sometimes explicit recursion patterns could easily be replaced with functions like `zipWith`, `zip`, `concatMap` and so on
- [Does Haskell have tail-recursive optimization?](http://stackoverflow.com/questions/13042353/does-haskell-have-tail-recursive-optimization) from Stack Overflow
- [foldl is tail recursive, so how come foldr runs faster than foldl?](http://stackoverflow.com/questions/3429634/foldl-is-tail-recursive-so-how-come-foldr-runs-faster-than-foldl) from Stack Overflow - for details, check notes for week 6

### Week 3 Algebraic Data Types

This lecture mainly talks about how to define custom data types and use them by
pattern matching, which in my understanding is way better than check type with 
`isInstanceOf`.

The best part is on *polymorphic data types*, where `Maybe` is given as an example.

```haskell
data Maybe a = Just a
             | Nothing
```

> `Maybe` is a type constructor or *parameterized type*. To become a proper, full-blooded type, we must supply `Maybe` with another type, like `LogMessage` or `Int`...
> 
>...it becomes useful to talk about the *type of a type*. This is called a kind. Any well-formed type in Haskell such as an `Int` or a `Bool` has kind `*`. A type constructor such as `Maybe` that takes a single type parameter has kind `* -> *`. The type `Maybe Int` has kind `*` because it does not need any more type parameters in order to be a well-formed type. Everything in a type annotation must have kind `*`. For example, `Int -> Maybe` is not a valid type for a function.

The homework in this week is interesting - it's to interpret a small imperative C-like language (...again!) given an AST. The boilerplate already has type definitions for *expressions* and *statements*, what I need to do is 

1. Define an environment (state) which binds variable names and their values;
2. Desugar statements, such as transforming for-loop to while-loops;
3. Implement evaluation functions on expressions and statements.

Overall it's still a small assignment (less than 70 LOC in my implementation) but touches some fundamental aspects on programming languages, and also demonstrates the power of Haskell's algebraic data types (in my undergraduate compiler course I used C++ to write a slightly more powerful language like this one and the code serving the same purpose as in this homework has ~900 LOC - I know they are incomparable but this at least could tell something).

### Week 4 Type Classes

Now this is something new. My intuition on type classes is that they're like interfaces, but the lecture says type class could be more general and flexible. I still do not have a clear picture on *multi-parameter type classes* and *functional dependency* (in the `Extract` example) though.

And then ... well well well, **Monoid**, and **Functor**.

```haskell
class Monoid m where
  mempty  :: m
  mappend :: m -> m -> m

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- Easiest monoid instance:
instance Monoid [a] where
  mempty  = []
  mappend = (++)
```

> Monoids are useful whenever an operation has to combine results, but there may be, in general, multiple different types of results and multiple different ways of combining the results.

For me, monoid could serve as an container, where I can define an instance of monoid with custom `mempty` and `mappend`, then suddenly I could use all available functions out there for monoids, which provide higher-level functionalities based on my definition of how to *combine* results.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
```

> Note that the type argument to Functor is not quite a type: it's a type constructor. (Or, equivalently, f has kind `* -> *`.)
> ...You can think of functors as being containers, where it is possible to twiddle the contained bits. The `fmap` operation allows you access to the contained bits, *without* affecting the container... For example, a binary tree might have a Functor instance. You can `fmap` to change the data in the tree, but the tree shape itself would stay the same.

Examples:

```haskell
-- (<$>) is an infix synonym for fmap.
show <$> Nothing -- Nothing
show <$> Just 3  -- Just "3"

(*2) <$> [1,2,3] -- [2, 4, 6]
```

******
The homework is to implement a polynomial type as an instance of `Num`, `Eq` and `Show`, thus it's necessary to define functions like `(==)`, `(+)` and `show` for the new type. Since polynomials have a list of coefficients, **type class constraints** on `newtype Poly a = P [a]` are very important, which is somehow not covered much in the lectures. After finishing it the code seems very easy, but before that I did spend a lot of time fighting with the compiler - sometimes it's about type inference and sometimes the errors are even unintelligible.

Small notes:
```haskell
instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P coefs) = P $ zipWith (*) (drop 1 coefs) [1..]
```

The constraint of `Enum a` is necessary.

> `[0..]` is syntactic sugar for `enumFrom 0`, defined in class `Enum`. Because you want to generate a list of `a`s with `[0..]` the compiler demands `a` to be in class `Enum`.

from [this Stack Overflow answer](http://stackoverflow.com/questions/1533585/why-does-haskell-interpret-my-num-type-as-an-enum).

### Week 5 IO

As the name suggests, this week is about I/O, in other words, Haskell with side effects. Also introduced are *record syntax*, `ByteStrings`, and `IO` as a `Functor`, all illustrated by following 3 lines :)

```haskell
data D = C { field1 :: T1, field2 :: T2, field3 :: T3 } -- 1
import qualified Data.ByteString as BS                  -- 2
getWords' = BS.split 32 <$> BS.getLine                  -- 3
```

On the other hand, the assignment requires substantially more efforts than previous. I think it's because this time we are really using Haskell for real world (-ish) jobs - read & process JSON file, use set / map and implement not-so-trivial algorithms. I spend roughly 3 hours on `undoTs` function and in the end it's quite rewarding to see it works with reasonable readability and modularization (unabashed).

### Week 6 Lazy Evaluation

Being lazy is not so unfamiliar to me :) However, in Haskell there is a very important rule for lazy evaluation: **Pattern matching drives evaluation**.

> The slogan to remember is *"pattern matching drives evaluation"*. To reiterate the important points:
1. Expressions are only evaluated when pattern-matched
2. ...only as far as necessary for the match to proceed, and no farther!

Strictness is also introduced, since it will force a value to be evaluated thus eliminating additional thunks. Both `seq` function or *Bang Patterns* could be used to achieve this.

References, worth reading:

- [HaskellWiki: Stack overflow](https://wiki.haskell.org/Stack_overflow). It talks about *Weak Head Normal Form* (WHNF) in pattern matching driven evaluation, and newbie stack overflowing code (like `foldr` and `foldr` for summing up), more importantly, the takeaway:

> A function strict* in its second argument will always require linear stack space with foldr, so foldl' should be used instead in that case. If the function is lazy/non-strict in its second argument we should use foldr to 1) support infinite lists and 2) to allow a streaming use of the input list where only part of it needs to be in memory at a time.

- [HaskellWiki: Fold](https://wiki.haskell.org/Fold) and [HaskellWiki: Foldr Foldl Foldl'](https://wiki.haskell.org/Foldr_Foldl_Foldl'). The *List folds as structural transformations* section in the first article is interesting.

Another topic is about profiling, and the usual workflow would be
```bash
$ ghc HW06.hs -rtsopts -fforce-recomp -main-is HW06
# Generate heap profile.
$ ./HW06 +RTS -s -h -i0.001
$ hp2ps -c HW06.hp
# Or show memory / time usage directly in terminal.
$ ./HW06 +RTS -s
```

The homework involves working with infinite lists (call them *Streams*). Most of them are like brain teasers, thus are funny to solve. But it also demonstrates the significance of strict evaluation. Following are two code snippets, in which we can see the huge difference in terms of performance.

```haskell
{-# LANGUAGE BangPatterns #-}

{- Total Memory in use 223 MB, in 0.627s -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

{- Total Memory in use 1 MB, in 0.093s -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = foldl' compare' Nothing
  where
    compare' Nothing v = Just (v, v)
    -- Use bang to force strict evaluation.
    compare' (Just (!prevMin, !prevMax)) v = Just(min v prevMin, max v prevMax)

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532
```

### Week 7 Monads

> Monads are just monoids on the category of endofunctors!

Finally I'm here, confronting the most notorious concept in FP world. 

```haskell
class Monad m where
  return :: a -> m a

   -- pronounced "bind"
  (>>=) :: m a -> (a -> m b) -> m b

  (>>)  :: m a -> m b -> m b
  m1 >> m2 = m1 >>= \_ -> m2
```

`m a` is usually called *monadic values* or *computations*, or *actions*, while the type constructor `m` is a monad. For example,

- `c1 :: Maybe a` is a computation which might fail but results in an `a` if it succeeds.
- `c2 :: [a]` is a computation which results in (multiple) `a`s.
- `c3 :: Rand StdGen a` is a computation which may use pseudo-randomness and produces an `a`.
- `c4 :: IO a` is a computation which potentially has some I/O effects and then produces an `a`.

And *bind* `(>>=)` is actually a function which,

> ...will *choose* the next computation to run based on the result(s) of first computation.

The lecture gives examples on some common monads like `[]` and `Maybe`, explains `do` block as syntactic sugar for binds (in which we can write imperative-ish code), introduces useful monad combinators like `sequence` as well as list comprehensions, which could be extended to any monad if using GHC language extension `MonadComprehensions`.

The homework is like a set of monad puzzles - choose the right combinator and everything would be fine; otherwise the code would be long and incomprehensible. It used `Data.Vector` and `Control.Monad.Random`, both are typical monads. 

Anyway, I think the code is worth reading, especially when later I feel hazy about the concepts (which I guess would happen really, really soon... :)

### Week 8 Monads II

Recall `fmap` of Functors:

```haskell
fmap :: (a -> b) -> (f a -> f b)
```

> (It) transforms a "normal" function (`g :: a -> b`) into one which operates over containers/contexts `(fmap g :: f a -> f b)`. This transformation is often referred to as a *lift*; `fmap` "lifts" a function from the "normal world" into the "f world".

Excerpted from [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia).

However if the function we want to *lift* is itself inside the context, `fmap` can no longer help us do the lifting, so `Applicative` class is introduced.

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

For example,

```haskell
Just (+3) <*> Just 9  -- = Just 12
pure (+) <*> Just 3 <*> Just 9  -- = Just 12
-- Or, equivalently,
(+) <$> Just 3 <*> Just 9
```

Notice how `Applicative` helps the chaining pattern `pure f <*> x <*> y <*> ...`.

For lists,

```haskell
(+) <$> [1,2,3] <*> [4,5,6]  -- = [5,6,7,6,7,8,7,8,9]
```

> (Lists) can be viewed as a computation that can't decide on which result it wants to have, so it presents us with all of the possible results... you can think of it as adding together two non-deterministic computations with +, only to produce another non-deterministic computation that's even less sure about its result.

Excerpted from [Applicative Functors from Learn You a Haskell](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors). Both this and the former excepted passage are well worth reading.

### <a name="gadt"></a>Week 10 & 11 GADTs

Lecture materials in those two weeks are about some advanced topics in Haskell, like type family, theorem proving, and most importantly, Generalized Algebraic Data Type (GADT).

[Haskell/GADT from wikibooks](https://en.wikibooks.org/wiki/Haskell/GADT) has a great page on it. And `ArithmeticExp.hs` in folder `week11` best illustrates the necessity of GADT, inspired by that article. The easies example is like this:

```haskell
data Expr = I Int
          | B Bool           -- boolean constants
          | Add Expr Expr
          | Mul Expr Expr
          | Eq  Expr Expr    -- equality test
```

In simply typed expressions,  `Add` constructor expects two `Expr`, but there is no way for it to know the *types* of the input expressions (`I` or `B`)! Therefore we have to do the type check manually for evaluations (such as using `Maybe (Either Int Bool)` as the evaluation result).

GADT helps restrict the exact kind of `Expr` to return.

```haskell
data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Expr Int -> Expr Int -> Expr Bool
```

Think constructor `I` like `Just` in `Maybe`, which is also a function. The difference is that `I` specified the exact type of the input and output, and will fail the type check if mismatch.

> To summarize, GADTs allows us to restrict the return types of constructors and thus enable us to take advantage of Haskell's type system.

As for the implementation of Simply Typed Lambda Calculus, I'm not sure I've understood it correctly, nevertheless the [code is here](https://github.com/EDFward/haskell-intro/blob/master/week11/Stlc.hs) and worth a look.

### Week 13 Making Haskell Projects

The final week is on some minutiae about building Haskell projects using Cabal, which also includes adding test suites and documentation (Haddock). Will check out later.




