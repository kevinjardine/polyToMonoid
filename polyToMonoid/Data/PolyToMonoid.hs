{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}
{- |
   Module     : Data.PolyToMonoid
   Copyright  : Copyright (C) 2010 Kevin Jardine
   License    : BSD3

   Maintainer : Kevin Jardine <kevinjardine@yahoo.com>
   Stability  : provisional
   Portability: non-portable (depends on GHC extensions)
   
Creates polyvariadic functions that map their parameters into a given monoid.

-}

module Data.PolyToMonoid (
    -- * Introduction
    -- $introduction
    
    -- * Concept
    -- $usage
    
    -- * Examples
    -- $examples
    
    -- * Extensions
    -- $extensions

    Monoidable(toMonoid),
    PolyVariadic(ptm),
    CPolyVariadic(ctm),
    Terminate(..)
) where

import Data.Monoid
{- $introduction
Haskell lists contain an indefinite number of elements of a single type. 
It is sometimes useful to create list-like functions that accept an indefinite
number of parameters of multiple types.

PolyToMonoid provides two such functions, 'ptm' and 'ctm', as well as a typeclass 'Monoidable'. 
The only precondition is that the
parameters of 'ptm' and 'ctm' can be mapped to an underlying monoid using 
the 'toMonoid' function provided by 'Monoidable'.
-}
{- $usage
To understand how the polyToMonoid functions work, consider a function @list@ that maps its parameters to 
a list:

> list p1 p2 ... pN =
>    [p1] ++ [p2] ++ ... ++ [pN]
    
(which is the same as @[p1,p2, ..., pN]@ )
    
@list@ can be generalised to any monoid conceptually as:

> polyToMonoid p1 p2 ... pN =
>    (toMonoid p1) `mappend` (toMonoid p2) `mappend` ... `mappend` (toMonoid pN)
    
Remember that a monoid, defined in "Data.Monoid", is any set of elements 
with an identity element @mempty@ and an associative operator @mappend@ .

As any list type is automatically a monoid with @mempty = []@ and @mappend = (++)@, 
the @list@ function defined above is just a specific version of @polyToMonoid@.

The main difficulty with defining a @polyToMonoid@ function is communicating to Haskell what
underlying monoid to use.

Through Haskell type magic, this can be done with a simple type annotation.

Specifically, you can pass @mempty@ as the first element of the function and annotate it with the type of the 
monoid it belongs to.

This library provides two variants of a polyToMonoid function. The first, 'ptm',
simply takes a list of arguments starting with mempty and returns the monoid result.

The second variant, 'ctm', is composible. In effect, it returns a function that consumes the next parameter.
You can feed the 'ctm' result to a second termination function 'trm' to get the actual result.

-}
{- $examples

We can first tell Haskell how to map a set of types to a list of strings using 'Monoidable':

> instance Show a => Monoidable a [String] where
>    toMonoid a = [show a]
    
and then tell 'ptm' to use the @[String]@ monoid:

> ptm (mempty :: [String]) True "alpha" [(5 :: Int)]

The result returned would be:

> ["True","\"alpha]\"","[5]"]

In this case, 'Monoidable' tells the 'ptm' function to accept a wide variety of types 
(anything with a @show@ function) when using the @[String]@ monoid.

The first parameter of 'ptm', @(mempty :: [String])@ tells it to map its parameters into the
@[String]@ monoid. Think of the @mempty@ value as like the initial value of a fold.

Unlike 'ptm', the 'ctm' function in effect returns a partial function ready to consume
the next parameter rather than a monoid result.

It is therefore more composable, at the cost of requiring a second termination function 'trm'
to return the actual monoid result.

> ctmfirstbit = ctm mempty :: [String]) True "alpha"
> ctmsecondbit = ctmfirstbit [(5 :: Int)]
> finalresult = trm ctmsecondbit

The result returned would be the same as above:

> ["True","\"alpha]\"","[5]"]

Monoids, of course, do not have to be lists.

Here's a second example which multiplies together numbers of several types:

> instance Monoid Double where
>     mappend = (*)
>     mempty = (1.0) :: Double

> instance Monoidable Int Double where
>     toMonoid = fromIntegral

> instance Monoidable Double Double where
>     toMonoid = id
   
> ptm (mempty :: Double) (5 :: Int) (2.3 :: Double) (3 :: Int)

In this case, 'ptm' accepts parameters that are either ints or doubles, converts them to doubles, 
and then multiplies them together.

You can use the composibility of 'ctm' to define a @productOf@ function:

> productOf = ctm (mempty :: Double)
> trm $ productOf (5 :: Int) (2.3 :: Double) (3 :: Int)

As before the 'trm' function is required to terminate the 'ctm' calculation and deliver the final result.
     
-}

{- $extensions
You will probably need to enable the following extensions to use this library:

> TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses

-}

-- | Define instances of Monoidable to tell Haskell
-- how to convert your parameters into values in the underlying 
-- monoid
class Monoid m => Monoidable a m where
    toMonoid :: a -> m
       
squish :: Monoidable a m => m -> a -> m  
squish m a = (m `mappend` (toMonoid a))

{-|
Conceptually, ptm is defined as:

>    ptm (mempty :: MyMonoid) p1 p2 ... pN =
>        (toMonoid p1) `mappend` (toMonoid p2) `mappend` ... `mappend` (toMonoid pN)

-}
class Monoid m => PolyVariadic m r where
    ptm :: m -> r

{-|
ctm is a composable variant of ptm.

To actually get its value, use the terminator function trm.
-}   
class Monoid m => CPolyVariadic m r where
    ctm :: m -> r
    
data Terminate m =
    Terminate {
        trm :: m -- ^ Use the terminator function trm to get the value of a ctm calculation.
    }

instance (Monoid m', m' ~ m) => CPolyVariadic m (Terminate m') where
    ctm acc = Terminate acc
    
instance (Monoidable a m, CPolyVariadic m r) => CPolyVariadic m (a->r) where
    -- ctm m a = \b -> m `mappend` (ctm (toMonoid a) b)
    ctm acc = \a -> ctm (squish acc a)
    
instance (Monoid m', m' ~ m) => PolyVariadic m m' where
    ptm acc = acc

instance (Monoidable a m, PolyVariadic m r) => PolyVariadic m (a->r) where
    ptm acc = \a -> ptm (squish acc a)