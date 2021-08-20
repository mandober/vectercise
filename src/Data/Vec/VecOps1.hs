{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}

{-# OPTIONS_GHC -Wall                               #-}
{-# OPTIONS_GHC -Wno-unused-imports                 #-}
{-# OPTIONS_GHC -Wno-unused-top-binds               #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Data.Vec.VecOps1 where
-- :load src/Data/Vec/VecOps1.hs

-- imports:
import Data.Kind
import Prelude hiding
    ( head
    , tail
    , init
    , last
    , map
    , and
    , or
    , any
    , all
    , sum
    , product
    , maximum
    , minimum
    , foldr
    , foldl
    , foldr1
    , foldl1
    )

-- imports: mine
import Data.Vec.Vec
import Data.Vec.Nat

-- ----------------------------------------------------------------------------
-- Vec functions, Part 1: Data.List functions in terms of Vec
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- 1.1 Vec n a
-- ----------------------------------------------------------------------------
and :: Vec n Bool -> Bool
and Nil = True
and (Cons x xs) = x && and xs

or :: Vec n Bool -> Bool
or Nil = False
or (Cons x xs) = x || or xs

all :: (a -> Bool) -> Vec n a -> Bool
all _ Nil = True
all p (Cons x xs) = p x && all p xs

any :: (a -> Bool) -> Vec n a -> Bool
any _ Nil = False
any p (Cons x xs) = p x || any p xs

sum :: Num a => Vec n a -> a
sum Nil = 0
sum (Cons x xs) = x + sum xs

product :: Num a => Vec n a -> a
product Nil = 1
product (Cons x xs) = x * product xs

-- ----------------------------------------------------------------------------
-- 1.2 Vec (S n) a
-- ----------------------------------------------------------------------------
singleton :: a -> Vec (S Z) a
singleton x = Cons x Nil

map :: (a -> b) -> Vec n a -> Vec n b
map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

head :: Vec (S n) a -> a
head (Cons x _) = x

tail :: Vec (S n) a -> Vec n a
tail (Cons _ xs) = xs


-- ----------------------------------------------------------------------------
-- 1.3 'Vec (S n) a' with the pattern 'xs@(Cons _ _)'
-- ----------------------------------------------------------------------------
init :: Vec (S n) a -> Vec n a
init (Cons _ Nil) = Nil
init (Cons x xs@(Cons _ _)) = x ↑ init xs

last :: Vec (S n) a -> a
last (Cons x Nil) = x
last (Cons _ xs@(Cons _ _)) = last xs

minimum :: Ord a => Vec (S n) a -> a
minimum (Cons x Nil) = x
minimum (Cons x xs@(Cons _ _)) = x `min` minimum xs

maximum :: Ord a => Vec (S n) a -> a
maximum (Cons x Nil) = x
maximum (Cons x xs@(Cons _ _)) = x `max` maximum xs


foldl :: (b -> a -> b) -> b -> Vec n a -> b
foldl _ z Nil = z
foldl f z (Cons x Nil) = f z x
foldl f z (Cons x xs@(Cons _ _)) = foldl f (f z x) xs

foldr :: (a -> b -> b) -> b -> Vec n a -> b
foldr _ z Nil = z
foldr f z (Cons x Nil) = f x z
foldr f z (Cons x xs@(Cons _ _)) = f x (foldr f z xs)


foldl1 :: (a -> a -> a) -> Vec (S n) a -> a
foldl1 _ (Cons x Nil) = x
foldl1 f (Cons x (Cons y Nil)) = f x y
foldl1 f (Cons x (Cons y ys@(Cons _ _))) = f (f x y) (foldl1 f ys)

foldr1 :: (a -> a -> a) -> Vec (S n) a -> a
foldr1 _ (Cons x Nil) = x
foldr1 f (Cons x xs@(Cons _ _)) = f x (foldr1 f xs)


foldl' :: (b -> a -> b) -> b -> Vec n a -> b
foldl' _ z Nil = z
foldl' f z (Cons x Nil) = f z x
foldl' f z (Cons x xs@(Cons _ _)) = foldl' f ((f $! z) $! x) xs

foldr1' :: (a -> a -> a) -> Vec (S n) a -> a
foldr1' _ (Cons x Nil) = x
foldr1' f (Cons x xs@(Cons _ _)) = (f $! x) $! (foldr1' f xs)
