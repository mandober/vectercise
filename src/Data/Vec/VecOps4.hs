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

module Data.Vec.VecOps4 where
-- :load src/Data/Vec/VecOps4.hs

-- imports:
import Data.Kind
import Prelude hiding
    ( filter
    )

-- imports: mine
import Data.Vec.EVec
import Data.Vec.Nat
import Data.Vec.SNat
import Data.Vec.Vec

-- ----------------------------------------------------------------------------
-- Vec functions, Part 4: Existentials
-- ----------------------------------------------------------------------------
{-
These ops return a vector whose length is an unknown, but existing, Nat.
-}

-- ----------------------------------------------------------------------------
-- Vec ops: List to Vec
-- ----------------------------------------------------------------------------
-- | Converting alist to Vec produces an EVec
listToVec :: [a] -> EVec a
listToVec []     = ExVec Nil
listToVec [x]    = ExVec (Cons x Nil)
listToVec (x:xs) = case listToVec xs of
    ExVec w -> ExVec (Cons x w)


l2v :: Num a => EVec a
l2v = listToVec [1,2,5,3,6]

-- ----------------------------------------------------------------------------
-- Vec ops: filter
-- ----------------------------------------------------------------------------
-- | filter returns the existentially wrapped vector as EVec
filter :: (a -> Bool) -> Vec n a -> EVec a
filter _ Nil = ExVec Nil
filter p (Cons x xs)
    | p x = case filter p xs of
        ExVec ys -> ExVec (Cons x ys)
    | otherwise = filter p xs
