{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}

{-# OPTIONS_GHC -Wall                               #-}
{-# OPTIONS_GHC -Wno-unused-imports                 #-}
{-# OPTIONS_GHC -Wno-unused-top-binds               #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Data.Vec.VecOps7 where
-- :load src/Data/Vec/VecOps7.hs

-- imports:
import Data.Kind
import Unsafe.Coerce
import Prelude hiding
    ( (!!)
    )

-- imports: mine
import Data.Vec.Nat
import Data.Vec.SNat
import Data.Vec.FNat
import Data.Vec.NatOps
import Data.Vec.EVec
import Data.Vec.Vec

-- ----------------------------------------------------------------------------
-- Vec functions, Part 7: custom GADTs to encode specific conditions
-- ----------------------------------------------------------------------------


-- ----------------------------------------------------------------------------
-- (!!)
-- ----------------------------------------------------------------------------
(!!) :: Vec n a -> FNat n -> a
(Cons x _)  !! FZ = x
(Cons _ xs) !! (FS f) = xs !! f

x1,x2,x3,x4,x5 :: Int
x1 = v5 !! FZ -- 2
x2 = v5 !! FS FZ -- 1
x3 = v5 !! FS (FS FZ) -- 4
x4 = v5 !! FS (FS (FS FZ)) -- 2
x5 = v5 !! FS (FS (FS (FS FZ))) -- 5

-- x6 :: Int
-- x6 = v5 !! FS (FS (FS (FS (FS FZ))))

-- ----------------------------------------------------------------------------
-- span
-- ----------------------------------------------------------------------------
{-
span :: (a -> Bool) -> [a] -> ([a], [a])

span is about the prefix (not later elements)

span (<4)   [1..5] == ([1,2,3], [4,5]       )
span (>2)   [1..5] == ([]     , [1,2,3,4,5] )
span (== 4) [1..5] == ([]     , [1,2,3,4,5] )
span (== 1) [1..5] == ([1]    , [2,3,4,5]   )

span :: (a -> Bool) -> Vec (p + q) a -> (Vec p a, Vec q a)

-}
