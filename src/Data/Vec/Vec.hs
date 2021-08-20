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
{-# OPTIONS_GHC -Wno-type-defaults                  #-}
{-# OPTIONS_GHC -Wno-unused-top-binds               #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Data.Vec.Vec where
-- :load src/Data/Vec/Vec.hs

-- imports
import Data.Kind
import Data.List (intercalate)

-- imports: mine
import Data.Vec.Nat

-- ----------------------------------------------------------------------------
-- Vec
-- ----------------------------------------------------------------------------
-- | Vec is a list with its lenght tracked in the type
type Vec :: Nat -> Type -> Type
data Vec n a where
    Nil  :: Vec 'Z a
    Cons :: a -> Vec n a -> Vec ('S n) a

(↑) :: a -> Vec n a -> Vec ('S n) a
(↑) = Cons
infixr 5 ↑

deriving stock instance Eq   a => Eq   (Vec n a)
deriving stock instance Ord  a => Ord  (Vec n a)

-- ----------------------------------------------------------------------------
-- Vec: Show
-- ----------------------------------------------------------------------------
-- deriving stock instance Show a => Show (Vec n a)

instance Show a => Show (Vec n a) where
    show = showVec

showVec :: Show a => Vec n a -> String
showVec Nil = "⟦⟧"
showVec xs = "⟦" <> intercalate ", " (map show (vecToList xs)) <> "⟧"

printVec :: (Show a) => Vec n a -> IO ()
printVec = putStrLn . showVec

-- ----------------------------------------------------------------------------
-- Vec ~~> List
-- ----------------------------------------------------------------------------
vecToList :: Vec n a -> [a]
vecToList Nil = []
vecToList (Cons x xs) = x : vecToList xs

-- ----------------------------------------------------------------------------

v0 :: Vec Z Int
v0 = Nil

v2 :: Vec II Int
v2 = 6 ↑ 7 ↑ Nil

v3 :: Vec III Int
v3 = 1 ↑ 2 ↑ 5 ↑ Nil

v5 :: Vec V Int
v5 = 2 ↑ 1 ↑ 4 ↑ 2 ↑ 5 ↑ Nil
