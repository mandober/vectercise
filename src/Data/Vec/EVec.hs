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

module Data.Vec.EVec where
-- :load src/Data/Vec/EVec.hs

-- imports
import Data.Kind

-- imports: mine
import Data.Vec.Nat
import Data.Vec.Vec


-- ----------------------------------------------------------------------------
-- Vectors with existential length
-- ----------------------------------------------------------------------------
-- | EVec type wrapps a 'Vec n a' hiding its length. EVec is used whenever we have a function where the resulting vector's length is unknown (but exists). However, how do we convert EVec back to Vec, i.e. how do we recover the length?
type EVec :: Type -> Type
data EVec a where
    ExVec :: forall (n :: Nat) a. Vec n a -> EVec a

deriving stock instance Show a => Show (EVec a)

-- ----------------------------------------------------------------------------
-- some existential vecors: seems they've lost their length info
e0 :: EVec a
e0 = ExVec Nil

e1 :: EVec Char
e1 = ExVec (Cons 'a' Nil) -- ExVec ⟦'a'⟧
