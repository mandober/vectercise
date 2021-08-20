{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Vec.Nat where
-- :load src/Data/Vec/Nat.hs

-- imports
import Data.Kind

-- ----------------------------------------------------------------------------
-- | Peano-style natural numbers to be promoted to Nat kind
data Nat = Z | S Nat deriving (Show, Read, Eq, Ord)

-- ----------------------------------------------------------------------------
-- | Term-level Nat shorthands
p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 :: Nat
p1  =                            S Z
p2  =                         S (S Z)
p3  =                      S (S (S Z))
p4  =                   S (S (S (S Z)))
p5  =                S (S (S (S (S Z))))
p6  =             S (S (S (S (S (S Z)))))
p7  =          S (S (S (S (S (S (S Z))))))
p8  =       S (S (S (S (S (S (S (S Z)))))))
p9  =    S (S (S (S (S (S (S (S (S Z))))))))
p10 = S (S (S (S (S (S (S (S (S (S Z)))))))))

-- | Type-level Nat shorthands
type I    =                            S Z
type II   =                         S (S Z)
type III  =                      S (S (S Z))
type IV   =                   S (S (S (S Z)))
type V    =                S (S (S (S (S Z))))
type VI   =             S (S (S (S (S (S Z)))))
type VII  =          S (S (S (S (S (S (S Z))))))
type VIII =       S (S (S (S (S (S (S (S Z)))))))
type IX   =    S (S (S (S (S (S (S (S (S Z))))))))
type X    = S (S (S (S (S (S (S (S (S (S Z)))))))))
