{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -Wall                               #-}
{-# OPTIONS_GHC -Wno-missing-signatures             #-}
{-# OPTIONS_GHC -Wno-unused-imports                 #-}
{-# OPTIONS_GHC -Wno-unused-top-binds               #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Data.Vec.SNat where
-- :load src/Data/Vec/SNat.hs

-- imports
import Data.Kind

-- imports: mine
import Data.Vec.Nat

-- ----------------------------------------------------------------------------
-- | Peano-style natural numbers as singletons
type SNat :: Nat -> Type
data SNat n where
    SZ :: SNat 'Z
    SS :: SNat n -> SNat ('S n)


deriving stock instance Show (SNat n)
deriving stock instance Eq   (SNat n)
deriving stock instance Ord  (SNat n)

-- ----------------------------------------------------------------------------
sn0 :: SNat 'Z
sn0 = SZ

sn1 :: SNat ('S 'Z)
sn1 = SS SZ

sn2 :: SNat ('S ('S 'Z))
sn2 = SS (SS SZ)

-- ----------------------------------------------------------------------------
sI    =                            SS SZ
sII   =                         SS (SS SZ)
sIII  =                      SS (SS (SS SZ))
sIV   =                   SS (SS (SS (SS SZ)))
sV    =                SS (SS (SS (SS (SS SZ))))
sVI   =             SS (SS (SS (SS (SS (SS SZ)))))
sVII  =          SS (SS (SS (SS (SS (SS (SS SZ))))))
sVIII =       SS (SS (SS (SS (SS (SS (SS (SS SZ)))))))
sIX   =    SS (SS (SS (SS (SS (SS (SS (SS (SS SZ))))))))
sX    = SS (SS (SS (SS (SS (SS (SS (SS (SS (SS SZ)))))))))
