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

module Data.Vec.VecOps5 where
-- :load src/Data/Vec/VecOps5.hs

-- imports:
import Data.Kind
import Prelude hiding
    ( 
    )

-- imports: mine
import Data.Vec.EVec
import Data.Vec.Nat
import Data.Vec.SNat
import Data.Vec.Vec

-- ----------------------------------------------------------------------------
-- Vec functions, Part 5: Constraints
-- ----------------------------------------------------------------------------
{-

-}

-- ----------------------------------------------------------------------------
-- Vec ops: 
-- ----------------------------------------------------------------------------
