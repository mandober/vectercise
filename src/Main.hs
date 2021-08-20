module Main where

-- ----------------------------------------------------------------------------
-- mine imports
import Data.Vec.EVec
import Data.Vec.FNat
import Data.Vec.Nat
import Data.Vec.NatOps
import Data.Vec.SNat
import Data.Vec.Vec
import Data.Vec.VecOps1
import Data.Vec.VecOps2
import Data.Vec.VecOps3
import Data.Vec.VecOps4
import Data.Vec.VecOps5
import Data.Vec.VecOps6
import Data.Vec.VecOps7

-- ----------------------------------------------------------------------------
-- imports
import Data.Kind

-- ----------------------------------------------------------------------------
-- hide these Prelude things
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
    , length
    , replicate
    , take
    , filter
    , (++)
    , reverse
    , (!!)
    )


-- ----------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "How about them length-indexed vectors!"
    putStrLn $ "v3: " <> showVec v3
    putStrLn $ "v5: " <> showVec v5
    putStrLn $ "head v5: " <> show (head v5)
    putStrLn $ "tail v5: " <> showVec (tail v5)
    putStrLn $ "map (^2) v5: " <> showVec (map (^ 2) v5)
    putStrLn $ "length v5: " <> show (length v5)
    putStrLn $ "replicate (SS (SS SZ)) 'a': " <> showVec (replicate sV 'a')
    putStrLn $ "take (SS (SS SZ)) v5: " <> showVec (take (SS (SS SZ)) v5)
    putStrLn $ "v5 ++ v3: " <> showVec (v5 ++ v3)
    putStrLn $ "filter (< 2) v5: " <> show (filter (< 2) v5)
    putStrLn $ "reverse v5: " <> showVec (reverse v5)
    putStrLn $ "v5 !! FS (FS (FS (FS FZ))): " <> show (v5 !! FS (FS (FS (FS FZ))))


-- ----------------------------------------------------------------------------
