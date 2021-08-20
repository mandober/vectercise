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

module Data.Vec.VecOps6 where
-- :load src/Data/Vec/VecOps6.hs

-- imports:
import Data.Type.Equality
import Data.Kind
import Unsafe.Coerce
import Prelude hiding
    ( reverse
    )

-- imports: mine
import Data.Vec.EVec
import Data.Vec.Nat
import Data.Vec.NatOps
import Data.Vec.SNat
import Data.Vec.Vec

-- ----------------------------------------------------------------------------
-- Vec functions, Part 6: Proofs
-- ----------------------------------------------------------------------------
{-
Writing structural inductive proofs to let GHC know about a few things.

Related to the "association" between certain TF and functions on vectors. That is, related to the necessary mirroring of their structures.

For example, NatAdd TF has the equation
Z + m = m
but the function using it, to type-encode a Vec's length (which is a Nat) fails because GHC cannot extrapolate commutativity of addition from this equation alone; instead, it needs to be shown a proof of it:
m + Z = m

And just adding this equation to the TF NAtAdd screws things up coz the strucutures ceise to be mirrors of eachother, they ceise to be almost exactly the same. On the other hand, adding the similar equation to both TF and the function is not always feasable for the function (TF would work fine on its own, though).

We do proofs using propositional equality (:~:) from Data.Type.Equality
This is the type-equality that disregards kinds.

    type (:~:) :: forall k. k -> k -> Type
    data a :~: b where
        Refl :: forall k (a :: k). a :~: a

The module says: Propositional equality: if (a :~: b) is inhabited by some terminating value, then the type `a` is the same as the type `b`. To use this equality in practice, pattern-match on the (a :~: b) to get out the Refl constructor; in the body of the pattern-match, the compiler knows that (a ~ b).


    instance forall k (a :: k) (b :: k).            Eq      (a :~: b)
    instance forall k (a :: k) (b :: k).            Ord     (a :~: b)
    instance forall k (a :: k) (b :: k).            Show    (a :~: b)
    instance forall k (a :: k) (b :: k). (a ~ b) => Read    (a :~: b)
    instance forall k (a :: k) (b :: k). (a ~ b) => Enum    (a :~: b)
    instance forall k (a :: k) (b :: k). (a ~ b) => Bounded (a :~: b)

These instances of the usual classes (Eq, Show, etc.) seems to be defined for any pair of types a and b, declaring them propositionally equal. Most of them are auto-derived.


BTW, the stronger type-equality wrt kinds is (:~~:)

    type (:~~:) :: forall k1 k2. k1 -> k2 -> Type
    data a :~~: b where
        HRefl :: forall k1 (a :: k1). a :~~: a

There then, the TF has the equation  Z + m = m, so we need to prove that this still holds with the params swapped: m + Z = m

    type (+) :: Nat -> Nat -> Nat
    type family n + m where
        Z     + m = m             (1)
        (S n) + m = S (n + m)     (2)


Z + m = m <=>               (1)
m + Z = m                   (1')

(S n) + m = S (n + m) <=>   (2)
n + (S m) = S (n + m)       (2')

-}


-- ----------------------------------------------------------------------------
-- Vec ops: reverse
-- ----------------------------------------------------------------------------
reverse1 :: Vec n a -> Vec n a
reverse1 Nil         = Nil
reverse1 (Cons x xs) = reverse1 xs `snoc` x

snoc :: Vec n a -> a -> Vec (S n) a
snoc Nil x         = x ↑ Nil
snoc (Cons y ys) x = y ↑ ys `snoc` x
{-
This reverse1 works but in quadratic time O(n²), the same as the reverse on list coded in a similar manner. To improve it to O(n) we need (the same as for lists) the accumulator.

However, introducing the acc means having a helper function, which, because we're dealing wih GADTs, needs its own signature - and that signature is the cause of all the commotion.

Without the sig, we get the error:

    Couldn't match expected type 'p1' with actual type 'p'
    'p' is untouchable
        inside the constraints: n1 ~ 'Z
        bound by a pattern with constructor:
        Nil :: forall a. Vec 'Z a,
    'p' is a rigid type variable bound by
        the inferred type of go :: p -> Vec n1 a1 -> p1
    'p1' is a rigid type variable bound by
        the inferred type of go :: p -> Vec n1 a1 -> p1
    Possible fix: add a type signature for 'go'

As the error says we need a sig and this sig is gonna be our demise.
Starting with the sig: go :: Vec m a -> Vec p a -> Vec (m + p) a

    reverse :: Vec n a -> Vec n a
    reverse ys = go Nil ys
        where
        go :: Vec m a -> Vec p a -> Vec (m + p) a
        go z Nil = z
        go z (Cons x xs) = go (Cons x z) xs

we get an error that:
    • Could not deduce: (m + 'Z) ~ m
      from the context: p ~ 'Z
      bound by a pattern with constructor:
      Nil :: forall a. Vec 'Z a

Deducing that (m + 'Z) ~ m seems str8fwd but GHC cannot do it. It relies on the TF NatAdd that has the equation (Z + m) ~ m, but this one has the args swapped. We need to do a proof of prop eq:

    mPlusZero :: forall m. (m + Z) :~: m
    mPlusZero = undefined

    reverse :: Vec n a -> Vec n a
    reverse ys = go Nil ys
        where
        go :: Vec m a -> Vec p a -> Vec (m + p) a
        go z Nil = z
        go z (Cons x xs) = go (Cons x z) xs


We stil get errors, the first of which is:

    • Could not deduce: (m + 'Z) ~ m
      from the context: p ~ 'Z
        bound by a pattern with constructor:
        Nil :: forall a. Vec 'Z a
      or from: m0 ~ (m0 + 'Z)
        bound by a pattern with constructor:
        Refl :: forall {k} (a :: k). a :~: a,
        in a case alternative
      Expected: Vec (m + p) a1
        Actual: Vec m a1
      m is a rigid type variable bound by
        the type signature for:
          go :: forall (m :: Nat) a1 (p :: Nat).
                Vec m a1 -> Vec p a1 -> Vec (m + p) a1
    • In the expression: z
      In a case alternative: Refl -> z
      In the expression: case mPlusZero of { Refl -> z }
    • Relevant bindings include
        z :: Vec m a1
        go :: Vec m a1 -> Vec p a1 -> Vec (m + p) a1

We need to specify (type apply) @m to mPlusZero so we case the expression (mPlusZero @m). This will finally prove the first equation; it will show GHC that m + Z = m holds, just like the Z + m = m holds due to TF. That is, we have managed to prove the former from the latter.


Now the second error message related to the second equation says:

 • Could not deduce: (m + 'S n1) ~ 'S (m + n1)
      from the context: p ~ 'S n1
        bound by a pattern with constructor:
        Cons :: forall a (n :: Nat). a -> Vec n a -> Vec ('S n) a
      Expected: Vec (m + p) a1
        Actual: Vec ('S m + n1) a1
    • In the expression: go (Cons x z) xs
      In an equation for go: go z (Cons x xs) = go (Cons x z) xs
      In an equation for reverse:
          reverse ys = go Nil ys
            where
            go :: forall m p a. Vec m a -> Vec p a -> Vec (m + p) a
            go z Nil = case mPlusZero @m of { Refl -> z }
            go z (Cons x xs) = go (Cons x z) xs
    • Relevant bindings include
        xs :: Vec n1 a1
        z  :: Vec m  a1
        go :: Vec m  a1 -> Vec p a1 -> Vec (m + p) a1

We also have to case the RHS of the 2nd eqaution, while applying @m and @p to mPlusSucc, i.e. as 'case mPlusSucc @m @p of'. However, it doesn't make the error go away because the @p is the entire Vec p a, but we need a type variable binding only its tail. So, in the pattern match on the LHS, we need to type annotate xs with 'Vec pp a', as in '(xs :: Vec pp a)'. Then we need to change the part on the RHS and have @pp applied instead of @p, as in 
'case mPlusSucc @m @pp of'. Now it's error free! Yey! So we have:

    reverse :: Vec n a -> Vec n a
    reverse ys = go Nil ys
        where
        go :: forall m p a. Vec m a -> Vec p a -> Vec (m + p) a
        go z Nil                       = case mPlusZero @m     of
            Refl -> z
        go z (Cons x (xs :: Vec pp a)) = case mPlusSucc @m @pp of
            Refl -> go (Cons x z) xs

    mPlusZero :: forall m. (m + Z) :~: m
    mPlusZero = undefined

    mPlusSucc :: forall n m. (n + (S m)) :~: (S (n + m))
    mPlusSucc = undefined


We still need to define mPlusZero and mPlusSucc, though...


-}

reverse :: Vec n a -> Vec n a
reverse ys = go SZ Nil ys
    where
    go :: forall m p a. SNat m -> Vec m a -> Vec p a -> Vec (m + p) a
    go m z Nil =
        case mPlusZero @m m of
            Refl -> z
    go m z (Cons x (xs :: Vec pp a)) =
        case mPlusSucc @pp @m m of
            Refl -> go (SS m) (Cons x z) xs


mPlusZero :: forall m. SNat m -> (m + Z) :~: m
mPlusZero SZ = Refl
mPlusZero (SS n) = case mPlusZero n of Refl -> Refl

{-# NOINLINE mPlusZero #-}
{-# RULES "mPlusZero" forall m. mPlusZero m = unsafeCoerce Refl #-}


mPlusSucc :: forall n m. SNat m -> (m + S n) :~: S (m + n)
mPlusSucc SZ = Refl
mPlusSucc (SS m') = case mPlusSucc @n m' of Refl -> Refl

{-# NOINLINE mPlusSucc #-}
{-# RULES "mPlusSucc" forall m. mPlusSucc m = unsafeCoerce Refl #-}
