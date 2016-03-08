{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Studio where

import Data.Monoid
import Data.List (intercalate)
import Data.Typeable (Proxy (..))
import GHC.Exts
import Data.Default (Default (..))


data Rec :: (u -> *) -> [u] -> * where
  RNil :: Rec f '[]
  (:&) :: !(f x) -> !(Rec f xs) -> Rec f (x ': xs)

infixr 7 :&

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

rappend :: Rec f as -> Rec f bs -> Rec f (as ++ bs)
rappend RNil bs = bs
rappend (a :& as) bs = a :& (rappend as bs)

rmap :: (forall x. f x -> g x) -> Rec f as -> Rec g as
rmap _ RNil = RNil
rmap f (a :& as) = f a :& (rmap f as)
{-# INLINE rmap #-}

newtype Lift (op :: l -> l' -> *) (f :: k -> l) (g :: k -> l') (x :: k)
  = Lift { getLift :: op (f x) (g x)}

rapply :: Rec (Lift (->) f g) rs -> Rec f rs -> Rec g rs
rapply RNil RNil = RNil
rapply (f :& fs) (x :& xs) = getLift f x :& rapply fs xs

class RecApplicative rs where
  rpure :: (forall x. f x) -> Rec f rs

instance RecApplicative '[] where
  rpure _ = RNil

instance RecApplicative rs => RecApplicative (r ': rs) where
  rpure s = s :& rpure s

rtraverse :: Applicative h => (forall x. f x -> h (g x)) -> Rec f rs -> h (Rec g rs)
rtraverse _ RNil = pure RNil
rtraverse f (x :& xs) = (:&) <$> f x <*> rtraverse f xs

newtype Const (a :: *) (b :: k) = Const {getConst :: a}

recordToList :: Rec (Const a) rs -> [a]
recordToList RNil = []
recordToList (x :& xs) = getConst x : recordToList xs

data Dict c a where
  Dict :: c a => a -> Dict c a

type family RecAll (f :: u -> *) (rs :: [u]) (c :: * -> Constraint) :: Constraint where
  RecAll f '[] ss = ()
  RecAll f (x ': xs) ss = (ss (f x), RecAll f xs ss)


newtype Compose (f :: l -> *) (g :: k -> l) (x :: k) = Compose {getCompose :: f (g x)}

type (:.) f g = Compose f g

reifyConstraint :: RecAll f rs c => proxy c -> Rec f rs -> Rec (Dict c :. f) rs
reifyConstraint prx rec =
  case rec of
    RNil -> RNil
    (x :& xs) -> Compose (Dict x) :& reifyConstraint prx xs

instance RecAll f rs Show => Show (Rec f rs) where
  show xs =
    (\str -> "{" <> str <> "}")
      . intercalate ", "
      . recordToList
      . rmap (\(Compose (Dict x)) -> Const $ show x)
      $ reifyConstraint (Proxy :: Proxy Show) xs

instance Monoid (Rec f '[]) where
  mempty = RNil
  _ `mappend` _ = RNil

instance (Monoid (f r), Monoid (Rec f rs)) => Monoid (Rec f (r ': rs)) where
  mempty = mempty :& mempty
  (a :& as) `mappend` (b :& bs) = (a `mappend` b) :& (as `mappend` bs)

instance Eq (Rec f '[]) where
  _ == _ = True

instance (Eq (f r), Eq (Rec f rs)) => Eq (Rec f (r ': rs)) where
  (a :& as) == (b :& bs) = (a == b) && (as == bs)

instance Ord (Rec f '[]) where
  _ `compare` _ = EQ

instance (Ord (f r), Ord (Rec f rs)) => Ord (Rec f (r ': rs)) where
  (a :& as) `compare` (b :& bs) = (a `compare` b) <> (as `compare` bs)

-- Instanza di Default
instance Default (Rec f '[]) where
  def = RNil

instance (Default (f r), Default (Rec f rs)) => Default (Rec f (r ': rs)) where
  def = def :& def
