{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Shaped.Generics where

import GHC.Generics
import Unsafe.Coerce
import Data.Proxy

import Data.Shaped.Types

plusLength :: Length xs -> Length ys -> Length (xs ++ ys)
plusLength Zero ys = ys
plusLength (Suc xs) ys = Suc (plusLength xs ys)

weaken :: proxy ys -> Elem xs x -> Elem (xs ++ ys) x
weaken _ = unsafeCoerce

bump :: Length xs -> Elem ys y -> Elem (xs ++ ys) y
bump Zero x = x
bump (Suc n) x = There (bump n x)

split :: Length xs -> proxy ys -> Elem (xs ++ ys) x -> Either (Elem xs x) (Elem ys x)
split Zero _ e = Right e
split (Suc n) _ Here = Left Here
split (Suc n) py (There (split n py -> Left  p)) = Left (There p)
split (Suc n) py (There (split n py -> Right q)) = Right q

append :: HList xs -> HList ys -> HList (xs ++ ys)
append Nil ys = ys
append (x :> xs) ys = x :> (append xs ys)

unappend :: Length xs -> proxy ys -> HList (xs ++ ys) -> (HList xs, HList ys)
unappend Zero _ xs = (Nil, xs)
unappend (Suc n) py (x :> xs) | (l, r) <- unappend n py xs = (x :> l, r)




class GShaped (f :: * -> *) where
  type GShape f :: [[*]]

  gtoData   :: f a -> Data (GShape f)
  gfromData :: Data (GShape f) -> f a

  glength :: proxy f -> Length (GShape f)


instance GShaped f => GShaped (D1 c f) where
  type GShape (D1 c f) = GShape f

  gtoData = gtoData . unM1
  gfromData = M1 . gfromData

  glength _ = glength (Proxy :: Proxy f)

instance GShapedP f => GShaped (C1 c f) where
  type GShape (C1 c f) = '[ GShapeP f ]

  gtoData (M1 x) = Data Here (gtoHList x)
  gfromData (Data Here f) = M1 (gfromHList f)

  glength _ = Suc Zero

instance GShaped V1 where
  type GShape V1 = '[]

  gtoData = \case {}
  gfromData = \case {}

  glength _ = Zero


instance (GShaped f, GShaped g) => GShaped (f :+: g) where
  type GShape (f :+: g) = GShape f ++ GShape g

  gtoData (L1 (gtoData -> Data t f)) = Data (weaken (Proxy :: Proxy (GShape g)) t) f
  gtoData (R1 (gtoData -> Data t f)) = Data (bump (glength (Proxy :: Proxy f))  t) f

  gfromData (Data (split (glength (Proxy :: Proxy f)) (Proxy :: Proxy (GShape g)) -> Left  l) f) = L1 (gfromData (Data l f))
  gfromData (Data (split (glength (Proxy :: Proxy f)) (Proxy :: Proxy (GShape g)) -> Right r) f) = R1 (gfromData (Data r f))

  glength _ = glength (Proxy :: Proxy f) `plusLength` glength (Proxy :: Proxy g)




class GShapedP f where
  type GShapeP f :: [*]

  gtoHList   :: f a -> HList (GShapeP f)
  gfromHList :: HList (GShapeP f) -> f a

  glengthP :: proxy f -> Length (GShapeP f)

instance GShapedP f => GShapedP (S1 c f) where
  type GShapeP (S1 c f) = GShapeP f

  gtoHList = gtoHList . unM1
  gfromHList = M1 . gfromHList

  glengthP _ = glengthP (Proxy :: Proxy f)

instance GShapedP U1 where
  type GShapeP U1 = '[]

  gtoHList _ = Nil
  gfromHList _ = U1

  glengthP _ = Zero

instance (GShapedP f, GShapedP g) => GShapedP (f :*: g) where
  type GShapeP (f :*: g) = GShapeP f ++ GShapeP g

  gtoHList (xs :*: ys) = gtoHList xs `append` gtoHList ys
  gfromHList (unappend (glengthP (Proxy :: Proxy f)) (Proxy :: Proxy (GShapeP g)) -> (l, r)) = (gfromHList l :*: gfromHList r)

  glengthP _ = glengthP (Proxy :: Proxy f) `plusLength` glengthP (Proxy :: Proxy g)


instance GShapedP (K1 i c) where
  type GShapeP (K1 i c) = '[c]

  gtoHList (K1 x) = x :> Nil
  gfromHList (x :> _) = K1 x

  glengthP _ = Suc Zero
