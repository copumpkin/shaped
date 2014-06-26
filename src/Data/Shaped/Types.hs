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
module Data.Shaped.Types where

infixr 1 :>

data Length (ts :: [k]) where
  Zero :: Length '[]
  Suc  :: Length ts -> Length (t ': ts)

type family (l :: [k]) ++ (r :: [k]) :: [k] where
  '[] ++ r = r
  (x ': xs) ++ r = x ': (xs ++ r)

data Elem (ts :: [k]) (t :: k) where
  Here  :: Elem (t ': ts) t
  There :: Elem ts t -> Elem (x ': ts) t

data HList (ts :: [*]) where
  Nil  :: HList '[]
  (:>) :: t -> HList ts -> HList (t ': ts)

data Data (shape :: [[*]]) = forall xs. Data { tag :: Elem shape xs, fields :: HList xs }
