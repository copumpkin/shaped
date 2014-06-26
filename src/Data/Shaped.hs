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
module Data.Shaped where

import GHC.Generics

import Data.Shaped.Types
import Data.Shaped.Generics

class Shaped (t :: *) where
  type Shape t :: [[*]]

  -- Use Iso
  toData   :: t -> Data (Shape t)
  fromData :: Data (Shape t) -> t

  -- constructor information, if anyone cares, mapping each outer Elem to some metadata



  -- Generic defaults

  type Shape t = GShape (Rep t)

  default toData :: (Generic t, GShaped (Rep t)) => t -> Data (GShape (Rep t))
  toData = gtoData . from

  default fromData :: (Generic t, GShaped (Rep t)) => Data (GShape (Rep t)) -> t
  fromData = to . gfromData



-- Shitty test
data Foo
  = A Int Bool String
  | B
  | C Double Int
  deriving (Generic)

instance Shaped Foo

