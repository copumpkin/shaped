{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Shaped.Product where

import GHC.Prim
import GHC.Types
import GHC.IO

-- This doesn't seem entirely worthless... worth coming up with a decent API for it
-- including perhaps how to actually construct these things... :)

type family Map (f :: a -> b) (xs :: [a]) :: [b] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

data Product (ts :: [*])   = Product (Array# Any)
data Elem    (ts :: [*]) t = Elem Int#
data Length  (ts :: [*])   = Length Int#

pget :: Product ts -> Elem ts t -> t
pget (Product arr#) (Elem i#) = case indexArray# arr# i# of (# x #) -> unsafeCoerce# x

plength :: Product ts -> Length ts
plength (Product arr#) = Length (sizeofArray# arr#)

pelems :: Product ts -> Product (Map (Elem ts) ts)
pelems = undefined





reallyUnsafeElem :: Int -> Elem ts t
reallyUnsafeElem (I# i#) = Elem i#

