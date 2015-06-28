{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Dockerfile.TypeLevel where

------------------------------------------------------------------------

type family Contains (needle :: *) (haystack :: [*]) :: Bool where
  Contains x '[]       = 'False
  Contains x (x ': ys) = 'True
  Contains x (y ': ys) = Contains x ys

type Requires x cs = Contains x cs ~ 'True

------------------------------------------------------------------------

-- | Append for type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

------------------------------------------------------------------------

-- | Logical-or for type-level booleans.
type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'False 'False = 'False
  Or _x     _y     = 'True
