{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Dockerfile.TypeLevel where

------------------------------------------------------------------------

-- | Searches for a required type in the list, and evaluates to True if it is
-- missing.
type family Missing (requirement :: *) (provided :: [*]) :: Bool where
  Missing x '[]       = 'True
  Missing x (x ': ys) = 'False
  Missing x (y ': ys) = Missing x ys

-- NOTE Using `Missing x cs ~ False` gives better error messages
-- NOTE than `Contains x cs ~ True`.

-- | Requires that `cs` contains `x`.
type Requires x cs = Missing x cs ~ 'False

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
