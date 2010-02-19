-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant
-- Copyright   :  (c) 2010, Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Top-level module which reexports the basic combinators and the generic
-- instances for common datatypes.
--
-----------------------------------------------------------------------------

module Generics.Instant (
  module Generics.Instant.Base,
  ) where
  
import Generics.Instant.Base
import Generics.Instant.Instances ()
