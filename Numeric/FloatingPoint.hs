-- | Constants related to floating point numbers 
module Numeric.FloatingPoint ( 
    FloatingPoint(..)
  ) where


class FloatingPoint a where
  -- | Smallest @x@ such that @1+x /= 1@
  epsilon    :: a
  -- | Maximum positive representable number
  maxReprNum :: a
  -- | Minimal positive representable number
  minReprNum :: a
  -- | Positive infinity
  posInfty   :: a
  -- | Negative infinity 
  negInfty   :: a 
  -- | Not a number
  nan        :: a
  

instance FloatingPoint Double where
  epsilon    = 2^^(-53)
  maxReprNum = (2^^53 - 1) * 2^^(-53) * 2^512 * 2^512
  minReprNum = 2^^(-1022)
  posInfty   =   1  / 0
  negInfty   = (-1) / 0
  nan        = 0 / 0

-- instance FloatingPoint Float where
--   epsilon    = 2^^(-24)
--   maxReprNum = 1
--   minReprNum = 1
--   posInfty   =   1  / 0
--   negInfty   = (-1) / 0
--   nan        = 0 / 0

-- NOTE!
--  There is package ieee754 which provide exactly same functionality
--  I should switch to it
