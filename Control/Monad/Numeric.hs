-- |
-- Module    : Control.Monad.Numeric
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Function useful for writing numeric code which works with mutable
-- data.
module Control.Monad.Numeric (
    forGen
  , for
  ) where

-- | For function which act much like for loop in the C
forGen :: Monad m 
       => a                     -- ^ Staring index value
       -> (a -> Bool)           -- ^ Condition
       -> (a -> a)              -- ^ Function to modify index
       -> (a -> m ())           -- ^ Action to perform
       -> m ()
forGen n test next a = worker n
  where
    worker i | test i    = a i >> worker (next i)
             | otherwise = return ()
{-# INLINE forGen #-}

-- | Specialized for loop. Akin to:
--
-- > for( i = 0; i < 10; i++) { ...
for :: Monad m 
    => Int                      -- ^ Starting index
    -> Int                      -- ^ Maximal index value not reached
    -> (Int -> m ())            -- ^ Action to perfor,
    -> m ()
for i maxI a = worker i
  where
    worker j | j < maxI  = a j >> worker (j+1)
             | otherwise = return ()
{-# INLINE for #-}
