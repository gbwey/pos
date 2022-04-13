{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Data.Pos
Description : positive numbers
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Data.Pos (
  -- ** core type
  Pos,

  -- ** destructors
  pattern Pos,
  unP,

  -- ** constructors
  _P,
  unsafePos,
  eitherPos,

  -- ** values
  _1P,
  _2P,
  _3P,
  _4P,
  _5P,
  _6P,
  _7P,
  _8P,
  _9P,
  _10P,
  _11P,
  _12P,
  _13P,
  _14P,
  _15P,
  _16P,
  _17P,
  _18P,
  _19P,
  _20P,

  -- ** arithmetic
  (*!),
  (+!),
  minusP,
  productP,
  productPInt,
  safeDivP,
  divModP,
  divModNextP,
  maxP,
  predP,
  succP,

  -- ** type level
  PosC (..),
  NS (..),

  -- ** miscellaneous
  fromPositives,
  toPositives,
  posRange,

  -- * parsers
  pPositives,
  pPos,
  pPosInt,
  pInt,
) where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Data.Char
import Data.Foldable
import Data.Function
import Data.Kind
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Proxy
import GHC.Enum
import GHC.Natural
import GHC.Read (readPrec)
import GHC.Stack
import GHC.TypeLits (KnownNat, Nat)
import qualified GHC.TypeLits as GL
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read (readMaybe)

-- | holds a positive number
newtype Pos = Pos' Int
  deriving stock (Eq, Ord)
  deriving newtype (NFData)

instance Show Pos where
  showsPrec _ (Pos i) = showChar '_' . showsPrec 11 i . showChar 'P'

-- | readonly pattern synonym for 'Pos'
{-# COMPLETE Pos #-}

pattern Pos :: Int -> Pos
pattern Pos n <- Pos' n

-- | parser for an 'Int'
pInt :: P.ReadP Int
pInt = do
  ii <- P.many1 (P.satisfy isDigit)
  maybe P.pfail pure (readMaybe @Int ii)

-- | parser for a 'Pos'
pPos :: P.ReadP Pos
pPos = P.skipSpaces *> P.char '_' *> pPosInt' <* P.char 'P'

-- | parser for a 'Pos' but just reading in a positive number
pPosInt :: P.ReadP Pos
pPosInt = P.skipSpaces *> pPosInt'

-- | parser for an int converted positive number
pPosInt' :: P.ReadP Pos
pPosInt' = do
  i <- pInt
  either (const empty) return (eitherPos i)

-- | parser for a list of positive numbers as ints
pPositives :: Char -> Char -> P.ReadP (NonEmpty Pos)
pPositives o c = do
  xs <- P.char o *> P.sepBy1 pPosInt (P.char ',') <* P.char c
  case xs of
    [] -> error "pPositives: empty" -- should not fail
    a : as -> pure (a :| as)

instance Read Pos where
  readPrec = PC.readP_to_Prec (const pPos)

-- | unwrap 'Pos'
unP :: Pos -> Int
unP (Pos i) = i
{-# INLINE unP #-}

-- | 'Enum' instance for 'Pos'
instance Enum Pos where
  pred (Pos i) = unsafePos "Enum.pred" (i - 1)
  toEnum = unsafePos "Enum.toEnum"
  fromEnum (Pos i) = i
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Bounded Pos where
  minBound = _1P
  maxBound = Pos' maxBound

infixl 7 *!

-- | multiply two positive numbers
(*!) :: Pos -> Pos -> Pos
Pos a *! Pos b = Pos' (a * b)
{-# INLINE (*!) #-}

infixl 6 +!

-- | add two positive numbers
(+!) :: Pos -> Pos -> Pos
Pos a +! Pos b = Pos' (a + b)
{-# INLINE (+!) #-}

-- | subtract two positive numbers
minusP :: Pos -> Pos -> Either String Pos
minusP = (eitherPos .) . on (-) unP

-- | try to convert an 'Int' to a 'Pos'
unsafePos :: HasCallStack => String -> Int -> Pos
unsafePos msg i
  | i >= 1 = Pos' i
  | otherwise = error $ "unsafePos:" ++ msg ++ " cannot be less than 1 found " ++ show i
{-# INLINE unsafePos #-}

-- | try to convert an 'Int' to a 'Pos'
eitherPos :: Int -> Either String Pos
eitherPos i
  | i <= 0 = Left $ "eitherPos: i<=0: found " ++ show i
  | otherwise = Right (Pos' i)
{-# INLINE eitherPos #-}

-- mod is always between 1 and N

-- | 'divMod' for 'Pos'
divModNextP :: Int -> Pos -> (Int, Pos)
divModNextP i (Pos j) = second (\n -> Pos' (n + 1)) (divMod i j)
{-# INLINE divModNextP #-}

-- | 'divMod' for 'Pos' returning 'Natural' for the remainder
divModP :: Int -> Pos -> (Int, Natural) -- second Int is always >= 0
divModP i (Pos n) = second toEnum (divMod i n) -- works fine
{-# INLINE divModP #-}

-- adds 1 to any division! this is useful for chunks where there is usually a remainder: also guarantees that Pos is valid

-- | safely divide 'Pos' values but the result is increased by one to guarantee the result is still positive
safeDivP :: Pos -> Pos -> Pos -- have to completely spell it out! else liquid gets confused
safeDivP (Pos i) (Pos j) = Pos' (mod i j + 1)
{-# INLINE safeDivP #-}

-- | product of list of 'Pos' values is always positive
productP :: Foldable t => t Pos -> Pos
productP = L.foldl' (*!) _1P
{-# INLINE productP #-}

-- | product of list of 'Pos' values is always positive
productPInt :: Foldable t => t Pos -> Int
productPInt = unP . productP
{-# INLINE productPInt #-}

-- | max of a 'Pos' and an 'Int'
maxP :: Pos -> Int -> Pos
maxP (Pos n) i = Pos' (max n i)
{-# INLINE maxP #-}

-- | next value for 'Pos' (not redundant as it is always successful and never partial)
succP :: Pos -> Pos
succP (Pos n) = Pos' (n + 1)
{-# INLINE succP #-}

-- | previous value for 'Pos'
predP :: Pos -> Either String Pos
predP (Pos n) = eitherPos (n - 1)
{-# INLINE predP #-}

-- | extract an 'Int' from a 'Nat'
pnat :: forall n. GL.KnownNat n => Int
pnat = fromInteger (GL.natVal (Proxy @n))

-- | constraint for positive numbers
type PosC :: Nat -> Constraint
class KnownNat n => PosC n where
  fromNP :: Pos
  fromN :: Int
  fromN = unP (fromNP @n)
instance
  ( KnownNat n
  , FailUnless
      (1 GL.<=? n)
      ( 'GL.Text "PosC n: requires n >= 1 but found "
          'GL.:<>: 'GL.ShowType n
      )
  ) =>
  PosC n
  where
  fromNP = unsafePos "fromN" (pnat @n)

type FailUnless :: Bool -> GL.ErrorMessage -> Constraint
type family FailUnless b err where
  FailUnless 'False err = GL.TypeError ( 'GL.Text "FailUnless: " 'GL.:<>: err)
  FailUnless 'True _ = ()

-- | conversion from list of Nats to Positives
type NS :: [Nat] -> Constraint
class NS ns where
  fromNSP :: NonEmpty Pos
  fromNSTotalP :: Pos
  fromNSTotalP = productP (fromNSP @ns)
  nsLengthP :: Pos

instance GL.TypeError ( 'GL.Text "NS: empty dimensions are not supported") => NS '[] where
  fromNSP = error "fromNSP: should not be here"
  nsLengthP = error "fromNSP: should not be here"
instance PosC n => NS '[n] where
  fromNSP = fromNP @n :| []
  nsLengthP = _1P
instance (PosC n, NS (n1 ': ns)) => NS (n ': n1 ': ns) where
  fromNSP = fromNP @n N.<| fromNSP @(n1 ': ns)
  nsLengthP = succP (nsLengthP @(n1 ': ns))

-- | construct a valid 'Pos' using a 'Nat'
_P :: forall n. PosC n => Pos
_P = Pos' (pnat @n)

-- | converts a container of positives to a list of ints
fromPositives :: Foldable t => t Pos -> [Int]
fromPositives = foldr ((:) . unP) []

-- | converts a list of ints to a nonempty list of positives
toPositives :: Foldable t => t Int -> Either String (NonEmpty Pos)
toPositives is = do
  ps <- traverse eitherPos (toList is)
  case ps of
    [] -> Left "toPositives: empty"
    x : xs -> Right (x :| xs)

-- | enumerate a nonempty list of 'Pos' from "i" to "j"
posRange :: Int -> Int -> Either String (NonEmpty Pos)
posRange i j = do
  i' <- eitherPos i
  j' <- eitherPos j
  case [i' .. j'] of
    [] -> Left $ "posRange: no values between " ++ show (unP i') ++ " and " ++ show (unP j')
    a : as -> Right (a :| as)

-- | commonly used values for 'Pos'
_1P, _2P, _3P, _4P, _5P, _6P, _7P, _8P, _9P, _10P, _11P, _12P, _13P, _14P, _15P, _16P, _17P, _18P, _19P, _20P :: Pos
_1P = Pos' 1
_2P = Pos' 2
_3P = Pos' 3
_4P = Pos' 4
_5P = Pos' 5
_6P = Pos' 6
_7P = Pos' 7
_8P = Pos' 8
_9P = Pos' 9
_10P = Pos' 10
_11P = Pos' 11
_12P = Pos' 12
_13P = Pos' 13
_14P = Pos' 14
_15P = Pos' 15
_16P = Pos' 16
_17P = Pos' 17
_18P = Pos' 18
_19P = Pos' 19
_20P = Pos' 20
