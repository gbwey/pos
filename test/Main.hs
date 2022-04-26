{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Pos
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestPos"
    [ testCase "succP" $ succP _4P @?= _5P
    , testCase "succP" $ succP _1P @?= _2P
    , testCase "predP" $ predP _4P @?= Right _3P
    , testCase "predP" $ predP _2P @?= Right _1P
    , testCase "predP" $ predP _1P @?= Left "eitherPos: i<=0: found 0"
    , testCase "maxP" $ maxP _1P 0 @?= _1P
    , testCase "maxP" $ maxP _1P 13 @?= _13P
    , testCase "maxP" $ maxP _1P 1 @?= _1P
    , testCase "maxP" $ maxP _1P (-4) @?= _1P
    , testCase "maxP" $ maxP _10P 4 @?= _10P
    , testCase "maxP" $ maxP _10P 41 @?= _P @41
    , testCase "add" $ (_P @11) +! (_P @23) @?= (_P @34)
    , testCase "enums" $ map unP [_1P, _3P .. _10P] @?= [1, 3 .. 10]
    , testCase "posRange" $ posRange 2 5 @?= Right (_2P :| [_3P, _4P, _5P])
    , testCase "posRange" $ posRange 5 5 @?= Right (_5P :| [])
    , testCase "posRange" $ posRange 5 4 @?= Left "posRange: no values between 5 and 4"
    , testCase "posRange" $ posRange 0 0 @?= Left "eitherPos: i<=0: found 0"
    , testCase "posRange" $ posRange (-4) (-1) @?= Left "eitherPos: i<=0: found -4"
    , testCase "posRange" $ posRange (-4) (-10) @?= Left "eitherPos: i<=0: found -4"
    , testCase "minusP" $ minusP _2P _1P @?= Right _1P
    , testCase "minusP" $ minusP _4P _3P @?= Right _1P
    , testCase "minusP" $ minusP _4P _4P @?= Left "eitherPos: i<=0: found 0"
    , testCase "minusP" $ minusP _4P _7P @?= Left "eitherPos: i<=0: found -3"
    , testCase "fromPositives" $ fromPositives (_4P :| [_7P, _3P, _10P]) @?= [4, 7, 3, 10]
    , testCase "fromPositives" $ fromPositives (_1P :| []) @?= [1]
    , testCase "toPositives" $ toPositives [] @?= Left "toPositives: empty"
    , testCase "toPositives" $ toPositives [-3] @?= Left "eitherPos: i<=0: found -3"
    , testCase "toPositives" $ toPositives [0] @?= Left "eitherPos: i<=0: found 0"
    , testCase "toPositives" $ toPositives [1, 2, 0, 4] @?= Left "eitherPos: i<=0: found 0"
    , testCase "toPositives" $ toPositives [5, 2, 1] @?= Right (_5P :| [_2P, _1P])
    , testCase "productP" $ productP [] @?= _1P
    , testCase "productP" $ productP (_3P :| [_1P, _5P, _7P]) @?= _P @105
    , testCase "productP" $ productP (_4P :| []) @?= _4P
    , testCase "productP" $ productP [_4P, _5P] @?= _P @20
    , testCase "productP" $ productP [_4P] @?= _4P
    , testCase "read" $ unP (read @Pos "_1325P") @?= 1325
    , testCase "read" $ read @Pos "_1325P" @?= _P @1325
    , testCase "show" $ show (_P @1325) @?= "_1325P"
    , testCase "read/show" $ read @Pos (show (_P @1325)) @?= _P @1325
    , testCase "reads 0" $ reads @Pos "0P" @?= []
    , testCase "reads -4" $ reads @Pos "-4P" @?= []
    , testCase "reads space between" $ reads @Pos " 9 P" @?= []
    , testCase "reads leading spaces and extra" $ reads @Pos " _9P xyz" @?= [(_9P, " xyz")]
    , testCase "reads leading spaces and extra" $ reads @Pos "   _123P" @?= [(_P @123, "")]
    , testCase "fromNSP" $ fromNSP @'[4] @?= (_4P :| [])
    , testCase "fromNSP" $ fromNSP @'[4, 6, 3] @?= (_4P :| [_6P, _3P])
    , testCase "fromNP 1" $ fromNP @1 @?= _1P
    , testCase "fromNP 2" $ fromNP @2 @?= _2P
    , testCase "fromNP 3" $ fromNP @3 @?= _3P
    , testCase "fromNP 4" $ fromNP @4 @?= _4P
    , testCase "fromNP 5" $ fromNP @5 @?= _5P
    , testCase "fromNP 6" $ fromNP @6 @?= _6P
    , testCase "fromNP 7" $ fromNP @7 @?= _7P
    , testCase "fromNP 8" $ fromNP @8 @?= _8P
    , testCase "fromNP 9" $ fromNP @9 @?= _9P
    , testCase "fromNP 10" $ fromNP @10 @?= _10P
    , testCase "fromNP 11" $ fromNP @11 @?= _11P
    , testCase "fromNP 12" $ fromNP @12 @?= _12P
    , testCase "fromNP 13" $ fromNP @13 @?= _13P
    , testCase "fromNP 14" $ fromNP @14 @?= _14P
    , testCase "fromNP 15" $ fromNP @15 @?= _15P
    , testCase "fromNP 16" $ fromNP @16 @?= _16P
    , testCase "fromNP 17" $ fromNP @17 @?= _17P
    , testCase "fromNP 18" $ fromNP @18 @?= _18P
    , testCase "fromNP 19" $ fromNP @19 @?= _19P
    , testCase "fromNP 20" $ fromNP @20 @?= _20P
    , testCase "fromN 4" $ fromN @4 @?= 4
    , testCase "fromN 1" $ fromN @1 @?= 1
    , testCase "nsLengthP" $
        nsLengthP @'[2, 3, 20]
          @?= _3P
    , testCase "nsLengthP" $
        nsLengthP @'[99]
          @?= _1P
    , testCase "nsLengthP" $
        nsLengthP @'[1, 2, 3, 4, 5, 6, 7, 8, 999]
          @?= _9P
    , testCase "fromNSTotalP" $
        fromNSTotalP @'[2, 3, 20]
          @?= _P @120
    , testCase "fromNSTotalP" $
        fromNSTotalP @'[1]
          @?= _1P
    , testCase "fromNSTotalP" $
        fromNSTotalP @'[1, 2, 3, 4, 5, 6, 7, 1, 1]
          @?= _P @5040
    , testCase "fromNSP" $
        fromNSP @'[2, 3, 20]
          @?= _2P :| [_3P, _20P]
    , testCase "pPosInt" $
        P.readP_to_S pPosInt "  12xyz"
          @?= [(_1P, "2xyz"), (_12P, "xyz")] -- ambiguous
    , testCase "pPos" $
        P.readP_to_S pPos "  _12Pxyz"
          @?= [(_12P, "xyz")]
    , testCase "pPosInt" $
        P.readP_to_S pPosInt "  10xyz"
          @?= [(_1P, "0xyz"), (_10P, "xyz")]
    , testCase "pPosInt" $
        P.readP_to_S pPosInt "  023xyz"
          @?= [(_2P, "3xyz"), (_P @23, "xyz")]
    , testCase "pPos" $
        P.readP_to_S pPos "  _12P xyz"
          @?= [(_12P, " xyz")]
    , testCase "readP pPositives" $
        P.readP_to_S ((,) <$> pInt <* P.char '@' <*> pPositives '{' '}') "1444@{1}"
          @?= [((1444, _1P :| []), "")]
    , testCase "readP pPositives" $
        P.readP_to_S ((,) <$> pInt <* P.char '@' <*> pPositives '{' '}') "1444@{1,2}"
          @?= [((1444, _1P :| [_2P]), "")]
    , testCase "readP pPositives" $
        P.readP_to_S ((,) <$> pInt <* P.char '@' <*> pPositives '{' '}') "1444@{1,2,3}"
          @?= [((1444, _1P :| [_2P, _3P]), "")]
    , testCase "readP pPositives" $
        P.readP_to_S ((,) <$> pInt <* P.char '@' <*> pPositives '{' '}') "0@{1,2,3}"
          @?= [((0, _1P :| [_2P, _3P]), "")]
    , testCase "safeDivP" $ safeDivP _1P _5P @?= _1P
    , testCase "safeDivP" $ safeDivP _10P _5P @?= _3P
    , testCase "safeDivP" $ safeDivP _11P _5P @?= _3P
    , testCase "safeDivP" $ safeDivP _10P _15P @?= _1P
    , testCase "safeDivP" $ safeDivP (_P @100) _7P @?= _15P
    , testCase "safeDivP" $ safeDivP _10P _10P @?= _2P
    , testCase "divModNextP" $ divModNextP 10 _10P @?= (1, _1P)
    , testCase "divModNextP" $ divModNextP 100 _7P @?= (14, _3P)
    , testCase "divModNextP" $ divModNextP 2 _20P @?= (0, _3P)
    , testCase "divModNextP" $ divModNextP 10 _6P @?= (1, _5P)
    , testCase "divModP" $ divModP 10 _10P @?= (1, 0)
    , testCase "divModP" $ divModP 100 _7P @?= (14, 2)
    , testCase "divModP" $ divModP 2 _20P @?= (0, 2)
    , testCase "divModP" $ divModP 10 _6P @?= (1, 4)
    ]
