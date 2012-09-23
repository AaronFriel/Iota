{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
module Data.Iota.Stateful.Tests.Text
 where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Internal.Buffer
import Data.Attoparsec.Text
import Data.Iota.Stateful.Text
import Data.Functor.Identity
import Control.Monad.Identity
import Control.Applicative
import Data.Monoid
import Control.Arrow
import Data.Text
import qualified Data.Text.IO as T


data CParserTest = CData
                 | CForwardSlash Builder
                 | CBlockComment
                 | CBlockCommentAsterisk
                 | CLineComment
  deriving (Show)

-- This instance counts the number of characters emitted.
instance IotaS CParserTest Int where
  initStateS = (CData, 0)

  -- example of special behavior:
  parseIotaS CData 10 =
        anyChar       .>= writeTextI "/*ten*/" (CData, (+3))
    <|> endI Terminal |>> writeTextI "/*ten*/" CData

  parseIotaS CData _ =
        char '/'      .>> bufferI CForwardSlash
    <|> anyChar       .>= emitI (CData, (+1))
    <|> endI Terminal |>> ignoreI CData

  parseIotaS (CForwardSlash buffer) _ =
        char '*'      .>> ignoreI CBlockComment
    <|> char '/'      .>> ignoreI CLineComment
    <|> otherwiseI    |>= prependI buffer (CData, (+1))

  parseIotaS CBlockComment _ =
        char '*'      .>> ignoreI CBlockCommentAsterisk
    <|> anyChar       .>> ignoreI CBlockComment
    <|> otherwiseI    |>> ignoreI CData

  parseIotaS CBlockCommentAsterisk _ =
        char '/'      .>> ignoreI CData
    <|> anyChar       .>> ignoreI CBlockComment
    <|> otherwiseI    |>> ignoreI CData

  parseIotaS CLineComment _ =
        string "\r\n" +>> emitI CData
    <|> char   '\n'   .>> emitI CData
    <|> anyChar       .>> ignoreI CLineComment
    <|> otherwiseI    |>> ignoreI CData

data HaskellParserTest = HData
                       | HBlockComment HaskellParserTest
                       | HLineComment HaskellParserTest
                       | HQuotedC (IotaResultS CParserTest Int)
  deriving (Show)

instance IotaS HaskellParserTest [Text] where
  initStateS = (HData, ["Data"])

  parseIotaS HData _ =
        string "--"   +>= ignoreI (HLineComment HData, ("Line Comment" :))
    <|> string "{-"   +>= ignoreI (HBlockComment HData, ("Block Comment" :))
    <|> string "[C|"  +>= emitI (HQuotedC initIotaS, ("Quoted C" :))
    <|> anyChar       .>> emitI HData
    <|> endI Terminal |>= ignoreI (HData, ("End" :))

  parseIotaS (HBlockComment prior) _ =
        string "-}"   +>= ignoreI (prior, ("Data" :))
    <|> anyChar       .>> ignoreI (HBlockComment prior)
    <|> endI Reparse  |>> ignoreI prior

  parseIotaS (HLineComment prior) _ =
        string "\r\n" +>= emitI (prior, ("Data" :))
    <|> char   '\n'   .>= emitI (prior, ("Data" :))
    <|> anyChar       .>> ignoreI (HLineComment prior)
    <|> endI Reparse  |>> ignoreI prior

  parseIotaS p@(HQuotedC cparser) _ =
        string "\\]"  +>> substI "]" (feedInnerS cparser HQuotedC)
    <|> string "{-"   +>= ignoreI (HBlockComment p, ("Block Comment" :))
    <|> string "--"   +>= ignoreI (HLineComment p, ("Line Comment" :))
    <|> char ']'      .>= closeInnerS cparser (HData, ("Data" :))
    <|> anyChar       .>> feedInnerS cparser HQuotedC
    <|> endI Reparse  |>> substI "]" (closeInnerS cparser HData)

type HaskellParser = (HaskellParserTest, [Text])

main = do
  x <- T.getLine
  print $ iotaS (initStateS :: HaskellParser) x