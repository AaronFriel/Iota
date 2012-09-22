{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
module Data.Iota.Tests.Text
 where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Internal.Buffer
import Data.Attoparsec.Text
import Data.Iota.Monad.Text
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

instance (Show a) => Show (Identity a) where
  show = show . runIdentity

-- This instance counts the number of characters emitted.
instance IotaM CParserTest Identity Int where
  initStateM = (CData, Identity 0)

  -- example of special behavior:
  parseIotaM CData (Identity 10) =
        anyChar       .>= writeTextI "/*ten*/" (CData, return . (+3))
    <|> endI Terminal |>> writeTextI "/*ten*/" CData

  parseIotaM CData _ =
        char '/'      .>> bufferI CForwardSlash
    <|> anyChar       .>= emitI (CData, return . (+1))
    <|> endI Terminal |>> ignoreI CData

  parseIotaM (CForwardSlash buffer) _ =
        char '*'      .>> ignoreI CBlockComment
    <|> char '/'      .>> ignoreI CLineComment
    <|> otherwiseI    |>= prependI buffer (CData, return . (+1))

  parseIotaM CBlockComment _ =
        char '*'      .>> ignoreI CBlockCommentAsterisk
    <|> anyChar       .>> ignoreI CBlockComment
    <|> otherwiseI    |>> ignoreI CData

  parseIotaM CBlockCommentAsterisk _ =
        char '/'      .>> ignoreI CData
    <|> anyChar       .>> ignoreI CBlockComment
    <|> otherwiseI    |>> ignoreI CData

  parseIotaM CLineComment _ =
        string "\r\n" +>> emitI CData
    <|> char   '\n'   .>> emitI CData
    <|> anyChar       .>> ignoreI CLineComment
    <|> otherwiseI    |>> ignoreI CData

data HaskellParserTest = HData
                       | HBlockComment HaskellParserTest
                       | HLineComment HaskellParserTest
                       | HQuotedC (IotaResultM CParserTest Identity Int)
  deriving (Show)

instance (Monoid a) => IotaM HaskellParserTest Identity a where
  initStateM = (HData, Identity mempty)

  parseIotaM HData _ =
        string "--"   +>> ignoreI (HLineComment HData)
    <|> string "{-"   +>> ignoreI (HBlockComment HData)
    <|> string "[C|"  +>> emitI (HQuotedC initIotaM)
    <|> anyChar       .>> emitI HData
    <|> endI Terminal |>> ignoreI HData

  parseIotaM (HBlockComment prior) _ =
        string "-}"   +>> ignoreI prior
    <|> anyChar       .>> ignoreI (HBlockComment prior)
    <|> endI Reparse  |>> ignoreI prior

  parseIotaM (HLineComment prior) _ =
        string "\r\n" +>> emitI prior
    <|> char   '\n'   .>> emitI prior
    <|> anyChar       .>> ignoreI (HLineComment prior)
    <|> endI Reparse  |>> ignoreI prior

  parseIotaM p@(HQuotedC cparser) _ =
        string "\\]"  +>> substI "]" (feedInnerM cparser HQuotedC)
    <|> string "{-"   +>> ignoreI (HBlockComment p)
    <|> string "--"   +>> ignoreI (HLineComment p)
    <|> char ']'      .>> closeInnerM cparser HData
    <|> anyChar       .>> feedInnerM cparser HQuotedC
    <|> endI Reparse  |>> substI "]" (closeInnerM cparser HData)

type HaskellParser a = (HaskellParserTest, Identity a)

main = do
  x <- T.getLine
  print $ iotaM (initStateM :: HaskellParser String) x