{-# LANGUAGE OverloadedStrings #-}
module Data.Iota.Tests.Text
 where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Attoparsec.Text
import Data.Iota.Text
import Data.Monoid ((<>))
import Control.Monad.Writer.Strict
import Debug.Trace

data CParserTest = CData
                 | CForwardSlash Builder
                 | CBlockComment
                 | CBlockCommentAsterisk
                 | CLineComment
  deriving (Show)

instance Iota CParserTest where
  parseIota CData =
        char '/'      .> bufferI CForwardSlash
    <|> anyChar       .> emitI CData
    <|> endI End (ignoreI CData)

  parseIota (CForwardSlash buffer) =
        char '*'      .> ignoreI CBlockComment
    <|> char '/'      .> ignoreI CLineComment
    <|> anyChar       .> prependI buffer CData
    <|> endI End (prependI buffer CData)

  parseIota CBlockComment =
        char '*'      .> ignoreI CBlockCommentAsterisk
    <|> anyChar       .> ignoreI CBlockComment
    <|> endI End (ignoreI CData)

  parseIota CBlockCommentAsterisk =
        char '/'      .> ignoreI CData
    <|> anyChar       .> ignoreI CBlockComment
    <|> endI End (ignoreI CData)

  parseIota CLineComment =
        string "\r\n" +> emitI CData
     <|> char   '\n'   .> emitI CData
     <|> anyChar       .> ignoreI CLineComment
     <|> endI End (ignoreI CData)
    -- <|> failI End (substI "UTTER FAIL" (emitI CData))

  --closeState (CForwardSlash buffer) t = buffer <> fromText t
  closeState _ t = flush

  defaultState = CData

data HaskellParserTest = HData
                       | HBlockComment HaskellParserTest
                       | HLineComment HaskellParserTest
                       | HQuotedC (IotaResult CParserTest)
  deriving (Show)

instance Iota HaskellParserTest where
  parseIota HData =
        string "--"   +> ignoreI (HLineComment HData)
    <|> string "{-"   +> ignoreI (HBlockComment HData)
    <|> string "[C|"  +> embedInner CData HQuotedC
    <|> anyChar       .> emitI HData
    <|> endI End (ignoreI HData)

  parseIota (HBlockComment prior) =
        string "-}"   +> ignoreI prior
    <|> anyChar       .> ignoreI (HBlockComment prior)
    <|> endI Reparse (ignoreI prior)

  parseIota (HLineComment prior) =
        string "\r\n" +> emitI prior
    <|> char   '\n'   .> emitI prior
    <|> anyChar       .> ignoreI (HLineComment prior)
    <|> endI Reparse (ignoreI prior)

  parseIota p@(HQuotedC cparser) =
        string "\\]"  +> substI "]" (feedInner cparser HQuotedC)
    <|> string "{-"   +> ignoreI (HBlockComment p)
    <|> string "--"   +> ignoreI (HLineComment p)
    <|> char ']'      .> closeInner cparser HData
    <|> anyChar       .> feedInner cparser HQuotedC
    <|> endI End (substI "]" (closeInner cparser HData))

{-

 <a href=this onClick=""

 <script>document.<!-- -->location = "www.google.com"</script>
 <script src=""
-}

  --closeState (HLineComment p) t = closeState p t
  --closeState (HBlockComment p) t = closeState p t
  --closeState (HQuotedC c) t = (closeIota $ feedIota c t) <> (fromText "]")
  closeState _ t = flush

  defaultState = HData