{-# LANGUAGE OverloadedStrings #-}
module Data.Iota.Tests.Text
 where

import Blaze.ByteString.Builder
import Data.Attoparsec.Text
import Data.Iota.Text

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
    <|> endI Terminal |> ignoreI CData

  parseIota (CForwardSlash buffer) =
        char '*'      .> ignoreI CBlockComment
    <|> char '/'      .> ignoreI CLineComment
    <|> anyChar       .> prependI buffer CData
    <|> otherwiseI    |> prependI buffer CData

  parseIota CBlockComment =
        char '*'      .> ignoreI CBlockCommentAsterisk
    <|> anyChar       .> ignoreI CBlockComment
    <|> otherwiseI    |> ignoreI CData

  parseIota CBlockCommentAsterisk =
        char '/'      .> ignoreI CData
    <|> anyChar       .> ignoreI CBlockComment
    <|> otherwiseI    |> ignoreI CData

  parseIota CLineComment =
        string "\r\n" +> emitI CData
    <|> char   '\n'   .> emitI CData
    <|> anyChar       .> ignoreI CLineComment
    <|> otherwiseI    |> ignoreI CData

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
    <|> endI Terminal |> ignoreI HData

  parseIota (HBlockComment prior) =
        string "-}"   +> ignoreI prior
    <|> anyChar       .> ignoreI (HBlockComment prior)
    <|> endI Reparse  |> ignoreI prior

  parseIota (HLineComment prior) =
        string "\r\n" +> emitI prior
    <|> char   '\n'   .> emitI prior
    <|> anyChar       .> ignoreI (HLineComment prior)
    <|> endI Reparse  |> ignoreI prior

  parseIota p@(HQuotedC cparser) =
        string "\\]"  +> substI "]" (feedInner cparser HQuotedC)
    <|> string "{-"   +> ignoreI (HBlockComment p)
    <|> string "--"   +> ignoreI (HLineComment p)
    <|> char ']'      .> closeInner cparser HData
    <|> anyChar       .> feedInner cparser HQuotedC
    <|> endI Reparse  |> substI "]" (closeInner cparser HData)

data FailParserTest = FailData
                    | FailComment
  deriving (Show)

instance Iota FailParserTest where
  parseIota FailData =
        otherwiseI   |> substI "fail" (emitI FailComment)

  parseIota FailComment =
        anyChar      .> substI "comment" (emitI FailData)