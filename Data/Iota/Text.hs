{-# LANGUAGE OverloadedStrings, UnboxedTuples, LiberalTypeSynonyms #-}
module Data.Iota.Text
    ( -- Type Classes
      Iota(..)
      -- Data Types
    , IotaResult
      -- Operators
    , (.>), (+>), (|>)
      -- Functions
    , runIota, feedIota, closeIota, iota
    -- Combinators
    , emitI, bufferI, bufferTextI, emitAndBufferI
    , ignoreI
    , prependI, writeI, appendI
    , substI
    , embedInner, feedInner, closeInner
    , endI, otherwiseI
    , IotaEndState(..)
    -- Exports
    , (<|>)
    )
 where

import Control.Applicative
import Control.Arrow
import Control.Monad.Writer.Strict
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8

-- Iota is an incremental, buffered parser library

instance Show Builder where
  show = show . toByteString

class (Show a) => Iota a where
  parseIota :: a -> Parser (IotaEndState, Writer Builder a)

data IotaEndState = Terminal | Reparse
  deriving (Show)

type IotaResult a = (Builder, a, [Text])

emitI :: s -> Text -> Writer Builder s
emitI s t = tell (fromText t) >> return s
{-# INLINE emitI #-}

bufferI :: (Builder -> s) -> Text -> Writer Builder s
bufferI s t = return . s $ fromText t
{-# INLINE bufferI #-}

bufferTextI :: (Text -> s) -> Text -> Writer Builder s
bufferTextI s t = return (s t)
{-# INLINE bufferTextI #-}

emitAndBufferI :: (Text -> s) -> Text -> Writer Builder s
emitAndBufferI s t = tell (fromText t) >> return (s t)
{-# INLINE emitAndBufferI #-}

ignoreI :: s -> Text -> Writer Builder s
ignoreI s _ = return s
{-# INLINE ignoreI #-}

prependI :: Builder -> s -> Text -> Writer Builder s
prependI b s t = tell (b <> fromText t) >> return s
{-# INLINE prependI #-}

writeI :: Text -> s -> Text -> Writer Builder s
writeI b s _ = tell (fromText b) >> return s
{-# INLINE writeI #-}

appendI :: Text -> s -> Text -> Writer Builder s
appendI b s t = tell (fromText (t <> b)) >> return s
{-# INLINE appendI #-}

substI :: (Show s) => Text -> (Text -> Writer Builder s) -> Text -> Writer Builder s
substI c f = const (f c)
{-# INLINE substI #-}

embedInner :: (Iota b) => b -> (IotaResult b -> s) -> Text -> Writer Builder s
embedInner i s t = tell o >> return (s (flush, a, b))
  where (o, a, b) = runIota i t
{-# INLINE embedInner #-}

feedInner :: (Iota b) => IotaResult b -> (IotaResult b -> s) -> Text -> Writer Builder s
feedInner i s t = tell o >> return (s (flush, a, b))
  where (o, a, b) = feedIota i t
{-# INLINE feedInner #-}

closeInner :: (Iota b) => IotaResult b -> s -> Text -> Writer Builder s
closeInner i s t = tell o >> tell (fromText t) >> return s
  where (o, _) = closeIota i
{-# INLINE closeInner #-}

(+>) :: Parser Text -> (Text -> Writer Builder s) -> Parser (IotaEndState, Writer Builder s)
(+>) t p = fmap (\ x -> (Reparse, p x)) t
{-# INLINE (+>) #-}

(.>) :: Parser Char -> (Text -> Writer Builder s) -> Parser (IotaEndState, Writer Builder s)
(.>) t p = fmap ((\ x -> (Reparse, p x)) . T.singleton) t
{-# INLINE (.>) #-}

(|>) :: Parser IotaEndState -> (Text -> Writer Builder s) -> Parser (IotaEndState, Writer Builder s)
(|>) t p = fmap (\x -> (x, p T.empty)) t
{-# INLINE (|>) #-}

endI :: IotaEndState -> Parser IotaEndState
endI s = endOfInput >> return s
{-# INLINE endI #-}

otherwiseI :: Parser IotaEndState
otherwiseI = return Reparse
{-# INLINE otherwiseI #-}

---- Repeatedly parse a chunk of text until a partial result is reached

incrIota :: (Iota s) => (Builder, s, [Text]) -> (Builder, s, [Text])
incrIota i@(o, s, b:bs) = result
   where
     result = case fmap (second runWriter) $ parse (parseIota s) b of
               Done "" (Terminal, (a, w)) -> (o <> w, a, bs)
               Done l  (_, (a, w))        -> incrIota (o <> w, a, l:bs)
               Partial _                  -> case bs of
                                               b':r -> incrIota (o, s, (b<>b'):r)
                                               _    -> i
               Fail {}                    -> error "Parsers must be total, add parse rules to ensure the parser is total."
incrIota i = i

runIota :: (Iota a) => a -> Text -> (Builder, a, [Text])
runIota a i = incrIota (flush, a, [i])
{-# INLINE runIota #-}

-- Feed additional input

feedIota :: (Iota a) => (Builder, a, [Text]) -> Text -> (Builder, a, [Text])
feedIota (o, a, b) t = incrIota (o, a, b++[t])
{-# INLINE feedIota #-}

-- Close the parser out returning a final chunk

closeIota :: (Iota a) => (Builder, a, [Text]) -> (Builder, a)
closeIota (o, s, bs) = 
  case incrIota (o, s, [T.concat bs]) of
    (o, s, b) ->
      case fmap (second runWriter) $ feed (parse (parseIota s) (T.concat b)) "" of
            Done "" (Terminal, (a, w)) -> (o <> w, a)
            Done l  (_, (a, w))        -> closeIota (o <> w, a, [l])
            Partial _                  -> error "Parsers must be total, add parse rules to ensure the parser is total." 
            Fail {}                    -> error "Parsers must be total, add parse rules to ensure the parser is total."

-- Simplified parser.

iota :: (Iota a) => a -> Text -> (Builder, a)
iota a t = closeIota $ runIota a t
{-# INLINE iota #-}