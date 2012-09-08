{-# LANGUAGE OverloadedStrings, UnboxedTuples, LiberalTypeSynonyms #-}
module Data.Iota.Text
    ( -- Type Classes
      Iota(..)
      -- Data Types
    , IotaResult
      -- Operators
    , (.>), (+>)
      -- Functions
    , runIota, feedIota, closeIota, iota
    -- Combinators
    , emitI, bufferI, bufferTextI, emitAndBufferI
    , ignoreI
    , prependI, writeI, appendI
    , substI
    , embedInner, feedInner, closeInner
    , endI, failI
    , IotaEndState(..)
    -- Exports
    , (<|>)
    )
 where

import Control.Applicative
import Control.Arrow
import Control.Monad.Writer.Strict
import Data.Attoparsec.Text
import Data.Monoid
import Data.Text

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8

import Debug.Trace

-- Iota is an incremental, buffered parser library

-- LIBRARY CODE

instance Show Builder where
  show = show . toByteString

class (Show a) => Iota a where
  parseIota :: a -> IotaParser a
  closeState :: a -> Text -> Builder

  defaultState :: a

type IotaWriter a = Writer Builder a
type IotaParser a = Parser (IotaWriter a)

type IotaState a  = (a, Text)

data IotaResult a = Awaiting !Builder !(IotaState a, Text -> Result (a, Builder))
                  | Complete !Builder

data IotaEndState = Continue | Reparse | End

getBuffer :: IotaResult a -> Builder
getBuffer (Awaiting b _) = b
getBuffer (Complete b)   = b

instance Show a => Show (IotaResult a) where
  show (Awaiting b ((a, b1), f)) = "Awaiting " <> show b -- <> " (" <> show a <> ", " <> show b1 <> ")"
  show (Complete b) = "Complete " <> show b

-- Combinators

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
ignoreI s t = return s
{-# INLINE ignoreI #-}

prependI :: Builder -> s -> Text -> Writer Builder s
prependI b s t = tell (b <> fromText t) >> return s
{-# INLINE prependI #-}

writeI :: Text -> s -> Text -> Writer Builder s
writeI b s t = tell (fromText b) >> return s
{-# INLINE writeI #-}

appendI :: Text -> s -> Text -> Writer Builder s
appendI b s t = tell (fromText (t <> b)) >> return s
{-# INLINE appendI #-}

substI :: Text -> (Text -> Writer Builder s) -> Text -> Writer Builder s
substI c f = const (f c)
{-# INLINE substI #-}

embedInner :: (Iota b) => b -> (IotaResult b -> s) -> Text -> Writer Builder s
embedInner i s t = tell (getBuffer p) >> return (s p)
  where p = runIota i t
{-# INLINE embedInner #-}

feedInner :: (Iota b) => IotaResult b -> (IotaResult b -> s) -> Text -> Writer Builder s
feedInner i s t = tell (getBuffer p) >> return (s p)
  where p = feedIota i t
{-# INLINE feedInner #-}

closeInner :: (Iota b) => IotaResult b -> s -> Text -> Writer Builder s
closeInner i s t = tell b >> tell (fromText t) >> return s
  where b = closeIota $ feedIota i ""
{-# INLINE closeInner #-}

(+>) :: Parser Text -> (Text -> Writer Builder s) -> IotaParser s
(+>) t p = (fmap p) t
{-# INLINE (+>) #-}

(.>) :: Parser Char -> (Text -> Writer Builder s) -> IotaParser s
(.>) t p = (fmap p) (fmap singleton t)
{-# INLINE (.>) #-}

--(|>) :: (Iota s) => Parser () -> Text -> IotaParser s
--(|>) _ p = return $ tell (fromText p) >> return defaultState
--{-# INLINE (|>) #-}

endI :: (Iota s) => IotaEndState -> (Text -> Writer Builder s) -> IotaParser s
endI e p = endOfInput >> failI e p
{-# INLINE endI #-}

failI :: (Iota s) => IotaEndState -> (Text -> Writer Builder s) -> IotaParser s
failI Reparse p = return $ do
  x <- p ""
  let r = runIota x ""
      r' = closeIota r
  tell (getBuffer r)
  tell r'
  return defaultState
failI End p = (pure "") +> p
{-# INLINE failI #-}


-- (|]) :: (Iota s) => t -> (Text -> Writer Builder s) -> IotaParser s
-- (|]) _ p = pure "" +> p

-- Parse one piece of text producing an attoparsec result

incrIota :: (Iota a) => a -> Text -> (IotaState a, Result (a, Builder))
incrIota s i = ((s, i), fmap runWriter $ parse (parseIota s) i)
{-# INLINE incrIota #-}

-- Repeatedly parse a chunk of text until a partial result is reached

foldIota :: (Iota a) => (IotaState a, Result (a, Builder)) -> IotaWriter (IotaState a, Text -> Result (a, Builder))
foldIota = go
  where
    --go (t, Done "" (s, b))        = return (s, )
    go (t, Done leftover (s, b)) = tell b >> go (incrIota s leftover)

    go (t, Fail leftover _ _)    = go $ incrIota defaultState leftover
    go (t, Partial f)            = return (t, f)
{-# INLINE foldIota #-}

 -- Wrapper around foldIota returning the IotaResult data type

runIota :: (Iota a) => a -> Text -> IotaResult a
runIota s i =
    Awaiting buffer next
  where
    (next, buffer) = runWriter $ foldIota (incrIota s i)
{-# INLINE runIota #-}

-- Feed additional input

feedIota :: (Iota a) => IotaResult a -> Text -> IotaResult a
feedIota iotaResult input =
  case iotaResult of
    Awaiting buffer ((s, t), next) -> let (n, b) = runWriter $ foldIota ((s, t <> input), next input) in Awaiting b n
    Complete buffer                -> let (n, b) = runWriter $ foldIota ((defaultState, input), Done input (defaultState, flush)) in Awaiting b n
{-# INLINE feedIota #-}

-- Close the parser out returning a final chunk

closeIota :: (Iota a) => IotaResult a -> Builder
closeIota iotaResult =
  case iotaResult of
    Awaiting buffer ((s, t), next) ->
      case feed (Partial next) "" of
        Done _ (_, b) -> b
        _              -> error "Iota parser is not total."
    Complete buffer                -> flush
{-# INLINE closeIota #-}

-- Simplified parser.

iota :: (Iota a) => a -> Text -> Builder
iota s t = let f = runIota s t in getBuffer f <> closeIota f
{-# INLINE iota #-}