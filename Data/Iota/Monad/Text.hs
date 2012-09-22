{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Data.Iota.Monad.Text
    ( -- Type Classes
      IotaM(..)
      -- Data Types
    , IotaResultM
      -- Operators
    , (.>=), (+>=), (|>=)
    , (.>>), (+>>), (|>>)
      -- Combinators
    , feedInnerM, closeInnerM
      -- Functions
    , initIotaM, runIotaM, feedIotaM, closeIotaM, iotaM
    -- Exports
    , module Data.Iota.Text
    )
 where

import Prelude
import Control.Applicative
import Control.Arrow
import Control.Monad.Writer.Strict
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8

import Data.Iota.Text

-- Iota is an incremental, buffered parser library

class (Monad m) => IotaM a m b where
  parseIotaM :: (Monad m) => a -> m b -> Parser (IotaEndState, Writer Builder (a, b -> m b))
  initStateM :: (a, m b)

type IotaResultM a m b = (Builder, a, m b, [Text])

-- Additional combinators

feedInnerM :: (IotaM a' m' b') => IotaResultM a' m' b' -> (IotaResultM a' m' b' -> a) -> Text -> Writer Builder a
feedInnerM s' s i = tell o >> return (s (flush, a, b, t))
  where (o, a, b, t) = feedIotaM s' i
{-# INLINE feedInnerM #-}

closeInnerM :: (IotaM a' m' b') => IotaResultM a' m' b' -> a -> Text -> Writer Builder a
closeInnerM i s t = tell o >> tell (fromText t) >> return s
  where (o, _, _) = closeIotaM i
{-# INLINE closeInnerM #-}

(+>=) :: (Monad m) => Parser Text -> (Text -> Writer Builder (a, b -> m b)) -> Parser (IotaEndState, Writer Builder (a, b -> m b))
(+>=) t p = fmap (\ x -> (Reparse, p x)) t
{-# INLINE (+>=) #-}

(.>=) :: (Monad m) => Parser Char -> (Text -> Writer Builder (a, b -> m b)) -> Parser (IotaEndState, Writer Builder (a, b -> m b))
(.>=) t p = fmap ((\ x -> (Reparse, p x)) . T.singleton) t
{-# INLINE (.>=) #-}

(|>=) :: (Monad m) => Parser IotaEndState -> (Text -> Writer Builder (a, b -> m b)) -> Parser (IotaEndState, Writer Builder (a, b -> m b))
(|>=) t p = fmap (\x -> (x, p T.empty)) t
{-# INLINE (|>=) #-}

(+>>) :: (Monad m) => Parser Text -> (Text -> Writer Builder a) -> Parser (IotaEndState, Writer Builder (a, b -> m b))
(+>>) t p = fmap (\ x -> (Reparse, fmap (\a -> (a, return)) (p x))) t
{-# INLINE (+>>) #-}

(.>>) :: (Monad m) => Parser Char -> (Text -> Writer Builder a) -> Parser (IotaEndState, Writer Builder (a, b -> m b))
(.>>) t p = fmap ((\x -> (Reparse, fmap (\a -> (a, return)) (p $ T.singleton x)))) t
{-# INLINE (.>>) #-}

(|>>) :: (Monad m) => Parser IotaEndState -> (Text -> Writer Builder a) -> Parser (IotaEndState, Writer Builder (a, b -> m b))
(|>>) t p = fmap (\x -> (x, fmap (\a -> (a, return)) (p T.empty))) t
{-# INLINE (|>>) #-}

---- Repeatedly parse a chunk of text until a partial result is reached

incrIota :: (Monad m, IotaM a m b) => IotaResultM a m b -> IotaResultM a m b
incrIota i@(o, a, b, t:ts) = result
   where
     result = case fmap (second runWriter) $ parse (parseIotaM a b) t of
               Done "" (Terminal, ((a, f), w)) -> (o <> w, a, b >>= f, ts)
               Done l  (_, ((a, f), w))        -> incrIota (o <> w, a, b >>= f, l:ts)
               Partial _                  -> case ts of
                                               t':r -> incrIota (o, a, b, (t<>t'):r)
                                               _    -> i
               Fail {}                    -> error "Parsers must be total, add parse rules to ensure the parser is total."
incrIota i = i

runIotaM :: (Monad m, IotaM a m b) => (a, m b) -> Text -> IotaResultM a m b
runIotaM (a, b) i = incrIota (flush, a, b, [i])
{-# INLINE runIotaM #-}

initIotaM :: (Monad m, IotaM a m b) => IotaResultM a m b
initIotaM = runIotaM initStateM ""
{-# INLINE initIotaM #-}

-- Feed additional input

feedIotaM :: (Monad m, IotaM a m b) => IotaResultM a m b -> Text -> IotaResultM a m b
feedIotaM (o, a, b, t) t' = incrIota (o, a, b, t++[t'])
{-# INLINE feedIotaM #-}

-- Close the parser out returning a final chunk

closeIotaM :: (Monad m, IotaM a m b) => IotaResultM a m b -> (Builder, a, m b)
closeIotaM (o, a, b, ts) =
  case incrIota (o, a, b, [T.concat ts]) of
    (o, a, b, t) ->
      case fmap (second runWriter) $ feed (parse (parseIotaM a b) (T.concat t)) "" of
            Done "" (Terminal, ((a, f), w)) -> (o <> w, a, b >>= f)
            Done l  (_, ((a, f), w))        -> closeIotaM (o <> w, a, b >>= f, [l])
            Partial _                  -> error "Parsers must be total, add parse rules to ensure the parser is total."
            Fail {}                    -> error "Parsers must be total, add parse rules to ensure the parser is total."

-- Simplified parser.

iotaM :: (Monad m, IotaM a m b) => (a, m b) -> Text -> (Builder, a, m b)
iotaM s t = closeIotaM $ runIotaM s t
{-# INLINE iotaM #-}