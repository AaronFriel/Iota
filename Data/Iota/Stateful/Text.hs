{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Data.Iota.Stateful.Text
    ( -- Type Classes
      IotaS(..)
      -- Data Types
    , IotaResultS
      -- Operators
    , (.>=), (+>=), (|>=)
    , (.>>), (+>>), (|>>)
      -- Combinators
    , feedInnerS, closeInnerS
      -- Functions
    , initIotaS, runIotaS, feedIotaS, closeIotaS, iotaS
    -- Exports
    , module Data.Iota.Text
    )
 where

import Prelude
import Control.Arrow
import Control.Monad.Writer.Strict
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8

import Data.Iota.Text

-- Iota is an incremental, buffered parser library

class IotaS a b where
  parseIotaS :: a -> b -> Parser (IotaEndState, Writer Builder (a, b -> b))
  initStateS :: (a, b)

type IotaResultS a b = (Builder, a, b, [Text])

-- Additional combinators

feedInnerS :: (IotaS a' b') => IotaResultS a' b' -> (IotaResultS a' b' -> a) -> Text -> Writer Builder a
feedInnerS s' s i = tell o >> return (s (flush, a, b, t))
  where (o, a, b, t) = feedIotaS s' i
{-# INLINE feedInnerS #-}

closeInnerS :: (IotaS a' b') => IotaResultS a' b' -> a -> Text -> Writer Builder a
closeInnerS i s t = tell o >> tell (fromText t) >> return s
  where (o, _, _) = closeIotaS i
{-# INLINE closeInnerS #-}

(+>=) :: Parser Text -> (Text -> Writer Builder (a, b -> b)) -> Parser (IotaEndState, Writer Builder (a, b -> b))
(+>=) t p = fmap (\x -> (Reparse, p x)) t
{-# INLINE (+>=) #-}

(.>=) :: Parser Char -> (Text -> Writer Builder (a, b -> b)) -> Parser (IotaEndState, Writer Builder (a, b -> b))
(.>=) t p = fmap ((\x -> (Reparse, p x)) . T.singleton) t
{-# INLINE (.>=) #-}

(|>=) :: Parser IotaEndState -> (Text -> Writer Builder (a, b -> b)) -> Parser (IotaEndState, Writer Builder (a, b -> b))
(|>=) t p = fmap (\x -> (x, p T.empty)) t
{-# INLINE (|>=) #-}

(+>>) :: Parser Text -> (Text -> Writer Builder a) -> Parser (IotaEndState, Writer Builder (a, b -> b))
(+>>) t p = fmap (\x -> (Reparse, fmap (\a -> (a, id)) (p x))) t
{-# INLINE (+>>) #-}

(.>>) :: Parser Char -> (Text -> Writer Builder a) -> Parser (IotaEndState, Writer Builder (a, b -> b))
(.>>) t p = fmap (\x -> (Reparse, fmap (\a -> (a, id)) (p $ T.singleton x))) t
{-# INLINE (.>>) #-}

(|>>) :: Parser IotaEndState -> (Text -> Writer Builder a) -> Parser (IotaEndState, Writer Builder (a, b -> b))
(|>>) t p = fmap (\x -> (x, fmap (\a -> (a, id)) (p T.empty))) t
{-# INLINE (|>>) #-}

---- Repeatedly parse a chunk of text until a partial result is reached

incrIota :: (IotaS a b) => IotaResultS a b -> IotaResultS a b
incrIota i@(o, a, b, t:ts) = result
   where
     result = case fmap (second runWriter) $ parse (parseIotaS a b) t of
               Done "" (Terminal, ((a', f), w)) -> (o <> w, a', f b, ts)
               Done l  (_, ((a', f), w))        -> incrIota (o <> w, a', f b, l:ts)
               Partial _                  -> case ts of
                                               t':r -> incrIota (o, a, b, (t<>t'):r)
                                               _    -> i
               Fail {}                    -> error "Parsers must be total, add parse rules to ensure the parser is total."
incrIota i = i
{-# INLINE incrIota #-}

runIotaS :: (IotaS a b) => (a, b) -> Text -> IotaResultS a b
runIotaS (a, b) i = incrIota (flush, a, b, [i])
{-# INLINE runIotaS #-}

initIotaS :: (IotaS a b) => IotaResultS a b
initIotaS = runIotaS initStateS ""
{-# INLINE initIotaS #-}

-- Feed additional input

feedIotaS :: (IotaS a b) => IotaResultS a b -> Text -> IotaResultS a b
feedIotaS (o, a, b, t) t' = incrIota (o, a, b, t++[t'])
{-# INLINE feedIotaS #-}

-- Close the parser out returning a final chunk

closeIotaS :: (IotaS a b) => IotaResultS a b -> (Builder, a, b)
closeIotaS (o, a, b, ts) =
  case incrIota (o, a, b, [T.concat ts]) of
    (o', a', b', t) ->
      case fmap (second runWriter) $ feed (parse (parseIotaS a' b') (T.concat t)) "" of
            Done "" (Terminal, ((a'', f), w)) -> (o' <> w, a'', f b)
            Done l  (_, ((a'', f), w))        -> closeIotaS (o' <> w, a'', f b, [l])
            Partial _                  -> error "Parsers must be total, add parse rules to ensure the parser is total."
            Fail {}                    -> error "Parsers must be total, add parse rules to ensure the parser is total."
{-# INLINE closeIotaS #-}

-- Simplified parser.

iotaS :: (IotaS a b) => (a, b) -> Text -> (Builder, a, b)
iotaS s t = closeIotaS $ runIotaS s t
{-# INLINE iotaS #-}