{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Scanf
    ( (:+)(..)
    , (|$), (|.), (|<$>), apply
    , fmt
    , scanf
    ) where

import Data.ByteString.Char8 (ByteString)
import Data.Kind
import FlatParse.Basic
import Language.Haskell.TH
import Language.Haskell.TH.Quote

infixr 1 :+
data a :+ b = a :+ b deriving (Show)

class Apply f xs where
    type Ret f xs :: Data.Kind.Type
    apply :: f -> xs -> Ret f xs

instance Apply r () where
    type Ret r () = r
    apply r () = r

instance (Apply r xs, a ~ x) => Apply (a -> r) (x :+ xs) where
    type Ret (a -> r) (x :+ xs) = Ret r xs
    apply f (x :+ xs) = apply (f x) xs

infixl 4 |$
(|$) :: Apply f a => f -> a -> Ret f a
a |$ b = apply a b

infixl 4 |.
(|.) :: Apply f b => f -> (a -> b) -> a -> Ret f b
a |. b = apply a . b

infixl 4 |<$>
(|<$>) :: (Functor f, Apply g a) => g -> f a -> f (Ret g a)
a |<$> b = apply a <$> b


formatString :: String -> Q Exp
formatString "" = [|pure ()|]
formatString ('%' : s1) =
    case s1 of
      'c' : s' -> [|(:+) <$> anyChar <*> $(formatString s')|]
      'd' : s' -> [|(:+) <$> anyAsciiDecimalInt <*> $(formatString s')|]
      'f' : s' -> [|(:+) <$> anyChar <*> $(formatString s')|]
      'l' : s' -> [|(:+) <$> anyAsciiDecimalInteger <*> $(formatString s')|]
      's' : s' -> [|(:+) <$> byteStringOf (some $ satisfy $ not . isSpace) <*> $(formatString s')|]
      'u' : s' -> [|(:+) <$> anyAsciiDecimalWord <*> $(formatString s')|]
      'x' : s' -> [|(:+) <$> anyAsciiHexInt <*> $(formatString s')|]
      '%' : s' -> [|$(char '%') >> $(formatString s')|]
      _ -> error "Invalid format string"
formatString s =
    let (s0, s') = break (== '%') s
    in [|$(string s0) >> $(formatString s')|]

fmt :: QuasiQuoter
fmt = QuasiQuoter
      { quoteExp = formatString
      , quotePat = undefined
      , quoteType = undefined
      , quoteDec = undefined
      }

-- Could report errors better.
scanf :: Parser e t -> ByteString -> t
scanf p input = case runParser p input of
                  OK a _ -> a
                  Fail -> error "Fail"
                  Err _ -> error "Error"
