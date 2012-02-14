{-# LANGUAGE
      DeriveGeneric,
      DefaultSignatures,
      TypeOperators,
      FlexibleInstances,
      FlexibleContexts,
      ScopedTypeVariables
 #-}
module Data.Bencode.Generic
  ( FromBencode(..)
  ) where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import GHC.Generics
import Data.Bencode

class GFromBencode f where
  gfromBencode :: BData -> Maybe (f a)

instance FromBencode a => GFromBencode (K1 i a) where
  gfromBencode bdata = K1 <$> fromBencode bdata

instance (Selector s, GFromBencode a) => GFromBencode (S1 s a) where
  gfromBencode (BDict d) = do x <- M.lookup (B.pack key) d
                              M1 <$> gfromBencode x
    where key = selName (undefined :: t s a p)
  gfromBencode _         = Nothing

instance GFromBencode a => GFromBencode (C1 i a) where
  gfromBencode bdata = M1 <$> gfromBencode bdata

instance GFromBencode a => GFromBencode (D1 i a) where
  gfromBencode bdata = M1 <$> gfromBencode bdata

instance (GFromBencode a, GFromBencode b) => GFromBencode (a :*: b) where
  gfromBencode d = (:*:) <$> gfromBencode d <*> gfromBencode d

class FromBencode a where
  fromBencode :: BData -> Maybe a

  default fromBencode :: (Generic a, GFromBencode (Rep a)) => BData -> Maybe a
  fromBencode = (to <$>) . gfromBencode

instance FromBencode Integer where
  fromBencode (BInteger x) = Just x
  fromBencode _            = Nothing

instance FromBencode ByteString where
  fromBencode (BString xs) = Just xs
  fromBencode _            = Nothing

instance FromBencode a => FromBencode [a] where
  fromBencode (BList xs) = mapM fromBencode xs
  fromBencode _          = Nothing
