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
  , AccessorMap(..)
  ) where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import GHC.Generics
import Data.Bencode

-- The Bencode format specification allows lists to be heterogenous;
-- it is not possible to use this interface with those yet.
class GFromBencode f where
  gfromBencode :: (ByteString -> ByteString) -> BData -> Maybe (f a)

instance FromBencode a => GFromBencode (K1 i a) where
  gfromBencode _ bdata = K1 <$> fromBencode bdata

instance (Selector s, GFromBencode a) => GFromBencode (S1 s a) where
  gfromBencode fm (BDict d) = do x <- M.lookup (fm key) d
                                 M1 <$> gfromBencode fm x
    where key = B.pack $ selName (undefined :: t s a p)
  gfromBencode _ _          = Nothing

instance GFromBencode a => GFromBencode (C1 i a) where
  gfromBencode fm bdata = M1 <$> gfromBencode fm bdata

instance GFromBencode a => GFromBencode (D1 i a) where
  gfromBencode fm bdata = M1 <$> gfromBencode fm bdata

instance (GFromBencode a, GFromBencode b) => GFromBencode (a :*: b) where
  gfromBencode fm d = (:*:) <$> gfromBencode fm d <*> gfromBencode fm d

newtype AccessorMap a = AM { unWrap :: ByteString -> ByteString }

class FromBencode a where
  fromBencode :: BData -> Maybe a

  accessorMap :: AccessorMap a
  accessorMap = AM id

  default fromBencode :: (Generic a, GFromBencode (Rep a)) => BData -> Maybe a
  fromBencode bdata = to <$> gfromBencode trfn bdata
    where trfn = unWrap (accessorMap :: AccessorMap a)

instance FromBencode Integer where
  fromBencode (BInteger x) = Just x
  fromBencode _            = Nothing

instance FromBencode ByteString where
  fromBencode (BString xs) = Just xs
  fromBencode _            = Nothing

instance FromBencode a => FromBencode [a] where
  fromBencode (BList xs) = mapM fromBencode xs
  fromBencode _          = Nothing
