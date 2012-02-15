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
  , parseBencode
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
  gfromBencode tr (BDict d) = do x <- M.lookup (tr key) d
                                 M1 <$> gfromBencode tr x
    where key = B.pack $ selName (undefined :: t s a p)
  gfromBencode _ _          = Nothing

instance GFromBencode a => GFromBencode (C1 i a) where
  gfromBencode tr bdata = M1 <$> gfromBencode tr bdata

instance GFromBencode a => GFromBencode (D1 i a) where
  gfromBencode tr bdata = M1 <$> gfromBencode tr bdata

instance (GFromBencode a, GFromBencode b) => GFromBencode (a :*: b) where
  gfromBencode tr d = (:*:) <$> gfromBencode tr d <*> gfromBencode tr d

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

parseBencode :: FromBencode a => ByteString -> Maybe a
parseBencode str = bdecode_ str >>= fromBencode
