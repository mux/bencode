{-# LANGUAGE OverloadedStrings #-}
module Data.Bencode
  ( BData(..)
  , bdecode
  , bdecode_
  ) where

import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString.Char8 hiding (reverse)
import Prelude hiding (null, head, tail, length, splitAt)

data BData = BInteger Integer
           | BString ByteString
           | BList [BData]
           | BDict (Map ByteString BData)
  deriving (Eq, Ord, Show)

bdecode_ :: ByteString -> Maybe BData
bdecode_ bytes = fst `fmap` bdecode bytes

bdecode :: ByteString -> Maybe (BData, ByteString)
bdecode bytes = guard (not (null bytes)) >> bdecode' bytes
  where bdecode' s = case head s of
                       'i' -> parse_int (tail s)
                       'l' -> parse_list (tail s) []
                       'd' -> parse_dict (tail s) M.empty
                       _   -> parse_str s
        parse_int s = do (num, end) <- readInteger s
                         guard ("e" `isPrefixOf` end)
                         return (BInteger num, tail end)
        parse_str s = do (len, rest)  <- readInt s
                         (sep, rest') <- uncons rest
                         guard (len >= 0 && sep == ':' && length rest' >= len)
                         let (str, end) = splitAt len rest'
                         return (BString str, end)
        parse_list s xs
          | "e" `isPrefixOf` s = return (BList (reverse xs), tail s)
          | otherwise          = do (x, rest) <- bdecode' s
                                    parse_list rest (x:xs)
        parse_dict s d
          | "e" `isPrefixOf` s = return (BDict d, tail s)
          | otherwise          = do (BString key, rest) <- parse_str s
                                    (val, end) <- bdecode rest
                                    parse_dict end (M.insert key val d)
