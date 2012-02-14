{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import GHC.Generics
import System.Environment
import Data.Bencode
import Data.Bencode.Generic

data Torrent =
  Torrent { announce :: ByteString
          , info     :: TorrentInfo
          } deriving (Show, Generic)

data TorrentInfo =
  TorrentInfo { files :: [File]
              } deriving (Show, Generic)

data File =
  File { path   :: [ByteString]
       , length :: Integer
       } deriving (Show, Generic)

instance FromBencode Torrent
instance FromBencode TorrentInfo
instance FromBencode File

main :: IO ()
main = do
  (file:_)       <- getArgs
  Just (bdata,_) <- bdecode <$> B.readFile file
  let Just torrent = fromBencode bdata
  forM_ (files (info torrent)) $ \(File pcs size) -> do
    B.putStr (B.intercalate "/" pcs)
    B.putStr " "
    B.putStrLn (B.pack (show size))
