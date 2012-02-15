{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import GHC.Generics
import System.Environment
import System.Exit
import System.FilePath
import Data.Bencode
import Data.Bencode.Generic

data Torrent =
  Torrent { announce  :: ByteString
          , comment   :: ByteString
          , createdBy :: ByteString
          , info      :: TorrentInfo
          } deriving (Show, Generic)

data TorrentInfo =
  TorrentInfo { files :: [File]
              } deriving (Show, Generic)

data File =
  File { path   :: [ByteString]
       , length :: Integer
       } deriving (Show, Generic)

instance FromBencode Torrent where
  accessorMap = AM (\f -> if f == "createdBy" then "created by" else f)

instance FromBencode TorrentInfo
instance FromBencode File

main :: IO ()
main = do
  (file:_) <- getArgs
  content  <- B.readFile file
  case bdecode_ content >>= fromBencode of
    Nothing      ->
      do B.putStrLn "Cannot parse torrent file"
         exitFailure
    Just torrent ->
      do B.putStrLn $ "Torrent name:\t" `B.append` (comment torrent)
         B.putStrLn $ "Announce URL:\t" `B.append` (announce torrent)
         B.putStrLn $ "Created by:\t" `B.append` (createdBy torrent)
         B.putStrLn ""
         forM_ (files (info torrent)) $ \(File pcs size) -> do
           B.putStrLn $ B.intercalate (B.singleton pathSeparator) pcs
                          `B.append` B.pack (' ':show size)
