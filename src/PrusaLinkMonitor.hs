{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module PrusaLinkMonitor
  ( PrusaLink (PrusaLink), prusaLinkNetrc ) where

import Xmobar
    ( Exec (alias, rate, run) )
import Data.Aeson ( FromJSON(parseJSON), eitherDecode, Value(..) )
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpBS )
import qualified Data.String as S8
import Network.HTTP.Client.TLS (applyDigestAuth, getGlobalManager)
import Data.Aeson.Types (ToJSON)
import Data.ByteString (fromStrict)
import GHC.Generics (Generic)
import Data.ByteString.Lazy (ByteString)
import Network.NetRc (readUserNetRc, nrHosts, NetRcHost (nrhName, nrhLogin, nrhPassword))
import Data.List (find)
import Data.ByteString.Char8 (unpack)
import Debug.Trace (traceShow, trace)

-- https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md
-- https://hackage.haskell.org/package/http-client-tls-0.3.6.3/docs/Network-HTTP-Client-TLS.html
data PrusaLink = PrusaLink String String String
  deriving (Show, Read)

data Status = Status
  { storage :: Maybe Storage
  , printer :: Printer
  , job     :: Maybe Job
  }
  deriving (Generic, Show)

data Storage = Storage
  { path :: String
  , name :: String
  , read_only :: Bool
  }
  deriving (Generic, Show)

data Job = Job
  { id :: Integer
  , progress :: Integer
  , time_remaining :: Integer
  , time_printing :: Integer
  }
  deriving (Generic, Show)

data Printer = Printer
  { state :: State
  , temp_bed :: Float
  , target_bed :: Float
  , temp_nozzle :: Float
  , target_nozzle :: Float
  , axis_x :: Maybe Float
  , axis_y :: Maybe Float
  , axis_z :: Maybe Float
  , flow   :: Float
  , speed  :: Float
  , fan_hotend :: Float
  , fan_print  :: Float
  }
  deriving (Generic, Show)

data State =
    Idle
  | Busy
  | Printing
  | Paused
  | Finished
  | Stopped
  | Error
  | Attention
  | Ready
  deriving (Generic, Show, Eq)

instance FromJSON Status
instance ToJSON Status
instance FromJSON Storage
instance ToJSON Storage
instance FromJSON Printer
instance ToJSON Printer
instance ToJSON State
instance FromJSON Job
instance ToJSON Job

instance FromJSON State where
  parseJSON v = do
    return $
      case v of
        String "IDLE" -> Idle
        String "BUSY" -> Busy
        String "PRINTING" -> Printing
        String "PAUSED" -> Paused
        String "FINISHED" -> Finished
        String "STOPPED" -> Stopped
        String "ERROR" -> Error
        String "ATTENTION" -> Attention
        String "READY" -> Ready
        v' -> error $ "Invalid status: " ++ show v'

getStatus :: String -> String -> String -> IO ByteString
getStatus host username password = do
  manager <- getGlobalManager
  req  <- parseRequest $ "GET http://" ++ host ++ "/api/v1/status"
  req' <- applyDigestAuth (S8.fromString username) (S8.fromString password) req manager
  resp <- httpBS =<< req'
  return (fromStrict $ getResponseBody resp)

readableFormatSeconds :: (Show a, Integral a) => a -> [Char]
readableFormatSeconds seconds =
  (if seconds >= 60 * 60 then
    show (seconds `div` (60 * 60)) ++ "h"
   else ""
  ) ++
  (if seconds >= 60 then
    show (seconds `mod` (60 * 60) `div` 60) ++ "m"
   else ""
  ) ++
  (if seconds < 60 * 60 then
    show (seconds `mod` 60) ++ "s"
   else ""
  )


prusaLinkNetrc :: String -> IO (Maybe PrusaLink)
prusaLinkNetrc host = do
  hosts <- readUserNetRc
  return $
    case hosts of
      Nothing -> Nothing
      Just (Left e) -> trace ("readUserNetRc error: " ++ show e) Nothing
      Just (Right netrc) ->
        traceShow netrc $
        find ((== host) . unpack . nrhName) (nrHosts netrc)
        >>= (\h -> Just $ PrusaLink host ((unpack . nrhLogin) h) ((unpack . nrhPassword) h))

instance Exec PrusaLink where
  run (PrusaLink host login password) = do
      bytes <- getStatus host login password
      let status = eitherDecode bytes :: Either String Status
      
      case status of
        Left e -> return $ "Error: " ++ e
        Right s ->
          return $
          case job s of
            Just jorb ->
              if state (printer s) == Printing then
                show (progress jorb)
                ++ "% ("
                ++ readableFormatSeconds (time_remaining jorb)
                ++ ")"
              else
                show (state $ printer s)
                ++ " "
                ++ show (progress jorb)
                ++ "%"
            Nothing ->
              show $ state $ printer s

  alias (PrusaLink host _ _) = host
  rate _ = 100
