{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module MK4Monitor
  ( MK4 (MK4), mk4Netrc ) where

import Xmobar
    ( Exec (start, alias) )
import Data.Aeson ( FromJSON(parseJSON), eitherDecode, Value(..) )
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpBS )
import qualified Data.String as S8
import Network.HTTP.Client.TLS (applyDigestAuth, getGlobalManager)
import Data.Aeson.Types (ToJSON)
import Data.ByteString (fromStrict)
import GHC.Generics (Generic)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T (unpack)
import Network.NetRc (readUserNetRc, nrHosts, NetRcHost (nrhName, nrhLogin, nrhPassword))
import Data.List (find)
import Data.ByteString.Char8 (unpack)
import Debug.Trace (traceShow, trace)

-- https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md
-- https://hackage.haskell.org/package/http-client-tls-0.3.6.3/docs/Network-HTTP-Client-TLS.html
data MK4 = MK4 String String String
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
  , progress :: Float
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
  , axis_x :: Float
  , axis_y :: Float
  , axis_z :: Float
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
  deriving (Generic, Show)

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

mk4Netrc :: String -> IO (Maybe MK4)
mk4Netrc host = do
  hosts <- readUserNetRc
  return $
    case hosts of
      Nothing -> trace "readUserNetRc returned Nothing" Nothing
      Just (Left e) -> trace ("readUserNetRc error: " ++ show e) Nothing
      Just (Right netrc) ->
        traceShow netrc $
        find ((== host) . unpack . nrhName) (nrHosts netrc)
        >>= (\h -> Just $ MK4 host ((unpack . nrhLogin) h) ((unpack . nrhPassword) h))

instance Exec MK4 where
  start (MK4 host login password) callback = do
    bytes <- getStatus host login password
    let status = eitherDecode bytes :: Either String Status
    case status of
      Left e -> callback ("Error: " ++ e)
      Right s -> callback (show $ state $ printer s)
  alias (MK4 host _ _) = host