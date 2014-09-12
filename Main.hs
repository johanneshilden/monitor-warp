{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent                ( threadDelay )
import Monitor.Warp
import Data.ByteString.Lazy              ( ByteString )
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import qualified Data.ByteString.Lazy.Char8 as BL

data AppData = AppData ByteString

app :: AppData -> Application
app (AppData a) req resp = do
    threadDelay 2000000 -- Sleep for 2 secs.
    resp $ responseLBS ok200 [] $ BL.concat [a, " says: YOLO!\n"]

onStartup :: IO (ServerSettings AppData)
onStartup = do
    putStrLn "Service starting."
    return $ ServerSettings 
        { handler  = app
        , config   = setPort 3333 $ defaultSettings
        , response = responseLBS serviceUnavailable503 [] "Server shutting down."
        , options  = AppData "demo"
        }

-- | Do nothing in particular.
onExit :: ServerSettings AppData -> Shutdown -> IO ()
onExit _ action = do
    case action of
        Restart -> putStrLn "Service closing down for restart.\n"
        Exit    -> putStrLn "Service terminating.\n"

main :: IO ()
main = sigServ onStartup onExit

