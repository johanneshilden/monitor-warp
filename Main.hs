{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent                ( threadDelay )
import Monitor.Warp
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

app :: Application
app req resp = do
    threadDelay 5000000 -- Sleep for 5 secs.
    resp $ responseLBS ok200 [] "YOLO!\n"

onStartup :: IO (ServerSettings ())
onStartup = do
    return $ ServerSettings 
        { handler  = app
        , config   = setPort 3333 $ defaultSettings
        , response = responseLBS serviceUnavailable503 [] "Shutting down."
        , options  = ()
        }

-- | Do nothing in particular.
onExit :: ServerSettings () -> IO ()
onExit _ = return ()

main :: IO ()
main = sigServ onStartup onExit

