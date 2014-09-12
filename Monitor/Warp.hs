{-# LANGUAGE RecordWildCards #-}
module Monitor.Warp where

import Control.Concurrent                ( forkIO, killThread )
import Control.Concurrent.STM
import Control.Monad                     ( when, liftM, join, void )
import Control.Monad.Trans               ( liftIO )
import Network.Wai
import Network.Wai.Handler.Warp
import System.Posix.Signals              ( installHandler, Handler(Catch), sigHUP, sigTERM, fullSignalSet )

data Shutdown = Exit | Restart
    deriving (Eq, Show)

-- | This function "wraps" around the original request handler to allow 
-- graceful shutdown and restart operations.
proxy :: TMVar Shutdown -> Application -> Response -> Application
proxy shutdown handler message req resp = 
    join . atomically $ do
        -- Check if a shutdown/restart has been initiated
        shouldRun <- isEmptyTMVar shutdown
        return $ if shouldRun
            then handler req resp
            -- If the TMVar is full, we're shutting down
            else resp message

-- | Server configuration parameterized over a generic type. This can be used 
-- by client applications to supply the request handler with shared application 
-- data, e.g., a database connection handle. 
data ServerSettings a = ServerSettings
    { handler  :: Application   -- ^ The client's request handler
    , config   :: Settings      -- ^ Warp server settings
    , response :: Response      -- ^ A response to return during shutdown
    , options  :: a             -- ^ Client application data which should be
                               -- made available to the request handler.
    }

-- | Provides some process management capabilities via SIGHUP and SIGTERM to the 
-- Warp server by running the server as a monitored worker thread and installing 
-- the required signal handlers.
sigServ :: IO (ServerSettings a)      -- ^ Called prior to server start
        -> (ServerSettings a -> IO ()) -- ^ Shutdown hook
        -> IO ()
sigServ onStartup onExit = do

    shutdown <- newEmptyTMVarIO
    conns <- newTVarIO (0 :: Int)

    installHandler sigHUP  (Catch $ hup  shutdown) (Just fullSignalSet)
    installHandler sigTERM (Catch $ term shutdown) (Just fullSignalSet)

    runWorker shutdown conns 

    putStrLn "-- Shutting down. --"

  where
    runWorker :: TMVar Shutdown -> TVar Int -> IO ()
    runWorker shutdown conns = do
    
        settings@ServerSettings{..} <- onStartup
    
        -- Install hooks to keep track of the number of open connections
        let settings' = setOnOpen  (onOpen  conns)
                      $ setOnClose (onClose conns)
                        config

        worker <- forkIO $ runSettings settings' (proxy shutdown handler response)
        action <- monitor shutdown conns

        -- The worker is taken down, in case we intend to restart the service
        killThread worker

        onExit settings
    
        -- If a SIGHUP was received, we reload any configuration
        -- files, re-initialize the server and fork a new thread
        when (Restart == action) $ runWorker shutdown conns 

-- | Wait for the worker thread to signal exit via the TMVar synchronization 
-- primitive and then block until all outstanding requests have completed.
-- The function returns one of the two Shutdown constructors to indicate the 
-- type of shutdown, viz., Exit or Restart.
monitor :: TMVar Shutdown -> TVar Int -> IO Shutdown
monitor shutdown conns = atomically $ do
    var <- takeTMVar shutdown
    conns <- readTVar conns
    when (conns /= 0) 
        retry
    return var

hup :: TMVar Shutdown -> IO ()
hup tvar = do
    putStrLn "SIGHUP"
    atomically $ putTMVar tvar Restart

term :: TMVar Shutdown -> IO ()
term tvar = do
    putStrLn "SIGTERM"
    atomically $ putTMVar tvar Exit

modify :: TVar a -> (a -> a) -> IO ()
modify tvar = atomically . modifyTVar' tvar

onOpen :: Num a => TVar a -> t -> IO Bool
onOpen tvar _  = modify tvar (+1) >> return True

onClose :: Num a => TVar a -> t -> IO ()
onClose tvar _ = modify tvar (subtract 1) 

