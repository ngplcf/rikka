import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Base
import Data.Map.Strict
import Control.Monad
import Network.Socket
import System.Environment
import System.IO

import Rikka.Data

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then 
        putStrLn "./rikka [address to listen on] [port number]"
    else do
        addr <- getAddrInfo Nothing (Just $ head args) (Just $ last args) >>= 
            (\addrs -> return $ head addrs)
        sock <- socket AF_INET Stream defaultProtocol
        bind sock (addrAddress addr)
        listen sock 5

        chanIds <- newChan :: IO (Chan ThreadId)
        varNum <- newMVar 0 :: IO (MVar Int)

        world <- newTVarIO voidWorld :: IO (TVar World)
        running <- newTVarIO 0 :: IO (TVar Int)

        listenId <- forkIO $ forever $ do
                (csock, caddr) <- accept sock
                putStrLn $ "client connected from " ++ show caddr

                handle <- socketToHandle csock ReadWriteMode
                hSetBuffering handle LineBuffering

                handleID <- forkIO $ do
                    atomically $
                        modifyTVar' running (\x -> x+1)
                    catch (handleConnection handle world)
                        (\e -> do
                            if e == ThreadKilled then do
                                hPutStrLn handle "server is shutting down"
                            else
                                hPutStrLn handle "unexpected error"
                            hClose handle
                        )
                    atomically $ 
                        modifyTVar' running (\x -> x-1)

                writeChan chanIds handleID
                modifyMVar_ varNum (\x -> return (x+1))


        doCommands chanIds varNum listenId
        close sock
        -- wait for threads
        atomically $ do
            alive <- readTVar running
            check (alive == 0)

handleConnection :: Handle -> TVar World -> IO ()
handleConnection handle world = do
    nick <- hGetLine handle
    pass <- hGetLine handle

    user <- atomically $ do
        w <- readTVar world
        if member nick (users w) then
            if pass == (password $ (users w) ! nick) then
                return $ Just $ (users w) ! nick
            else
                return Nothing
        else do
            let user = User nick pass
            modifyTVar' world (addUser user)
            return $ Just user

    case user of
        Nothing ->
            hPutStrLn handle "wrong password" >> hClose handle
        Just _  -> do
            hPutStrLn handle "logged in"
            handleConnection' handle world user

    where
        handleConnection' handle world user = do
            eof <- hIsEOF handle
            if eof then
                putStrLn "client disconnected"
            else
                hGetLine handle >>= hPutStrLn handle >> handleConnection' handle world user

doCommands :: (Chan ThreadId) -> (MVar Int) -> ThreadId -> IO ()
doCommands chanIds varNum listenId = do
    cmd <- getLine
    case cmd of
        "quit"      -> do
            killThread listenId
            readMVar varNum >>= killall chanIds 
        _           -> do
            putStrLn "unknown command"
            doCommands chanIds varNum listenId
    where
        killall _ 0 = return ()
        killall chan n = 
            readChan chan >>= killThread >> killall chan (n-1)
