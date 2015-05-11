import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Base
import Control.Monad
import Network.Socket
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then 
        putStrLn "./rikka [port number]"
    else do
        addr <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ head args) >>= 
            (\addrs -> return $ head addrs)
        sock <- socket AF_INET Stream defaultProtocol
        bind sock (addrAddress addr)
        listen sock 5

        chanIds <- newChan :: IO (Chan ThreadId)
        varNum <- newMVar 0 :: IO (MVar Int)

        listenId <- forkIO $ forever $ do
                (csock, caddr) <- accept sock
                putStrLn $ "client connected from " ++ show caddr

                handle <- socketToHandle csock ReadWriteMode
                hSetBuffering handle LineBuffering

                handleID <- forkIO $ catch (handleConnection handle)
                    (\e -> do
                        if e == ThreadKilled then
                            hPutStrLn handle "server is shutting down"
                        else
                            hPutStrLn handle "unexpected error"
                        hClose handle
                    )

                writeChan chanIds handleID
                modifyMVar_ varNum (\x -> return (x+1))


        doCommands chanIds varNum listenId
        close sock

handleConnection :: Handle -> IO ()
handleConnection handle = do
    eof <- hIsEOF handle
    if eof then
        putStrLn "client disconnected"
    else
        hGetLine handle >>= hPutStrLn handle >> handleConnection handle

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
        killall chan 0 = return ()
        killall chan n = 
            readChan chan >>= killThread >> killall chan (n-1)
