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
        closeChan <- newChan :: IO (Chan String)

        listenId <- forkIO $ forever $ do
                (csock, caddr) <- accept sock
                putStrLn $ "client connected from " ++ show caddr

                handle <- socketToHandle csock ReadWriteMode
                hSetBuffering handle LineBuffering

                handleID <- forkIO $ catch (handleConnection handle)
                    (\e -> do
                        if e == ThreadKilled then do
                            hPutStrLn handle "server is shutting down"
                            writeChan closeChan $ "closed " ++ show caddr
                        else
                            hPutStrLn handle "unexpected error"
                        hClose handle
                    )

                writeChan chanIds handleID
                modifyMVar_ varNum (\x -> return (x+1))


        doCommands chanIds varNum closeChan listenId
        close sock

handleConnection :: Handle -> IO ()
handleConnection handle = do
    eof <- hIsEOF handle
    if eof then
        putStrLn "client disconnected"
    else
        hGetLine handle >>= hPutStrLn handle >> handleConnection handle

doCommands :: (Chan ThreadId) -> (MVar Int) -> (Chan String) -> ThreadId -> IO ()
doCommands chanIds varNum chanClose listenId = do
    cmd <- getLine
    case cmd of
        "quit"      -> do
            killThread listenId
            readMVar varNum >>= killall chanIds chanClose
        _           -> do
            putStrLn "unknown command"
            doCommands chanIds varNum chanClose listenId
    where
        killall _ _ 0 = return ()
        killall chan cchan n = 
            readChan chan >>= killThread >> readChan cchan >>= putStrLn >> killall chan cchan (n-1)
