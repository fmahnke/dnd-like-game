import Control.Concurrent (forkIO, threadDelay)
import Data.Maybe
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)

welcomeMessage = "Welcome to our multiverse."

sayCommand handle name text = do
    let output = text
    let playerChunk = "[" ++ name ++ "]"
    let textChunk = output

    hPutStrLn handle $ playerChunk ++ " " ++ textChunk

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    line <- hGetLine handle

    let struct = read line :: [String]
    putStrLn $ "Struct" ++ show struct
    let params = read $ struct !! 1 :: [([Char], [Char])]
    putStrLn $ "Params" ++ show params

    let cmd = struct !! 0
    let name = Data.Maybe.fromMaybe "nothing" $ lookup "name" params
    let text = Data.Maybe.fromMaybe "nothing" $ lookup "text" params

    case (cmd) of
        "/s" -> sayCommand handle name text
        _ -> do hPutStrLn handle "Unknown command"
    commandProcessor handle

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle LineBuffering

    hPutStrLn handle welcomeMessage

    forkIO $ commandProcessor handle

    sockHandler sock

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)

    putStrLn $ "Starting server on port" ++ " " ++ show port ++ "."
    sock <- listenOn $ PortNumber port

    sockHandler sock
