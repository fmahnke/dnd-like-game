module Main
    where

import Control.Concurrent
import qualified Data.ByteString.Char8 as C
import GHC.Word
import Network.Socket
import Network.Socket.ByteString
import System.Environment

prompt = "# "

playerName = "Player"

getChatText input =
    "(\\\"text\\\", \\\"" ++ input ++ "\\\")"

processCommand command params socket = do
    --let command = words input
    case (command) of
        "/s" -> sendCommand socket $ "[\"/s\", \"[(\\\"name\\\", \\\"Player\\\")," ++ (getChatText params) ++ "]\"]"
        _ -> sendCommand socket $ command

-- Send a command over a socket. The command is a string. It will be terminated with a newline.
sendCommand socket command = do
    Network.Socket.ByteString.send socket $ C.pack $ command ++ "\n"

-- Receive data on a socket and echo it.
commThread socket = do
    out <- Network.Socket.ByteString.recv socket 4096
    C.putStrLn out
    commThread socket

-- Continuously display prompt and process input.
mainloop socket = do
    putStrLn prompt
    input <- getLine

    let arr = words input
    let command = head arr
    let params = unwords $ tail arr

    processCommand command params socket

    mainloop socket

main = do
    args <- getArgs
    
    let hostname = args !! 0
    let port = fromIntegral (read $ args !! 1)
    putStrLn $ "Attempting to connect to server at" ++ " " ++ hostname ++ ":" ++ show port

    socket <- socket AF_INET Stream defaultProtocol
    host <- inet_addr hostname

    conn <- connect socket (SockAddrInet port host)

    forkIO $ commThread socket

    mainloop socket

