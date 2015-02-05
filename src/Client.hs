module Main
    where

import Control.Concurrent
import qualified Data.ByteString.Char8 as C
import GHC.Word
import Network.Socket
import Network.Socket.ByteString
import System.Environment

import DM
import Menu

prompt = "# "

getChatText input =
    "(\\\"text\\\", \\\"" ++ input ++ "\\\")"

getChatName name =
    "(\\\"name\\\", \\\"" ++ name ++ "\\\")"

-- Process a standard chat command.
processCommand clientName command params socket = do
    case (command) of
        "/s" -> sendCommand socket $ "[\"/s\", \"[" ++ (getChatName clientName) ++ "," ++ (getChatText params) ++ "]\"]"
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
mainloop socket clientName currentScene = do
    putStrLn prompt
    input <- getLine

    let arr = words input
    let command = head arr
    let params = unwords $ tail arr

    processCommand clientName command params socket
    nextScene <- processDMCommand command params currentScene

    mainloop socket clientName nextScene

main = do
    args <- getArgs
    
    adventure <- loadAdventure "../content/the_dreaming_heralds.json"
    let scene = (scenes adventure) !! 0

    let hostname = args !! 0
    let port = fromIntegral (read $ args !! 1)
    let clientName = args !! 2

    putStrLn $ "Attempting to connect to server at" ++ " " ++ hostname ++ ":" ++ show port

    socket <- socket AF_INET Stream defaultProtocol
    host <- inet_addr hostname

    conn <- connect socket (SockAddrInet port host)

    forkIO $ commThread socket

    mainloop socket clientName scene

