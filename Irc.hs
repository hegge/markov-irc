#!/usr/bin/env runhaskell

-- Stolen from:
-- http://stackoverflow.com/questions/12118246/haskell-irc-bot-stalling-out

module Irc (main) where

import Network
import Data.List
import System.IO

server = "10.1.1.140"
port   = 6667
chan   = "#dev"
nick   = "nonsense"

main :: IO ()
main = do
    bot <- connect
    run bot

connect :: IO Handle
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return h
  where
    notify a = do
        putStrLn ("Connecting to " ++ server ++ " ... ") >> hFlush stdout
        putStrLn "done."
        a

run :: Handle -> IO ()
run h = do
    write h "NICK" nick
    write h "USER" (nick++" 0 * :silly bot")
    write h "JOIN" chan
    listen h
--
-- Process each line from the server
--
listen :: Handle -> IO ()
listen h = forever $ do
    s <- init `fmap` hGetLine h
    putStrLn s
    if ping s then pong s else eval h (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)

eval :: Handle -> String -> IO ()
eval     h "!quit"               = write h "QUIT" ":byebye"
eval     _ _                     = return () -- ignore everything else

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

write :: Handle -> String -> String -> IO ()
write handle s t = do
    hPutStr handle $ s ++ " " ++ t ++ "\r\n"
    putStrLn $ "> " ++ s ++ " " ++ t ++ "\n"
