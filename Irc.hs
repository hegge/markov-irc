#!/usr/bin/env runhaskell

-- Stolen from:
-- http://stackoverflow.com/questions/12118246/haskell-irc-bot-stalling-out

module Irc (main) where

import Network
import Data.List
import System.IO
import System.Random

import Markov

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
    library <- learn "logfile"
    listen h library
--
-- Process each line from the server
--
listen :: Handle -> Library -> IO ()
listen h lib = forever $ do
    s <- init `fmap` hGetLine h
    putStrLn s
    if ping s
        then pong s
        else if priv s
             then eval h lib (clean s)
             else return ()
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    priv x    = "PRIVMSG" `isInfixOf` x
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)

eval :: Handle -> Library -> String -> IO ()
eval     h lib "!quit"               = write h "QUIT" ":byebye"
eval     h lib m                     = if toMe m then do
                                                        gen <- newStdGen
                                                        privmsg h (reply gen)
                                                     else return ()
    where
        toMe x    = nick `isInfixOf` x
        question = words m
        stop = if length question > 0 then (last question) else "NEWLINE"
        reply gen = generateReply lib gen stop
eval     _ _ _                    = return () -- ignore everything else

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

write :: Handle -> String -> String -> IO ()
write handle s t = do
    hPutStr handle $ s ++ " " ++ t ++ "\r\n"
    putStrLn $ "> " ++ s ++ " " ++ t ++ "\n"
