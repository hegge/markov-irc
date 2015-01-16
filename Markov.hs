#!/usr/bin/env runhaskell

{-# LANGUAGE TemplateHaskell #-}

module Markov (Library, learn, generateReply) where

import Test.QuickCheck
import Test.QuickCheck.All

import Network.HTTP

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split

import qualified Data.Map as Map

import System.Environment
import System.Random

type LibraryData = Map.Map (String, String) [String]

newtype Library = MakeLibrary LibraryData

fromLibrary :: Library -> LibraryData
fromLibrary (MakeLibrary l) = l

emptyLibrary :: Library
emptyLibrary = MakeLibrary Map.empty

main = do
        args <- getArgs
        {-case (args!!0) of-}
            {-"import" -> print "learning"-}
            {-"reply" -> let question = (args!!1)-}
                       {-in print $ generateReply question-}

        let question = (args!!0)

        library <- learn "logfile"
        gen <- getStdGen
        print $ generateReply library gen question

learn :: String -> IO Library
learn fileName = do blob <- readFile fileName
                    return $ fillLibrary $ splitWords blob

splitWords :: String -> [String]
splitWords blob = words $ intercalate " NEWLINE " $ lines blob

fillLibrary :: [String] -> Library
fillLibrary blob = toMultiMap emptyLibrary $
                    zipWith3 (\x y z -> ((x,y), z)) blob (tail blob) (tail (tail blob))

toMultiMap :: Library -> [((String,String), String)] -> Library
toMultiMap library [] = library
toMultiMap library (p:ps) = let (k,v) = p
                                newLibrary = MakeLibrary $ Map.insertWith (++) k [v] (fromLibrary library)
                            in toMultiMap newLibrary ps

generateReply :: Library -> StdGen -> String -> String
generateReply library gen input =
                              let word = words input
                                  lastWord = last word
                                  canReply = Map.member (lastWord, "NEWLINE") (fromLibrary library)
                                  (answer, gen') = reply library gen (lastWord, "NEWLINE")
                              in if canReply
                                then answer
                                else let size = length $ Map.keys (fromLibrary library)
                                         (seed, gen') = randomR (0, size-1) gen
                                         elem = Map.elemAt seed (fromLibrary library)
                                         (answer, gen'') = reply library gen' (fst elem)
                                     in if answer /= ""
                                          then answer
                                          else generateReply library gen'' input

terminatingReply :: Library -> StdGen -> (String, String) -> (String, StdGen)
terminatingReply library gen (firstWord, secondWord)
  | secondWord == "NEWLINE" = ("", gen)
  | otherwise  =
                let (nextWord, gen') = reply library gen (firstWord, secondWord)
                in (secondWord ++ " " ++ nextWord, gen')

reply :: Library -> StdGen -> (String, String) -> (String, StdGen)
reply library gen (firstWord, secondWord) =
    let nextP = Map.lookup (firstWord, secondWord) (fromLibrary library)
    in case nextP of
        Nothing -> ("", gen)
        Just next -> let (nextWord, gen') = selectWord gen next
                         (answer, gen'') = terminatingReply library gen' (secondWord, nextWord)
                     in (answer, gen'')

selectWord :: StdGen -> [String] -> (String, StdGen)
selectWord gen list =
        let size = length list
            (seed, gen') = randomR (0, size-1) gen
        in (list!!seed, gen')

test = $(quickCheckAll)
