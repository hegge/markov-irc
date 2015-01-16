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
                              in if canReply
                                then reply library gen (lastWord, "NEWLINE")
                                else let size = length $ Map.keys (fromLibrary library)
                                         (seed, gen_) = randomR (0, size) gen
                                         elem = Map.elemAt seed (fromLibrary library)
                                     in reply library gen (fst elem)

terminatingReply :: Library -> StdGen -> (String, String) -> String
terminatingReply library gen (firstWord, secondWord)
  | secondWord == "NEWLINE" = ""
  | otherwise  = secondWord ++ " " ++ reply library gen (firstWord, secondWord)

reply :: Library -> StdGen -> (String, String) -> String
reply library gen (firstWord, secondWord) =
    let nextP = Map.lookup (firstWord, secondWord) (fromLibrary library)
    in case nextP of
        Nothing -> ""
        Just next -> let (gen1, gen2) = System.Random.split gen
                         nextWord = selectWord gen1 next
                     in terminatingReply library gen2 (secondWord, nextWord)

selectWord :: StdGen -> [String] -> String
selectWord gen list =
        let size = length list
            (seed, gen_) = randomR (0, size-1) gen
        in list!!seed

test = $(quickCheckAll)
