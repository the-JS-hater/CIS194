{-# OPTIONS_GHC -Wall #-}
module Homework2 where

import Log

--Exercise 1

parseMessage :: String -> LogMessage
parseMessage message = 
    let wordList = words message in
    case wordList of
        ("I" : timeStamp : msg) -> LogMessage Info (read timeStamp) (unwords msg)
        ("W" : timeStamp : msg) -> LogMessage Warning (read timeStamp) (unwords msg)
        ("E" : level : timeStamp : msg) -> LogMessage (Error (read level)) (read timeStamp) (unwords msg)
        _ -> Unknown (unwords wordList)
