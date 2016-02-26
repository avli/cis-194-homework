{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage message =
  let
    messageChunks = words message
    restoreString = foldl1 (\acc word -> acc ++ " " ++ word) in
    case messageChunks of
      ("E":priority:timestamp:description) -> LogMessage (Error (read priority)) (read timestamp) (restoreString description)
      ("W":timestamp:description) -> LogMessage Warning (read timestamp) (restoreString description)
      ("I":timestamp:description) -> LogMessage Info (read timestamp) (restoreString description)
      _ -> Unknown message
