{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage message =
  let messageChunks = words message in
      case messageChunks of
        ("E":priority:timestamp:description) -> LogMessage (Error (read priority)) (read timestamp) (unwords description)
        ("W":timestamp:description) -> LogMessage Warning (read timestamp) (unwords description)
        ("I":timestamp:description) -> LogMessage Info (read timestamp) (unwords description)
        _ -> Unknown message
