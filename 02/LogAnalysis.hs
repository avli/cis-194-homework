{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage message =
  let messageChunks = words message in
    case messageChunks of
      (["E", priority, timestamp, description]) -> LogMessage (Error (read priority)) (read timestamp) description
      (["W", timestamp, description]) -> LogMessage Warning (read timestamp) description
      (["I", timestamp, description]) -> LogMessage Info (read timestamp) description
      _ -> Unknown message
