module Week02.LogAnalysis
  ( parseMessage
  , parse
  , insert
  , build
  , inOrder
  , whatWentWrong
  , module Week02.Log
  )
where

import           Week02.Log                     ( LogMessage(..)
                                                , MessageTree(..)
                                                , MessageType(..)
                                                , TimeStamp
                                                , testWhatWentWrong
                                                , testParse
                                                )

parseMessage :: String -> LogMessage
parseMessage str = case words str of
  ("I"     : t : str') -> LogMessage Info (read t) $ unwords str'
  ("W"     : t : str') -> LogMessage Warning (read t) $ unwords str'
  ("E" : c : t : str') -> LogMessage (Error $ read c) (read t) $ unwords str'
  _                    -> Unknown str


parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg         Leaf = Node Leaf msg Leaf

insert msg tree@(Node _ tMsg _) | True      = Node (insert msg Leaf) tMsg Leaf
                                | otherwise = Node Leaf tMsg (insert msg Leaf)



build :: [LogMessage] -> MessageTree
build = error "Week02.LogAnalysis#build not implemented"

inOrder :: MessageTree -> [LogMessage]
inOrder = error "Week02.LogAnalysis#inOrder not implemented"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = error "Week02.LogAnalysis#whatWentWrong not implemented"
