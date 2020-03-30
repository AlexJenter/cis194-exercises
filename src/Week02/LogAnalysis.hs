
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
insert msg (Node l tMsg r) | time msg <= time tMsg = Node (insert msg l) tMsg r
                           | otherwise             = Node l tMsg (insert msg r)
 where
  time (LogMessage _ t _) = t
  time (Unknown _       ) = undefined

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map pluckMsg . filter relevant . inOrder . build
 where
  pluckMsg :: LogMessage -> String
  pluckMsg (LogMessage _ _ s) = s
  pluckMsg _                  = ""

  relevant :: LogMessage -> Bool
  relevant (LogMessage (Error eCode) _ _) = eCode > 50
  relevant _                              = False


