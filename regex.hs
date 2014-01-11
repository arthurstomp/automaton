-- Functions for read a Regular Expression and compute words.
import AFN
import Data.Maybe

regexReader :: [Char] -> AFN
regexReader regex = rrp Nothing (Nothing,regex) Nothing 

rrp :: Maybe ( AFN -> AFN ) -> (Maybe AFN,[Char]) -> Maybe ( AFN -> AFN ) -> AFN
-- Case of empty regex
rrp sleepyOP (Nothing,[]) Nothing = simple ' '
-- Case maybeAFN is nothing. That`s the first char read or the regex have just entered in a parenteses
rrp sleepyOp (Nothing,(x:xs)) Nothing 
  | x == '(' = rrp sleepyOp (Nothing,xs) Nothing
  | notElem x ['.','+','*','(',')'] = rrp sleepyOp (Just $ simple x, xs) Nothing
  | otherwise = simple ' '
-- Reaches a empty list 
rrp sleepyOp (Just afn, []) Nothing
  | isNothing sleepyOp = afn
  | isJust sleepyOp = (fromJust sleepyOp) afn 
  | otherwise = error "Reaches a empty list"
-- Close a parenteses and have no sleepyOp 
rrp Nothing (Just afn, (')':y:xs)) nextOp 
  | y == '*' = rrp Nothing (Just $ star afn, xs) Nothing
  | otherwise = rrp Nothing (Just afn,(y:xs)) Nothing
-- Close parenteses and have a sleepyOp
rrp (Just sleepyOp) (Just afn, (')':y:xs)) nextOp 
  | y == '*' = rrp Nothing (Just $ sleepyOp $ star afn,xs) Nothing
  | otherwise = rrp Nothing (Just $ sleepyOp afn, (y:xs)) Nothing

rrp sleepyOp (Just afn, (x:xs)) nextOp
  | x == '(' = rrp nextOp (Nothing,xs) Nothing
  | notElem x ['.','+','*','(',')'] = do let op = fromJust nextOp
                                         rrp Nothing (Just $ op $ simple x,xs) Nothing
  | x == '*' = rrp Nothing (Just $ star afn,xs) Nothing
  | x == '.' = rrp Nothing (Just afn, xs) (Just $ AFN.concat afn)
  | x == '+' = rrp Nothing (Just afn, xs) (Just $ AFN.or afn)
