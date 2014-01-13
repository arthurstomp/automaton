-- Functions for read a Regular Expression and compute words.
module Regex(
regexReader
) where

import AFN
import Data.Maybe


regexReader :: [Char] -> AFN
regexReader regex = rrp Nothing (Nothing,regex) Nothing 

-- rrp :: maybe sleepyOp -> (maybe AFN, regex) -> maybe currentOp -> resultAFN
--  maybe sleepyOp /= Nothing when reading is outside parenteses
--  maybe AFN and resultAFN is just for start the process and give back the result
--  currentOp keep the operation waiting for it second element
rrp :: Maybe ( AFN -> AFN ) -> (Maybe AFN,[Char]) -> Maybe ( AFN -> AFN ) -> AFN
-- Case of empty regex
rrp _ (Nothing,[]) Nothing = simple 'V'
-- Reading outside parenteses the first character or start with '('
rrp sleepyOp (Just afn,[]) Nothing 
 | isNothing sleepyOp = afn
 | otherwise = (fromJust sleepyOp) afn
rrp sleepyOp (Nothing,[x]) Nothing = rrp sleepyOp (Just $ simple x, []) Nothing
rrp sleepyOp (Nothing,(x:y:xs)) Nothing
  | notElem x ['.','+','*','(',')'] && y /= '*' = rrp sleepyOp (Just $ simple x, (y:xs)) Nothing
  | notElem x ['.','+','*','(',')'] && y == '*' = rrp sleepyOp (Just $ star $ simple x, (xs)) Nothing
rrp Nothing (Nothing,(x:y:xs)) Nothing
  | x == '(' = rrp Nothing (Nothing, (y:xs)) Nothing
-- Reading operation and then, suddenly, a wild parenteses apear
rrp _ (Just afn,(x:y:xs)) Nothing
  | x == '.' && y == '(' = rrp (Just $ AFN.concat afn) (Nothing, xs) Nothing
  | x == '+' && y == '(' = rrp (Just $ AFN.or afn) (Nothing, xs) Nothing
-- Reading operation 
rrp sleepyOp (Just afn,(x:xs)) Nothing
  | x == '.' = rrp sleepyOp (Nothing, xs) (Just $ AFN.concat afn)
  | x == '+' = rrp sleepyOp (Nothing, xs) (Just $ AFN.or afn)
  | x == '*' = rrp sleepyOp (Just $ star $ afn,xs) Nothing 
-- Filing currentOp
rrp sleepyOp (Nothing, [x]) (Just currentOp) = rrp Nothing (Just(currentOp $ simple x),[]) Nothing
rrp sleepyOp (Nothing, (x:y:xs)) (Just currentOp)
  | x == '(' = rrp (Just currentOp) (Nothing,(y:xs)) Nothing
  | notElem x ['.','+','*','(',')'] && y == '*' = rrp sleepyOp (Just(currentOp $ star $ simple x),(xs)) Nothing
  | notElem x ['.','+','*','(',')'] && y /= '*' = rrp sleepyOp (Just(currentOp $ simple x),(y:xs)) Nothing
-- Closing paranteses when no sleepyOp
rrp Nothing (Just afn,[')']) _ = rrp Nothing (Just afn,[]) Nothing
rrp Nothing (Just afn,(x:y:xs)) _
  | x == ')' && y == '*' = rrp Nothing (Just $ star afn,xs) Nothing
  | x == ')' && y /= '*' = rrp Nothing (Just afn,(y:xs)) Nothing
-- Closing parenteses
rrp (Just sleepyOp) (Just afn,[')']) _ = rrp Nothing (Just(sleepyOp $ afn),[]) Nothing
rrp (Just sleepyOp) (Just afn,(x:y:xs)) _
  | x == ')' && y == '*' = rrp Nothing (Just(sleepyOp $ star afn),xs) Nothing
  | x == ')' && y /= '*' = rrp Nothing (Just(sleepyOp $ afn),(y:xs)) Nothing
