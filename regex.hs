-- Functions for read a Regular Expression and compute words.
import AFN
import Data.Maybe

regexReader :: [Char] -> AFN
regexReader [x] = simple x
regexReader (x:y:xs) 
  | notElem x operationsChars =  rrp (simple x) Nothing (y:xs)
  | otherwise = do let (a,s) = rrpa Nothing (simple y,xs) Nothing
                   rrp a Nothing s
  where operationsChars = ['.','+','*','(',')']
 
rrp :: Maybe ( AFN -> AFN ) -> (AFN,[Char]) -> Maybe ( AFN -> AFN ) -> AFN
-- Case before reach a parenteses
rrp Nothing (afn,(x:xs)) nextOp
  | isJust nextOp && notElem x ['.','+','*','(',')'] = do let newAFN = fromJust nextOp $ simple x
                                                          rrp Nothing (newAFN,xs) Nothing
  | x == '*' = rrp Nothing (star afn,xs) Nothing
  | x == '+' = rrp Nothing (afn, xs) Just $ AFN.or afn)
  | x == '.' = rrp Nothing (afn, xs) Just $ AFN.concat afn)
