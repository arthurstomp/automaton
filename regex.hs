-- Functions for read a Regular Expression and compute words.
import AFN
import Data.Maybe

regexReader :: [Char] -> AFN
regexReader [x] = simple x
regexReader (x:y:xs) 
  | notElem x operationsChars =  rrp (simple x) Nothing (y:xs)
  | otherwise = rrp a Nothing s
  where operationsChars = ['.','+','*','(',')']
        (a,s) = rrpa Nothing (simple y,xs) Nothing

-- regexReaderProcessor = rrp
rrp :: AFN -> Maybe (AFN -> AFN) -> [Char] -> AFN
rrp afn mop [] = afn
rrp afn mop (x:xs)
  | x == '(' = rrp a Nothing s
  where (a,s) = rrpa mop (afn, xs) Nothing
rrp afn mop (x:xs)
  | isNothing mop && x == '*' = rrp (star afn) Nothing xs
  | isNothing mop && x == '.' = rrp afn (Just $ AFN.concat afn) xs
  | isNothing mop && x == '+' = rrp afn (Just $ AFN.or afn) xs
  | isJust mop && notElem x operationsChars = rrp (fromJust mop $ simple x) Nothing xs
  | otherwise = afn
  where operationsChars = ['.','+','*','(',')']

-- regexReaderProcessorAuxiliar = rrpa
rrpa :: Maybe (AFN -> AFN) -> (AFN,[Char]) -> Maybe (AFN -> AFN) -> (AFN,[Char]) 
rrpa sleeOp (afn,[]) mop = (afn,[])
rrpa sleepOp (afn,[')']) mop 
  | isJust sleepOp = (fromJust sleepOp afn, [])
  | otherwise = (afn,[])
rrpa sleepOp (afn,(x:y:xs)) mop 
  | x == ')' && y == '*' && isJust sleepOp = (fromJust sleepOp (star afn), xs) 
  | x == ')' && y == '*' && isNothing sleepOp = (star afn, xs) 
  | x == ')' && y /= '*' && isJust sleepOp = (fromJust sleepOp afn, (y:xs))  
  | x == ')' && y /= '*' && isNothing sleepOp= (afn, (y:xs))  
rrpa sleepOp (afn,(x:xs)) mop 
  | isNothing mop && x == '*' = rrpa sleepOp ((star afn),xs) Nothing
  | isNothing mop && x == '.' = rrpa sleepOp (afn,xs) (Just $ AFN.concat afn)
  | isNothing mop && x == '+' = rrpa sleepOp (afn,xs) (Just $ AFN.or afn)
  | isJust mop && notElem x operationsChars = rrpa sleepOp ((fromJust mop $ simple x),xs) Nothing 
  | otherwise = (rrp afn mop (x:xs),(x:xs))
  where operationsChars = ['.','+','*','(',')']

main :: IO() 
main = do rx <- getLine
          let afn = regexReader rx
          w <- getLine
          let r = compute afn (prettyInitialState afn) w
          if r == True then putStrLn "1"
          else putStrLn "0"
