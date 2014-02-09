import Stack
import Data.List.Split
import Data.List
import ReadCFG
import Data.Maybe

data PDA = PDA { states :: [Int],
                 alphabet :: [Char],
                 stackAlphabet :: [Char],
                 transitionMatrix :: [[[(Int,Char)]]],
                 initialState :: Int,
                 stackInitialSymbol :: Char,
                 finalStates :: [Int],
                 stack :: Stack Char
                }

newPDA :: [Int] -> [Char] -> [Char] -> [[[(Int,Char)]]] -> Int -> Char -> [Int] -> PDA
newPDA s a sa tm is sis fs = PDA{ states = s,
                                  alphabet = a,
                                  stackAlphabet = sa,
                                  transitionMatrix = tm,
                                  initialState = is,
                                  stackInitialSymbol = sis,
                                  finalStates = fs,
                                  stack = Stack.push sis $ Stack.empty
                                  }

statesFromCFG :: ([[Char]], [Char], [Char]) -> [Int]
statesFromCFG (productions , _ , _ ) = [1..(3+additional_states)]
  where additional_states = foldl (+) 0 [(length y ) - 1 |  p <- productions, let x = splitOn "->" p, let y = x !! 1]

alphabetFromCFG :: ([[Char]], [Char], [Char]) -> [Char] 
alphabetFromCFG ( _ , _ , terminals)
  | elem 'E' terminals = terminals
  | otherwise = terminals ++ "E"

stackAlphabetFromCFG :: ([[Char]], [Char], [Char]) -> [Char] 
stackAlphabetFromCFG ( _ , variables, terminals) 
  | elem 'E' sa = sa
  | otherwise = sa ++ "E"
  where sa = concat [variables, terminals]

initialStateFromCFG :: Int
initialStateFromCFG = 1

stackInitialSymbolFromCFG :: Char
stackInitialSymbolFromCFG = '$'

finalStatesFromCFG :: [Int]
finalStatesFromCFG = [3]

initTransitionMatrix :: [Int] -> [Char] -> [Char] -> [[[(Int,Char)]]]
initTransitionMatrix states alphabet stackAlphabet = replicate sl ( replicate sa ( replicate ssa (0,'#')))
  where sl = length states
        sa = length alphabet
        ssa = length stackAlphabet

loadStandardValues :: [[[(Int,Char)]]] -> [Char] -> [Char] -> [[[(Int,Char)]]]
loadStandardValues tm a sa = 
  where iEa = fromJust $ elemIndex 'E' a
        iEsa = fromJust $ elemIndex 'E' sa

getElemetFromTransitionMatrix :: [[[(Int,Char)]]] -> Int -> Int -> Int -> (Int,Char)
getElemetFromTransitionMatrix tm i1 i2 i3 = ((( tm !! i1) !! i2) !! i3)
