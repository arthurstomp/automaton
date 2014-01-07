--Functions for AFN

--module AFN
--( afn
--, AFN
--, compute
--, afn1
--, states
--, alphabet
--, initialState
--, finalStates
--, transition
--)where

import Data.Set
import Data.Matrix
import Data.List
import Data.Maybe

data AFN = AFN { states :: [Int]
               , alphabet :: [Char]
               , transition :: Matrix (Set Int)
               , initialState :: Int
               , finalStates :: [Int]
               } deriving (Show)

afn :: [Int] -> [Char] -> Matrix (Set Int) -> Int -> [Int] -> AFN
afn s a t is fs = AFN { states = s
                       , alphabet = alpha
                       , transition = t
                       , initialState = is
                       , finalStates = fs
                       }
  where alpha = Data.List.insert 'E' a

accept :: AFN -> Set Int -> Bool
accept afn cs  = True `elem` acceptance 
  where acceptance = [ Data.List.elem x $ finalStates afn | x <- Data.Set.elems cs] 

compute :: AFN -> Set Int -> [Char] -> Bool
compute afn cs [] = accept afn cs
compute afn cs (x:xs) = compute afn ns xs
  where ns = nextState afn cs x

nextState :: AFN -> Set Int -> Char -> Set Int
nextState afn cs t = Data.Set.union (partialNextState afn cs t) (partialNextState afn cs 'E')

partialNextState :: AFN -> Set Int -> Char -> Set Int
partialNextState afn cs c = Data.Set.unions [transitionFromState afn x c | x <- Data.Set.elems cs]

prettyInitialState :: AFN -> Set Int
prettyInitialState afn = Data.Set.singleton $ initialState afn

transitionFromState :: AFN -> Int -> Char -> Set Int
transitionFromState afn cs t
  | ns == Data.Set.empty = Data.Set.singleton cs 
  | ns /= Data.Set.empty = ns
  where it = indexTransition afn t
        ns = (transition afn) Data.Matrix.! (cs,it+1)

indexTransition :: AFN -> Char -> Int
indexTransition afn t = Data.Maybe.fromJust $ Data.List.elemIndex t a
  where a = alphabet afn

-- Testing Area
mountMatrix :: [Int] -> [Char] -> ((Int,Char) -> Set Int) -> Matrix (Set Int)
mountMatrix s a foo = Data.Matrix.fromLists [possibleTransitions x a foo | x <- s]

possibleTransitions :: Int -> [Char] -> ((Int,Char) -> Set Int) -> [Set Int]
possibleTransitions cs a foo = [foo(cs,c) | c <- a]

foo :: (Int,Char) -> Set Int
foo (cs,c)
  | cs == 1 && c == '0' = Data.Set.fromList [1,2] 
  | cs == 1 && c == '1' = Data.Set.singleton 1
  | cs == 1 && c == 'E' = Data.Set.empty
  | cs == 2 && c == '0' = Data.Set.singleton 3
  | cs == 2 && c == '1' = Data.Set.empty
  | cs == 2 && c == 'E' = Data.Set.empty
  | cs == 3 && c == '0' = Data.Set.empty
  | cs == 3 && c == '1' = Data.Set.empty
  | cs == 3 && c == 'E' = Data.Set.empty

afn1 = afn s a m is fs
  where s = [1,2,3]
        a = ['0','1']
        m = mountMatrix s alpha foo
        is = 1
        fs = [3]
        alpha = Data.List.insert 'E' a

is = prettyInitialState afn1
ns1 = nextState afn1 is '0'
ns2 = nextState afn1 ns1 '0'
ns3 = nextState afn1 ns2 '1'
