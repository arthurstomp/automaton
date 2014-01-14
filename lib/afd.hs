--Functions for AFD

import Data.Set
import Data.Matrix
import Data.List
import Data.Maybe
import qualified AFN as AFN
import Powerset

data AFD = AFD { states :: [Set Int]
                 , alphabet :: [Char]
                 , transition :: Matrix (Set Int)
                 , initialState :: Set Int
                 , finalStates :: [Set Int]
                 } deriving (Show)

afd :: [Set Int] -> [Char] -> Matrix (Set Int)-> Set Int -> [Set Int] -> AFD 
afd s a t is fs = AFD { states = s
                       , alphabet = a
                       , transition = t
                       , initialState = is
                       , finalStates = fs
                       }

compute :: AFD -> Set Int -> [Char] -> Bool
compute afd cs [] = accept afd cs
compute afd cs (x:xs) = compute afd ns xs
  where ns = nextState afd cs x
  
accept :: AFD -> Set Int -> Bool
accept afd cs = Data.List.elem (cs) $ finalStates afd

nextState :: AFD -> Set Int -> Char -> Set Int
nextState afd cs t = do 
  case Data.List.elemIndex t $ alphabet afd of
    Just n -> m Data.Matrix.! (ics,n+1)
    Nothing -> Data.Set.singleton (-1)
  where m = transition afd
        ics = (Data.Maybe.fromJust $ Data.List.elemIndex cs $ states afd) + 1

-- Transforma um AFN num AFD
-- turn a AFN into AFD
turn :: AFN.AFN -> AFD
turn afn = afd s a t is fs
  where s = turnStates afn
        a = turnAlphabet afn
        t = turnTransition afn
        is = turnInitialState afn
        fs = turnFinalStates afn

turnStates :: AFN.AFN -> [Set Int]
turnStates afn = Data.Set.toAscList $ powerset setAFNStates
  where setAFNStates = Data.Set.fromList $ AFN.states afn

turnAlphabet :: AFN.AFN -> [Char]
turnAlphabet afn = Data.List.delete 'E' afnAlphabet
  where afnAlphabet = AFN.alphabet afn

turnTransition :: AFN.AFN -> Matrix ( Set Int )
turnTransition afn = Data.Matrix.fromLists [transitionsOfAFNState afn s | s <- ns]
  where ns = turnStates afn

turnInitialState :: AFN.AFN -> Set Int
turnInitialState afn = AFN.transitiveClosure afn $ Data.Set.singleton is
  where is = AFN.initialState afn

turnFinalStates :: AFN.AFN -> [Set Int]
turnFinalStates afn = [y | x <- AFN.finalStates afn, y <- turnStates afn, Data.Set.member x y]

transitionsOfAFNState :: AFN.AFN -> Set Int -> [Set Int]
transitionsOfAFNState afn s = [turnNextState afn s c | c <- alpha] 
  where alpha = turnAlphabet afn

turnNextState :: AFN.AFN -> Set Int -> Char -> Set Int
turnNextState afn cs c = Data.Set.unions [turnTransitionsFromAFNState afn x c | x <- Data.Set.elems cs]

turnTransitionsFromAFNState :: AFN.AFN -> Int -> Char -> Set Int
turnTransitionsFromAFNState afn cs t 
  | ns == Data.Set.empty = Data.Set.empty
  | ns /= Data.Set.empty = ns
  where it = AFN.indexTransition afn t
        ns = (AFN.transition afn) Data.Matrix.! (cs,it+1)

-- Testing Area
afd1 = afd s a t is fs
  where s = [Data.Set.singleton 1,Data.Set.singleton 2]
        a = ['0','1']
        t = mountMatrix ss a foo
        is = Data.Set.singleton 1
        fs = [Data.Set.singleton 2] 
        ss = Data.Set.fromList s

afn1 = AFN.afn1

mountMatrix :: Set (Set Int) -> [Char] -> ((Set Int, Char) -> Set Int) -> Matrix (Set Int)
mountMatrix sets s foo = Data.Matrix.fromLists [possibleTransitions st s foo | st <- e]
  where e = Data.Set.toAscList sets

possibleTransitions :: Set Int -> [Char] -> ((Set Int, Char) -> Set Int) -> [Set Int]
possibleTransitions cs s foo = [foo(cs,c)| c <- s]

foo :: (Set Int, Char) -> Set Int
foo (cs, s)
  | cs == Data.Set.singleton 1 && s == '0' = Data.Set.singleton 2
  | cs == Data.Set.singleton 1 && s == '1' = Data.Set.singleton 1
  | cs == Data.Set.singleton 2 && s == '0' = Data.Set.singleton 3
  | cs == Data.Set.singleton 2 && s == '1' = Data.Set.singleton 1
  | cs == Data.Set.singleton 3 && s == '0' = Data.Set.singleton 3
  | cs == Data.Set.singleton 3 && s == '1' = Data.Set.singleton 1
