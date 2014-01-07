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

-- Parse a AFN to AFD
afnToAFDStates :: AFN.AFN -> [Set Int]
afnToAFDStates afn = Data.Set.toAscList $ powerset setAFNStates
  where setAFNStates = Data.Set.fromList $ AFN.states afn

afnToAFDAlphabet :: AFN.AFN -> [Char]
afnToAFDAlphabet afn = Data.List.delete 'E' afnAlphabet
  where afnAlphabet = AFN.alphabet afn

afnToAFDFinalStates :: AFN.AFN -> [Set Int]
afnToAFDFinalStates afn = [y | x <- AFN.finalStates afn, y <- afnToAFDStates afn, Data.Set.member x y]

afnToAFDInitialState :: AFN.AFN -> Set Int
afnToAFDInitialState afn = AFN.transitiveClosure afn $ Data.Set.singleton is
  where is = AFN.initialState afn

--afnToAFDTransition :: AFN.AFN -> Matrix ( Set Int )
--afnToAFDTransition afn = 
--  where ns = afnToAFDStates afn

--transitionsOfState :: AFN.AFN -> Set Int -> [Char] -> [Set Int]
--transitionsOfState afn s a = 
--  where afnAscStates = Data.Set.toAscList $ AFN.states afn
--        is = Data.Maybe.fromJust (Data.List.elemIndex s afnAscStates)

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
  | cs == Data.Set.singleton 1 && s == '0' = Data.Set.singleton 1
  | cs == Data.Set.singleton 1 && s == '1' = Data.Set.singleton 2
  | cs == Data.Set.singleton 2 && s == '0' = Data.Set.singleton 1
  | cs == Data.Set.singleton 2 && s == '1' = Data.Set.singleton 2
