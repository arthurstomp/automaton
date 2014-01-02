--Functions for AFD

import Data.Set
import Data.Matrix
import Data.List
import Data.Maybe
import qualified AFN as AFN
import Prelude hiding (map)

data AFD = AFD { states :: Set (Set Int)
                 , alphabet :: [Char]
                 , transition :: Matrix (Set Int)
                 , initialState :: Set Int
                 , finalStates :: Set (Set Int)
                 } deriving (Show)

afd :: [Set Int] -> [Char] -> Matrix (Set Int)-> Set Int -> [Set Int] -> AFD 
afd s a t is fs = AFD { states = Data.Set.fromList s
                       , alphabet = a
                       , transition = t
                       , initialState = is
                       , finalStates = (Data.Set.fromList fs)
                       }

accept :: AFD -> Set Int -> Bool
accept afd cs = Data.Set.member (cs) $ finalStates afd

nextState :: AFD -> Set Int -> Char -> Set Int
nextState afd cs t = do 
  case Data.List.elemIndex t $ alphabet afd of
    Just n -> m Data.Matrix.! (ics,n+1)
    Nothing -> Data.Set.singleton (-1)
  where m = transition afd
        ics = (Data.Maybe.fromJust $ Data.List.elemIndex cs $ Data.Set.toAscList $ states afd) + 1

compute :: AFD -> Set Int -> [Char] -> Bool
compute afd cs [] = accept afd cs
compute afd cs (x:xs) = compute afd ns xs
  where ns = nextState afd cs x

mountMatrix :: Set (Set Int) -> [Char] -> ((Set Int, Char) -> Set Int) -> Matrix (Set Int)
mountMatrix sets s foo = Data.Matrix.fromLists [possibleTransitions st s foo | st <- e]
  where e = Data.Set.toAscList sets

possibleTransitions :: Set Int -> [Char] -> ((Set Int, Char) -> Set Int) -> [Set Int]
possibleTransitions cs s foo = [foo(cs,c)| c <- s]

afd1 = afd s a t is fs
  where s = [Data.Set.singleton 1,Data.Set.singleton 2]
        a = ['0','1']
        t = mountMatrix ss a foo
        is = Data.Set.singleton 1
        fs = [Data.Set.singleton 2] 
        ss = Data.Set.fromList s

foo :: (Set Int, Char) -> Set Int
foo (cs, s)
  | cs == Data.Set.singleton 1 && s == '0' = Data.Set.singleton 1
  | cs == Data.Set.singleton 1 && s == '1' = Data.Set.singleton 2
  | cs == Data.Set.singleton 2 && s == '0' = Data.Set.singleton 1
  | cs == Data.Set.singleton 2 && s == '1' = Data.Set.singleton 2

-- Parse a AFN to AFD
afnToAFDStates :: AFN.AFN -> Set (Set Int)
afnToAFDStates afn = powerset $ AFN.states afn

afnToAFDFinalStates :: AFN.AFN -> Set (Set Int)
afnToAFDFinalStates afn = Data.Set.fromList [y | x <- Data.Set.elems $ AFN.finalStates afn, y <- Data.Set.elems $ afnToAFDStates afn, Data.Set.member x y]

afnToAFDInitialState :: AFN.AFN -> Set Int
afnToAFDInitialState afn = Data.Set.singleton $ AFN.initialState afn

powerset s
    | s == empty = Data.Set.singleton empty
    | otherwise = Data.Set.map (Data.Set.insert x) pxs `Data.Set.union` pxs
      where (x, xs) = Data.Set.deleteFindMin s
            pxs = powerset xs
