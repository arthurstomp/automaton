--Functions for AFD

import Data.Set
import Data.Matrix
import Data.List
import qualified AFN as AFN
import Prelude hiding (map)

data AFD = AFD { states :: Set Int
                 , alphabet :: Set Char
                 , transition :: Matrix Int
                 , initialState :: Int
                 , finalStates :: Set Int
                 } deriving (Show)

afd :: [Int] -> [Char] -> ((Int,Int) -> Int) -> Int -> [Int] -> AFD 
afd s a ft is fs = AFD { states = Data.Set.fromList s
                       , alphabet = Data.Set.fromList a
                       , transition = t
                       , initialState = is
                       , finalStates = (Data.Set.fromList fs)
                       }
  where t = Data.Matrix.matrix (length s) (length a) $ ft

accept :: AFD -> Int -> Bool
accept afd cs = Data.Set.member cs $ finalStates afd

nextState :: AFD -> Int -> Char -> Int
nextState afd cs t = do 
  case Data.List.elemIndex t $ toAscList $ alphabet afd of
    Just n -> m Data.Matrix.! (cs,n+1)
    Nothing -> -1
  where m = transition afd

compute :: AFD -> Int -> [Char] -> Bool
compute afd cs [] = accept afd cs
compute afd cs (x:xs) = compute afd ns xs
  where ns = nextState afd cs x


--afdFromAFN :: AFN.AFN -> AFD
--afdFromAFN afn = afd1


powerset s
    | s == empty = singleton empty
    | otherwise = Data.Set.map (Data.Set.insert x) pxs `Data.Set.union` pxs
      where (x, xs) = deleteFindMin s
            pxs = powerset xs

afd1 = afd [1,2] ['0','1'] foo 1 [2] 

foo :: (Int,Int) -> Int
foo (x,y) 
  | x == 1 && y == 1 = 1
  | x == 1 && y == 2 = 2
  | x == 2 && y == 1 = 1
  | x == 2 && y == 2 = 2
  | otherwise = 0
