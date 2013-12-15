--Functions for AFN

import Data.Set
import Data.Matrix
import Data.List

data AFN = AFN { states :: Set Int
               , alphabet :: Set Char
               , transition :: Matrix (Maybe (Set Int))
               , initialState :: Int
               , finalStates :: Set Int
               } deriving (Show)

afd :: [Int] -> [Char] -> ((Int,Int) -> Maybe (Set Int)) -> Int -> [Int] -> AFN
afd s a ft is fs = AFN { states = Data.Set.fromList s
                       , alphabet = Data.Set.fromList a
                       , transition = t
                       , initialState = is
                       , finalStates = (Data.Set.fromList fs)
                       }
  where t = Data.Matrix.matrix (length s) (length a) $ ft

test :: Maybe (Set Int)
test = Just $ Data.Set.fromList [1,2,3]
--
--accept :: AFD -> Int -> Bool
--accept afd cs = Data.Set.member cs $ finalStates afd
--
--nextState :: AFD -> Int -> Char -> Int
--nextState afd cs t = do 
--  case Data.List.elemIndex t $ toAscList $ alphabet afd of
--    Just n -> m Data.Matrix.! (cs,n+1)
--    Nothing -> -1
--  where m = transition afd
--
--compute :: AFD -> [Char] -> Bool
--compute afd chain = accept afd $ last xs
--  where xs = Data.List.map (nextState afd $ initialState afd) chain
--
--test :: AFD 
--test = AFD { states = Data.Set.empty
--           , alphabet = Data.Set.empty
--           , transition = Data.Matrix.fromList 0 0 []
--           , initialState = 0
--           , finalStates = Data.Set.empty
--           }
--
--afd1 = afd [1,2] ['0','1'] foo 1 [2] 
--
--foo :: (Int,Int) -> Int
--foo (x,y) 
--  | x == 1 && y == 1 = 1
--  | x == 1 && y == 2 = 2
--  | x == 2 && y == 1 = 1
--  | x == 2 && y == 2 = 2
--  | otherwise = 0
