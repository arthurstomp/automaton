--Functions for AFN

import Data.Set
import Data.Matrix
import Data.List
import Data.Maybe

data AFN = AFN { states :: Set Int
               , alphabet :: Set Char
               , transition :: Matrix (Maybe (Set Int))
               , initialState :: Int
               , finalStates :: Set Int
               } deriving (Show)

afn :: [Int] -> [Char] -> ((Int,Int) -> Maybe (Set Int)) -> Int -> [Int] -> AFN
afn s a ft is fs = AFN { states = Data.Set.fromList s
                       , alphabet = alpha
                       , transition = t
                       , initialState = is
                       , finalStates = (Data.Set.fromList fs)
                       }
  where t = Data.Matrix.matrix (length s) (Data.Set.size alpha) $ ft
        alpha = Data.Set.insert 'E' $ Data.Set.fromList a

foo :: (Int,Int) -> Maybe (Set Int)
foo (x,y)
  | x == 1 && y == 1 = Just $ Data.Set.singleton 1
  | x == 1 && y == 2 = Just $ Data.Set.fromList [1,2]
  | x == 1 && y == 3 = Nothing
  | x == 2 && y == 1 = Just $ Data.Set.singleton 3
  | x == 2 && y == 2 = Nothing
  | x == 2 && y == 3 = Just $ Data.Set.singleton 3
  | x == 3 && y == 1 = Nothing
  | x == 3 && y == 2 = Just $ Data.Set.singleton 4
  | x == 3 && y == 3 = Nothing
  | x == 4 && y == 1 = Just $ Data.Set.singleton 4
  | x == 4 && y == 2 = Just $ Data.Set.singleton 4
  | x == 4 && y == 3 = Nothing

afn1 = afn [1,2,3,4] ['0','1'] foo 1 [4] 

-- Durante a passagem do estado eu preciso lembrar de não salvar estados Nothing
accept :: AFN -> Set (Maybe Int) -> Bool
accept afn cs  = True `elem` acceptance 
  where acceptance = [ Data.Set.member (Data.Maybe.fromJust x) $ finalStates afn | x <- Data.Set.elems cs] 

--nextState :: AFN -> Maybe (Set Int) -> Char -> Int
--nextState afn mcs t = do 
--  case Data.List.elemIndex t $ toAscList $ alphabet afn of
--    Just n -> m Data.Matrix.! (cs,n+1)
--    Nothing -> -1
--  where m = transition afn
--        cs = fromMaybe Nothing mcs

--compute :: AFD -> [Char] -> Bool
--compute afd chain = accept afd $ last xs
--  where xs = Data.List.map (nextState afd $ initialState afd) chain
