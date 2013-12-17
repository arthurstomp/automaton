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

-- Durante a passagem do estado eu preciso lembrar de não salvar estados Nothing
accept :: AFN -> Set (Maybe Int) -> Bool
accept afn cs  = True `elem` acceptance 
  where acceptance = [ Data.Set.member (Data.Maybe.fromJust x) $ finalStates afn | x <- Data.Set.elems cs] 

nextState :: AFN -> Set (Maybe Int) -> Char -> Set (Maybe Int)
nextState afn cs t = Data.Set.union (partialNextState afn cs t) (partialNextState afn cs 'E')

compute :: AFN  -> [Char] -> Bool
compute afn chain = True

partialNextState :: AFN -> Set (Maybe Int) -> Char -> Set (Maybe Int)
partialNextState afn cs t = Data.Set.unions [transitionFromState afn (Data.Maybe.fromJust x) t | x <- Data.Set.elems cs]

transitionFromState :: AFN -> Int -> Char -> Set (Maybe Int)
transitionFromState afn cs t = case ns of
  Just n -> Data.Set.map Just n
  Nothing -> Data.Set.empty
  where it = indexTransition afn t
        ns = (transition afn) Data.Matrix.! (cs,it+1)


indexTransition :: AFN -> Char -> Int
indexTransition afn t = Data.Maybe.fromJust $ Data.List.elemIndex t ascList
  where a = alphabet afn
        ascList = Data.Set.toAscList a 

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

--compute :: AFD -> [Char] -> Bool
--compute afd chain = accept afd $ last xs
--  where xs = Data.List.map (nextState afd $ initialState afd) chain
