--Functions for AFN

module AFN
( afn
, AFN
, compute
, afn1
, states
, alphabet
, initialState
, finalStates
, transition
, nextState
, partialNextState
, transitiveClosure
, transitionFromState
, indexTransition 
)where

import Data.Set
import Data.Matrix
import Data.List
import Data.Maybe
import Data.Vector

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
  where alpha = a Data.List.++ "E"

compute :: AFN -> Set Int -> [Char] -> Bool
compute afn cs [] = accept afn cs
compute afn cs (x:xs) = compute afn ns xs
  where ns = nextState afn cs x

accept :: AFN -> Set Int -> Bool
accept afn cs  = True `Data.List.elem` acceptance 
  where acceptance = [ Data.List.elem x $ finalStates afn | x <- Data.Set.elems cs] 

nextState :: AFN -> Set Int -> Char -> Set Int
nextState afn cs t = Data.Set.union (partialNextState afn cs t) (transitiveClosure afn cs)

transitiveClosure :: AFN -> Set Int -> Set Int
transitiveClosure afn cs = partialNextState afn cs 'E'

partialNextState :: AFN -> Set Int -> Char -> Set Int
partialNextState afn cs c = Data.Set.unions [transitionFromState afn x c | x <- Data.Set.elems cs]

transitionFromState :: AFN -> Int -> Char -> Set Int
transitionFromState afn cs t
  | ns == Data.Set.empty = Data.Set.singleton cs 
  | ns /= Data.Set.empty = ns
  where it = indexTransition afn t
        is = indexState afn cs
        ns = (transition afn) Data.Matrix.! (is+1,it+1)

indexTransition :: AFN -> Char -> Int
indexTransition afn t = Data.Maybe.fromJust $ Data.List.elemIndex t a
  where a = alphabet afn

indexState :: AFN -> Int -> Int
indexState afn cs = Data.Maybe.fromJust $ Data.List.elemIndex cs s
  where s = states afn

-- Operations 
--  Simple
simple :: Char -> AFN
simple c =  afn s a t is fs
  where s = [1,2]
        a = [c]
        t = Data.Matrix.fromLists [[Data.Set.singleton 2, Data.Set.empty],[Data.Set.empty, Data.Set.empty]]
        is = 1
        fs = [2]
        alpha = a Data.List.++ "E"

--  Or
--or :: AFN -> AFN -> AFN
--or afn1 afn2 = 

orStates :: AFN -> AFN -> [Int]
orStates afn1 afn2 = nis : (s1 Data.List.++ ns2)
  where s1 = states afn1
        s2 = states afn2
        l = Data.List.last s1
        ns2 = Data.List.map (l + ) s2
        nis = 0

orAlphabet afn1 afn2 = alpha
  where a1 = alphabet afn1
        a2 = alphabet afn2
        a = Data.List.delete 'E' (removeDuplicates (a1 Data.List.++ a2))
        alpha = a Data.List.++ "E"

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
  where rdHelper seen [] = seen
        rdHelper seen (x:xs)
          | x `Data.List.elem` seen = rdHelper seen xs
          | otherwise = rdHelper (seen Data.List.++ [x]) xs

--orTransition :: AFN -> AFN -> Matrix ( Set Int)
--orTransition afn1 afn2 =
--  where t1 = transition afn1
--        t2 = transition afn2
--        st1 = Data.Matrix.nrows t1
--        st2 = Data.Matrix.nrows t2
--        rt1 = [Data.Vector.toList (Data.Matrix.getRow x t1) | x <- [1..st1]]
--        rt2 = [Data.Vector.toList (Data.Matrix.getRow x t2) | x <- [1..st2]]
--        newInitialStateRow = 
        

orInitialState = 0

orFinalStates :: AFN -> AFN -> [Int]
orFinalStates afn1 afn2 = fs1 Data.List.++ nfs2
  where l = Data.List.last $ states afn1
        nfs2 = Data.List.map (l + ) $ finalStates afn2
        fs1 = finalStates afn1

--  Concatenation
--  Star
star :: AFN -> AFN
star af = afn s a t is fs
  where s = starStates af
        a = starAlphabet af
        t = starTransition af
        is = starInitialState af
        fs = starFinalState af

starStates :: AFN -> [Int]
starStates afn = [0] Data.List.++ s
  where s = states afn

starAlphabet :: AFN -> [Char]
starAlphabet afn = Data.List.delete 'E' (alphabet afn)

starTransition :: AFN -> Matrix (Set Int)
starTransition afn = ft
  where t = transition afn
        alpha = alphabet afn
        nt = addNewInitialState t alpha
        ifs = [indexState afn x | x <- finalStates afn]
        ft = finalStatesToNewInitialState afn nt ifs

addNewInitialState :: Matrix (Set Int) -> [Char] -> Matrix (Set Int)
addNewInitialState t a = Data.Matrix.fromLists (nr : rt)
  where st = Data.Matrix.nrows t
        rt = [Data.Vector.toList (Data.Matrix.getRow x t) | x <- [1..st]]
        nr = [if c == 'E' then Data.Set.singleton 1 else Data.Set.empty | c <- a]

finalStatesToNewInitialState :: AFN -> Matrix (Set Int) -> [Int] -> Matrix (Set Int)
finalStatesToNewInitialState afn t [x] = Data.Matrix.setElem nct (x+1,ie+1) t
  where ie = indexTransition afn 'E'
        ct = Data.Matrix.getElem x (ie+1) t 
        nct = Data.Set.insert 0 ct
finalStatesToNewInitialState afn t (x:xs) = finalStatesToNewInitialState afn nt xs
  where ie = indexTransition afn 'E'
        ct = Data.Matrix.getElem x (ie+1) t 
        nct = Data.Set.insert 0 ct
        nt = Data.Matrix.setElem nct (x+1,ie+1) t
        

starInitialState :: AFN -> Int
starInitialState afn = 0

starFinalState :: AFN -> [Int]
starFinalState afn = [0]

-- Testing Area
prettyInitialState :: AFN -> Set Int
prettyInitialState afn = Data.Set.singleton $ initialState afn

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
        alpha = a Data.List.++ "E"

is = prettyInitialState afn1
ns1 = nextState afn1 is '0'
ns2 = nextState afn1 ns1 '0'
ns3 = nextState afn1 ns2 '1'
