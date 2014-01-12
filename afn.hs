--Functions for AFN

module AFN
( afn
, AFN
, compute
, states
, alphabet
, initialState
, finalStates
, transition
, nextState
, transitiveClosure
, transitionFromState
, transitionWithE
, indexTransition 
, simple
, AFN.or
, AFN.concat
, star
, prettyInitialState
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
                       , alphabet = a
                       , transition = t
                       , initialState = is
                       , finalStates = fs
                       }

compute :: AFN -> [Char] -> Bool
compute afn word = computeProcessor afn (prettyInitialState afn) word

computeProcessor :: AFN -> Set Int -> [Char] -> Bool
computeProcessor afn cs [] = accept afn cs
computeProcessor afn cs (x:xs) = computeProcessor afn ns xs
  where ns = nextState afn cs x
        

accept :: AFN -> Set Int -> Bool
accept afn cs = do let tc = transitiveClosure afn cs
                   let u = Data.Set.union cs tc
                   let acceptance = [ Data.List.elem x $ finalStates afn | x <- Data.Set.elems u] 
                   True `Data.List.elem` acceptance 

nextState :: AFN -> Set Int -> Char -> Set Int
nextState afn cs c = do let transitiveClosureCs = transitiveClosure afn cs
                        let u = Data.Set.union cs transitiveClosureCs
                        Data.Set.unions [transitionFromState afn x c | x <- Data.Set.elems u]

-- Transitive Closure = Every node reached with a E.
transitiveClosure :: AFN -> Set Int -> Set Int
transitiveClosure afn cs = transitiveClosureProcessor afn cs cs
-- transitiveClosureProcessor :: AFN -> initialSet -> initialSetToBeRemoved -> Return
transitiveClosureProcessor :: AFN -> Set Int -> Set Int -> Set Int
transitiveClosureProcessor afn cs toBeRemoved
  | Data.Set.isSubsetOf tE cs = cs 
  | otherwise = do let u = Data.Set.union tE cs
                   let ncs = Data.Set.difference u toBeRemoved
                   transitiveClosureProcessor afn ncs Data.Set.empty
  where tE = Data.Set.unions [transitionWithE afn x | x <- Data.Set.elems cs]

-- Transition with E = next states reached with a empty transition 
transitionWithE :: AFN -> Int -> Set Int
transitionWithE afn s = transitionMatrix Data.Matrix.! (statePosition,ePosition)
  where statePosition = 1 + (indexState afn s)
        ePosition = 1 + (indexTransition afn 'E')
        transitionMatrix = transition afn

transitionFromState :: AFN -> Int -> Char -> Set Int
transitionFromState afn cs t
  | ns == Data.Set.empty = Data.Set.empty
  | ns /= Data.Set.empty = ns
  where it = indexTransition afn t
        is = indexState afn cs
        ns = (transition afn) Data.Matrix.! (is+1,it+1)

indexTransition :: AFN -> Char -> Int
indexTransition afn t = Data.Maybe.fromMaybe (-1) $ Data.List.elemIndex t a
  where a = alphabet afn

indexState :: AFN -> Int -> Int
indexState afn cs = Data.Maybe.fromMaybe (-1) $ Data.List.elemIndex cs s
  where s = states afn

prettyInitialState :: AFN -> Set Int
prettyInitialState afn = Data.Set.singleton $ initialState afn

-- Operations 
--  Simple
simple :: Char -> AFN
simple 'V' = afn s alpha t is fs
  where s = [1]
        a = []
        t = Data.Matrix.fromLists [[Data.Set.empty]]
        is = 1
        fs = []
        alpha = a Data.List.++ "E"
simple 'E' = afn s alpha t is fs
  where s = [1]
        a = []
        t = Data.Matrix.fromLists [[Data.Set.singleton 1]]
        is = 1
        fs = [is]
        alpha = a Data.List.++ "E"
simple c =  afn s alpha t is fs
  where s = [1,2]
        a = [c]
        t = Data.Matrix.fromLists [[Data.Set.singleton 2, Data.Set.empty],[Data.Set.empty, Data.Set.empty]]
        is = 1
        fs = [2]
        alpha = a Data.List.++ "E"

--  Or
or :: AFN -> AFN -> AFN
or afn1 afn2 = afn s a t is fs
  where s = orStates afn1 afn2
        a = orAlphabet afn1 afn2
        t = orTransition afn1 afn2 
        is = orInitialState afn1 afn2
        fs = orFinalStates afn1 afn2

orStates :: AFN -> AFN -> [Int]
orStates afn1 afn2 = (s1 Data.List.++ ns2) Data.List.++ [nis]
  where s1 = states afn1
        s2 = states afn2
        l = Data.List.last s1
        ns2 = Data.List.map (l + ) s2
        nis = (Data.List.last ns2) + 1

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

orTransition :: AFN -> AFN -> Matrix ( Set Int)
orTransition afn1 afn2 = Data.Matrix.fromLists $  afn1Rows Data.List.++ afn2Rows Data.List.++ [lastRow]
  where lastRow = [ if c /= 'E' then Data.Set.empty else Data.Set.fromList [is1,is2] | c <- alpha]
        row1 s = [operationTransitionFromState afn1 s c | c <- alpha ]
        row2 s = [(Data.Set.map (l+) $ operationTransitionFromState afn2 s c) | c <- alpha ]
        afn1Rows = [row1 s | s <- states afn1]
        afn2Rows = [row2 s | s <- states afn2]
        alpha = orAlphabet afn1 afn2
        is1 = initialState afn1
        l = (Data.List.last $ states afn1)
        is2 = (initialState afn2) + l

operationTransitionFromState :: AFN -> Int -> Char -> Set Int
operationTransitionFromState afn cs t
  | is == -1 || it == -1 = Data.Set.empty
  | ns == Data.Set.empty = Data.Set.empty
  | ns /= Data.Set.empty = ns
  where it = indexTransition afn t
        is = indexState afn cs
        ns = (transition afn) Data.Matrix.! (is+1,it+1)

orInitialState :: AFN -> AFN -> Int
orInitialState afn1 afn2 = lnfs2 + 1
  where l = Data.List.last $ states afn1
        nfs2 = Data.List.map (l + ) $ finalStates afn2
        lnfs2 = Data.List.last nfs2

orFinalStates :: AFN -> AFN -> [Int]
orFinalStates afn1 afn2 = fs1 Data.List.++ nfs2
  where l = Data.List.last $ states afn1
        nfs2 = Data.List.map (l + ) $ finalStates afn2
        fs1 = finalStates afn1

--  Concatenation
concat ::  AFN -> AFN -> AFN
concat afn1 afn2 = afn s a t is fs
  where s = concatStates afn1 afn2
        a = concatAlphabet afn1 afn2
        t = concatTransition afn1 afn2
        is = concatInitialState afn1 afn2
        fs = concatFinalStates afn1 afn2

concatStates :: AFN -> AFN -> [Int] 
concatStates afn1 afn2 = s1 Data.List.++ ns2 
  where s1 = states afn1
        s2 = states afn2
        l = Data.List.last s1
        ns2 = Data.List.map (l + ) s2

concatAlphabet :: AFN -> AFN -> [Char]
concatAlphabet afn1 afn2 = alpha
  where a1 = alphabet afn1
        a2 = alphabet afn2
        a = Data.List.delete 'E' (removeDuplicates (a1 Data.List.++ a2))
        alpha = a Data.List.++ "E"

concatTransition :: AFN -> AFN -> Matrix (Set Int)
concatTransition afn1 afn2 = Data.Matrix.fromLists $  afn1Rows Data.List.++ afn2Rows 
  where row1 s = [if Data.List.elem s (finalStates afn1) && c == 'E' 
                  then Data.Set.union (Data.Set.singleton is2) (operationTransitionFromState afn1 s c)
                  else operationTransitionFromState afn1 s c | c <- alpha ]
        row2 s = [(Data.Set.map (l+) $ operationTransitionFromState afn2 s c) | c <- alpha ]
        afn1Rows = [row1 s | s <- states afn1]
        afn2Rows = [row2 s | s <- states afn2]
        alpha = orAlphabet afn1 afn2
        is1 = initialState afn1
        l = (Data.List.last $ states afn1)
        is2 = (initialState afn2) + l

concatInitialState :: AFN -> AFN -> Int
concatInitialState afn1 afn2 = initialState afn1

concatFinalStates :: AFN -> AFN -> [Int]
concatFinalStates afn1 afn2 = Data.List.map (l+) $ finalStates afn2
  where l = Data.List.last $ states afn1 

--  Star
star :: AFN -> AFN
star af = afn s a t is fs
  where s = starStates af
        a = starAlphabet af
        t = starTransition af
        is = starInitialState af
        fs = starFinalState af

starStates :: AFN -> [Int]
starStates afn = s Data.List.++ [ls]
  where s = states afn
        ls = (Data.List.last s) + 1

starAlphabet :: AFN -> [Char]
starAlphabet afn = alphabet afn

starTransition :: AFN -> Matrix (Set Int)
starTransition afn = ft
  where t = transition afn
        alpha = alphabet afn
        nt = addNewInitialState afn t alpha
        ifs = [indexState afn x | x <- finalStates afn]
        ft = finalStatesToNewInitialState afn nt ifs

addNewInitialState :: AFN -> Matrix (Set Int) -> [Char] -> Matrix (Set Int)
addNewInitialState afn t a = Data.Matrix.fromLists (rt Data.List.++ [nr])
  where st = Data.Matrix.nrows t
        rt = [Data.Vector.toList (Data.Matrix.getRow x t) | x <- [1..st]]
        nr = [if c == 'E' then Data.Set.singleton is else Data.Set.empty | c <- a]
        is = initialState afn

finalStatesToNewInitialState :: AFN -> Matrix (Set Int) -> [Int] -> Matrix (Set Int)
finalStatesToNewInitialState afn t [x] = Data.Matrix.setElem nct (x+1,ie+1) t
  where ie = indexTransition afn 'E'
--        ct = Data.Matrix.getElem x (ie+1) t 
        ct = transitionFromState afn x 'E'
        nct = Data.Set.insert nls ct
        nls = 1 + (Data.List.last $ states afn)
finalStatesToNewInitialState afn t (x:xs) = finalStatesToNewInitialState afn nt xs
  where ie = indexTransition afn 'E'
        ct = Data.Matrix.getElem x (ie+1) t 
        nct = Data.Set.insert nls ct
        nt = Data.Matrix.setElem nct (x+1,ie+1) t
        nls = 1 + (Data.List.last $ states afn)

starInitialState :: AFN -> Int
starInitialState afn = 1 + (Data.List.last $ states afn)

starFinalState :: AFN -> [Int]
starFinalState afn = [1 + (Data.List.last $ states afn)]

