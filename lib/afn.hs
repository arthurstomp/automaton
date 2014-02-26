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
, indexState
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

-- Define AFN como uma quintupla composta por estados, alfabeto, transições, estado inicial e estados finais.
data AFN = AFN { states :: [Int]
               , alphabet :: [Char]
               , transition :: Matrix (Set Int)
               , initialState :: Int
               , finalStates :: [Int]
               } deriving (Show)

-- Contrutor de um AFN
afn :: [Int] -> [Char] -> Matrix (Set Int) -> Int -> [Int] -> AFN
afn s a t is fs = AFN { states = s
                       , alphabet = a
                       , transition = t
                       , initialState = is
                       , finalStates = fs
                       }

-- Processa um AFN a partir de seu estado inicial
compute :: AFN -> [Char] -> Bool
compute afn word = computeProcessor afn (prettyInitialState afn) word

-- Função responsável pela lógica de processamento
computeProcessor :: AFN -> Set Int -> [Char] -> Bool
computeProcessor afn cs [] = accept afn cs
computeProcessor afn cs (x:xs) = computeProcessor afn ns xs
  where ns = nextState afn cs x
        
-- Função que checa se o estado atual, cs == currentState, é final ou se pode ser alcançado com transições vazias.
accept :: AFN -> Set Int -> Bool
accept afn cs = do let tc = transitiveClosure afn cs
                   let u = Data.Set.union cs tc
                   let acceptance = [ Data.List.elem x $ finalStates afn | x <- Data.Set.elems u] 
                   True `Data.List.elem` acceptance 

-- Informa o próximo estado dado um estado atual e uma letra lida.
nextState :: AFN -> Set Int -> Char -> Set Int
nextState afn cs c = do let transitiveClosureCs = transitiveClosure afn cs
                        let u = Data.Set.union cs transitiveClosureCs
                        Data.Set.unions [transitionFromState afn x c | x <- Data.Set.elems u]

-- Função que retorna o feixo transitivo para um dado estado.
-- Transitive Closure = Every node reached with a E.
transitiveClosure :: AFN -> Set Int -> Set Int
transitiveClosure afn cs = transitiveClosureProcessor afn cs cs
-- Função que realiza o processamento do feixo transitivo.
-- transitiveClosureProcessor :: AFN -> initialSet -> initialSetToBeRemoved -> Return
transitiveClosureProcessor :: AFN -> Set Int -> Set Int -> Set Int
transitiveClosureProcessor afn cs toBeRemoved
  | Data.Set.isSubsetOf tE cs = cs 
  | otherwise = do let u = Data.Set.union tE cs
                   let ncs = Data.Set.difference u toBeRemoved
                   transitiveClosureProcessor afn ncs Data.Set.empty
  where tE = Data.Set.unions [transitionWithE afn x | x <- Data.Set.elems cs]

-- Retorna o proximo estado alcançado pela transição vazia 'E'
-- Transition with E = next states reached with a empty transition 
transitionWithE :: AFN -> Int -> Set Int
transitionWithE afn s = transitionMatrix Data.Matrix.! (statePosition,ePosition)
  where statePosition = 1 + (indexState afn s)
        ePosition = 1 + (indexTransition afn 'E')
        transitionMatrix = transition afn

-- Transições de um dado estdo atual com um letra
transitionFromState :: AFN -> Int -> Char -> Set Int
transitionFromState afn cs t
  | ns == Data.Set.empty = Data.Set.empty
  | ns /= Data.Set.empty = ns
  where it = indexTransition afn t
        is = indexState afn cs
        ns = (transition afn) Data.Matrix.! (is+1,it+1)

-- Índice do character na matriz de transição de um afn
indexTransition :: AFN -> Char -> Int
indexTransition afn t = Data.Maybe.fromMaybe (-1) $ Data.List.elemIndex t a
  where a = alphabet afn

-- Índice de um estado na matriz de transição de um afn
indexState :: AFN -> Int -> Int
indexState afn cs = Data.Maybe.fromMaybe (-1) $ Data.List.elemIndex cs s
  where s = states afn

-- Transforma o estado inicial de um afn em um Data.Set
prettyInitialState :: AFN -> Set Int
prettyInitialState afn = Data.Set.singleton $ initialState afn

-- Operations 
--  Simple
-- Simple afn que aceita só um letra
simple :: Char -> AFN
-- Gera um afn que não aceita palavra nenhuma
simple 'V' = afn s alpha t is fs
  where s = [1]
        a = []
        t = Data.Matrix.fromLists [[Data.Set.empty]]
        is = 1
        fs = []
        alpha = a Data.List.++ "E"
-- Gera afn que aceita qualquer estado
simple 'E' = afn s alpha t is fs
  where s = [1]
        a = []
        t = Data.Matrix.fromLists [[Data.Set.singleton 1]]
        is = 1
        fs = [is]
        alpha = a Data.List.++ "E"
-- Gera que aceita a letra c
simple c =  afn s alpha t is fs
  where s = [1,2]
        a = [c]
        t = Data.Matrix.fromLists [[Data.Set.singleton 2, Data.Set.empty],[Data.Set.empty, Data.Set.empty]]
        is = 1
        fs = [2]
        alpha = a Data.List.++ "E"

--  Or
-- Realiza a operação de ou entre dois afn
or :: AFN -> AFN -> AFN
or afn1 afn2 = afn s a t is fs
  where s = orStates afn1 afn2
        a = orAlphabet afn1 afn2
        t = orTransition afn1 afn2 
        is = orInitialState afn1 afn2
        fs = orFinalStates afn1 afn2

-- Estados do ou entre dois afns
orStates :: AFN -> AFN -> [Int]
orStates afn1 afn2 = (s1 Data.List.++ ns2) Data.List.++ [nis]
  where s1 = states afn1
        s2 = states afn2
        l = Data.List.last s1
        ns2 = Data.List.map (l + ) s2
        nis = (Data.List.last ns2) + 1

-- Alfabeto do ou entre dois afns
orAlphabet afn1 afn2 = alpha
  where a1 = alphabet afn1
        a2 = alphabet afn2
        a = Data.List.delete 'E' (removeDuplicates (a1 Data.List.++ a2)) -- Remove os E do alfabeto
        alpha = a Data.List.++ "E" -- Adiciona E ao final do alfabeto

-- Remove duplicatas de uma lista
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
  where rdHelper seen [] = seen
        rdHelper seen (x:xs)
          | x `Data.List.elem` seen = rdHelper seen xs
          | otherwise = rdHelper (seen Data.List.++ [x]) xs

-- Transições do or de dois afns
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

-- Transição de um estado utilizado para a contrução da matriz de transição das operações
operationTransitionFromState :: AFN -> Int -> Char -> Set Int
operationTransitionFromState afn cs t
  | is == -1 || it == -1 = Data.Set.empty
  | ns == Data.Set.empty = Data.Set.empty
  | ns /= Data.Set.empty = ns
  where it = indexTransition afn t
        is = indexState afn cs
        ns = (transition afn) Data.Matrix.! (is+1,it+1)

-- Estado inicial do ou de dois afns
orInitialState :: AFN -> AFN -> Int
orInitialState afn1 afn2 = lnfs2 + 1
  where l = Data.List.last $ states afn1
        nfs2 = Data.List.map (l + ) $ finalStates afn2
        lnfs2 = Data.List.last nfs2

-- Estados finais do ou de dois afns
orFinalStates :: AFN -> AFN -> [Int]
orFinalStates afn1 afn2 = fs1 Data.List.++ nfs2
  where l = Data.List.last $ states afn1
        nfs2 = Data.List.map (l + ) $ finalStates afn2
        fs1 = finalStates afn1

--  Concatenation
--  Concatenando dois afns
concat ::  AFN -> AFN -> AFN
concat afn1 afn2 = afn s a t is fs
  where s = concatStates afn1 afn2
        a = concatAlphabet afn1 afn2
        t = concatTransition afn1 afn2
        is = concatInitialState afn1 afn2
        fs = concatFinalStates afn1 afn2

-- Estados da concatenação de dois afns
concatStates :: AFN -> AFN -> [Int] 
concatStates afn1 afn2 = s1 Data.List.++ ns2 
  where s1 = states afn1
        s2 = states afn2
        l = Data.List.last s1
        ns2 = Data.List.map (l + ) s2

-- Alfabeto da concatenação de dois afns
concatAlphabet :: AFN -> AFN -> [Char]
concatAlphabet afn1 afn2 = alpha
  where a1 = alphabet afn1
        a2 = alphabet afn2
        a = Data.List.delete 'E' (removeDuplicates (a1 Data.List.++ a2))
        alpha = a Data.List.++ "E"

-- Transição para a concatenação de dois afns
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

-- Estado inicial para a concatenação de dois afns
concatInitialState :: AFN -> AFN -> Int
concatInitialState afn1 afn2 = initialState afn1

-- Estados finais para a concatenação de dois afns
concatFinalStates :: AFN -> AFN -> [Int]
concatFinalStates afn1 afn2 = Data.List.map (l+) $ finalStates afn2
  where l = Data.List.last $ states afn1 

--  Star
-- Operaçãodo feixo de kleene sobre um afn
star :: AFN -> AFN
star af = afn s a t is fs
  where s = starStates af
        a = starAlphabet af
        t = starTransition af
        is = starInitialState af
        fs = starFinalState af

-- Estados para a operação estrela sobre um afn
starStates :: AFN -> [Int]
starStates afn = s Data.List.++ [ls]
  where s = states afn
        ls = (Data.List.last s) + 1

-- Alfabeto para a operação estrela sobre um afn
starAlphabet :: AFN -> [Char]
starAlphabet afn = alphabet afn

-- Transição para a operação estrela sobre um afn
starTransition :: AFN -> Matrix (Set Int)
starTransition afn = do let t = transition afn
                        let alpha = alphabet afn
                        let tWithNewInitialState = addNewInitialState afn t alpha
                        let indexFinalStates = [indexState afn x | x <- finalStates afn]
                        finalStatesToNewInitialState afn tWithNewInitialState indexFinalStates

-- Adiciona um estado inicial a matriz de uma afn
addNewInitialState :: AFN -> Matrix (Set Int) -> [Char] -> Matrix (Set Int)
addNewInitialState afn t a = Data.Matrix.fromLists (rt Data.List.++ [nr])
  where st = Data.Matrix.nrows t
        rt = [Data.Vector.toList (Data.Matrix.getRow x t) | x <- [1..st]]
        nr = [if c == 'E' then Data.Set.singleton is else Data.Set.empty | c <- a]
        is = initialState afn

-- Leva uma transição 'E' de todos os estados finais anteriores para o novo estado inicial
finalStatesToNewInitialState :: AFN -> Matrix (Set Int) -> [Int] -> Matrix (Set Int)
finalStatesToNewInitialState afn t [x] = do let indexOfE = indexTransition afn 'E'
                                            let fsTransition = transitionFromState afn (x+1) 'E'
                                            let newFsTransition = Data.Set.insert (starInitialState afn) fsTransition
                                            Data.Matrix.setElem newFsTransition (x+1,indexOfE+1) t
finalStatesToNewInitialState afn t (x:xs) = do let indexOfE = indexTransition afn 'E'
                                               let fsTransition = transitionFromState afn (x+1) 'E'
                                               let newFsTransition = Data.Set.insert (starInitialState afn) fsTransition
                                               let newTransition = Data.Matrix.setElem newFsTransition (x+1,indexOfE+1) t
                                               finalStatesToNewInitialState afn newTransition xs

-- Estado Inicial para a operação estrela sobre um afn
starInitialState :: AFN -> Int
starInitialState afn = 1 + (Data.List.last $ states afn)

-- Estado Final para a operação estrela sobre um afn
starFinalState :: AFN -> [Int]
starFinalState afn = [1 + (Data.List.last $ states afn)]

