module ReadCFG(readCFG, removeSpaces) where

import Data.List.Split
import Data.List
import Data.Maybe

readCFG :: [Char] -> ([[Char]], [Char], [Char])

readCFG cgfInLine = do let productions = getProductions $ removeSpaces cgfInLine
                       let (variables,terminals) = getVariablesAndTerminals productions
                       (productions, variables, terminals)

getProductions :: [Char] -> [[Char]]
getProductions cfgInLine = splitOn "," cfgInLine

getVariablesAndTerminals :: [[Char]] -> ( [Char] , [Char] )
getVariablesAndTerminals productions = do let variablesAndTerminals = [ x | p <- productions, x <- splitOn "->" p ]
                                          let (v,t) = ReadCFG.split variablesAndTerminals
                                          let variables = concat $ remove_dups v
                                          let tAux = [deleteElem [va] ta | va <- variables, ta <- t]
                                          let terminals = concat tAux
                                          (variables,terminals)

deleteElem e [] = []                       
deleteElem e (x:xs)                        
  | [x] == e = deleteElem e xs             
  | otherwise = x : deleteElem e xs

remove_dups :: (Ord a, Eq a) => [a] -> [a]
remove_dups xs = remove $ Data.List.sort xs
 where remove []  = []
       remove [x] = [x]
       remove (x1:x2:xs)
        | x1 == x2  = remove (x1:xs)
        | otherwise = x1 : remove (x2:xs)

split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = (x:xp, y:yp) where (xp, yp) = ReadCFG.split xs

removeSpaces [] = []
removeSpaces [x]
  | x == ' ' = []
  | otherwise = [x]
removeSpaces (x:xs)
  | x == ' ' = removeSpaces xs
  | otherwise = x : removeSpaces xs
