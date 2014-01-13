-- Functions for read a Regular Expression and compute words.
module Regex(
regexReader
) where

import AFN
import Data.Maybe

-- srr = Simple Regex Reader
srr :: (Maybe AFN, [Char]) -> (Maybe AFN, [Char])
srr (Nothing, []) = (Just $ simple 'E', [])
srr (Nothing, [x]) = (Just $ simple x, [])
srr (Nothing, (x:xs))
  | notElem x ['.','+','*','(',')'] = srr (Just $ simple x, xs)
  | x == '(' = srr (Nothing,xs)
srr (Just afn,[]) = (Just afn,[])
srr (Just afn, (x:xs))
  | x == '*' = srr (Just $ star afn, xs)
  | x == ')' = (Just afn,xs)
  | x == '.' = do let (a,ys) = srr (Nothing,xs)
                  let justA = fromJust a
                  let c = AFN.concat afn justA
                  srr (Just c,ys)
  | x == '+' = do let (a,ys) = srr (Nothing,xs)
                  let justA = fromJust a
                  let o = AFN.or afn justA
                  srr (Just o, ys)

regexReader :: [Char] -> AFN
regexReader regex = do let (afn, rest) = srr (Nothing, regex)
                       fromJust afn
