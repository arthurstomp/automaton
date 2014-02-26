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
  | notElem x ['.','+','*','(',')'] = do if xs == "" then srr (Just $ simple x, xs)
                                         else do let (y:ys) = xs
                                                 if y == '*' then srr (Just $ star $ simple x,ys)
                                                 else srr (Just $ simple x, xs)
  | x == '(' = do let (p,s) = srr (Nothing,xs)
                  if s == "" then (p,s) 
                  else do let (y:ys) = s
                          if y == '*' then srr (Just $ star $ fromJust p,ys)
                          else srr (p,ys)
srr (Just afn,[]) = (Just afn,[])
srr (Just afn,['*']) = (Just afn, ['*'])
srr (Just afn, (x:xs))
  | x == '*' = (Just afn,(x:xs))
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
