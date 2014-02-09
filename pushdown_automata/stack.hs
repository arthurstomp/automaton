module Stack (Stack, empty, isEmpty, push, top, pop) where

empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)

type Stack a = [a] -- opaque!
empty = []
isEmpty stack = null stack
push x (xs) = x:xs
top stack = head stack
pop (x:xs) = (x,xs)
