--Functions for Autonomous Deterministic Finite

import Data.Set (Set)
import Data.Matrix (Matrix)
import System.Environment

data AFD s a t is fs = AFD { states :: s
                           , alphabet :: a
                           , transition :: t
                           , initialState :: is
                           , finalStates :: fs
                           } deriving (Show)

test = [1,2,3,4,5]
test

main :: IO()
main = getArgs >>= print . haqify . head
haqify s = "Haq! " ++ s
