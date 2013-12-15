--Functions for AFD

import Data.Set
import Data.Matrix

data AFD a = AFD { states :: Set a
               , alphabet :: Set a
               , transition :: Matrix a
               , initialState :: Integer
               , finalStates :: Set a
               } deriving (Show)

test :: AFD a 
test = AFD {states = Data.Set.empty, alphabet = Data.Set.empty, transition = Data.Matrix.fromList 0 0 [], initialState = 0, finalStates = Data.Set.empty}
