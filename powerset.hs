module Powerset
(powerset
)where
import Prelude hiding (map)
import Data.Set
powerset s
    | s == empty = singleton empty
    | otherwise = map (insert x) pxs `union` pxs
      where (x, xs) = deleteFindMin s
            pxs = powerset xs
