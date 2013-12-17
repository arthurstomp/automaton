import Prelude hiding (map)
powerset s
    | s == empty = singleton empty
    | otherwise = map (insert x) pxs `union` pxs
      where (x, xs) = deleteFindMin s
            pxs = powerset xs