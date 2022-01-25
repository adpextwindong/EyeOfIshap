{-
https://hackage.haskell.org/package/fgl
http://web.engr.oregonstate.edu/~erwig/fgl/haskell/
http://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf
https://web.engr.oregonstate.edu/~erwig/fpg/
-}

--"More or less equiv according to codingbuddy"

class (Eq a) => Node a where
    gen :: Int -> [a]
    new :: [a] -> a

data GNode = GNode
    deriving Eq

instance Node GNode
--TODO
