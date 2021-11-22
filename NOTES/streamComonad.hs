data Stream a = Cons a (Stream a)
    deriving Show

unfoldStream :: s -> (s -> (a, s)) -> Stream a
unfoldStream initialState next = Cons a (unfoldStream nextState next)
    where
        (a, nextState) = next initialState
