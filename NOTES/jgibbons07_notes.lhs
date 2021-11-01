Notes on Jeremy Gibbons ["Functional Programming for Domain Specific Languages"](http://dx.doi.org/10.1007/978-3-319-15940-9_1)

Additional video: https://www.youtube.com/watch?v=1cR6a3g_Y10

\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

--Page 12.

data Expr :: * where
    ValI :: Integer -> Expr Integer       -- Expr Integer in this type forces the Kind Signature to be * -> *
    If :: Expr Bool -> Expr a -> Expr a -> Expr a

\end{code}
