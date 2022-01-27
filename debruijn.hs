import Data.Map.Strict as M

data Term v
  = Var v
  | Lam v (Term v)
  | App (Term v) (Term v)
  deriving Show

prettyDebruijn :: Term Char -> Term Int
prettyDebruijn = toDebruijn M.empty

toDebruijn :: M.Map Char Int -> Term Char -> Term Int
toDebruijn m (Var v) = Var (m M.! v)
toDebruijn m (App l r) = App (toDebruijn m l) (toDebruijn m r)
toDebruijn m (Lam b t) = Lam 0 (toDebruijn (M.insert b 0 (incrM m)) t)
  where
    incrM = fmap (+ 1) :: M.Map Char Int -> M.Map Char Int

tx :: Term Char
tx = Lam 'x' (Lam 'y' (Var 'x'))

ty :: Term Char
ty = Lam 'x' (Lam 'y' (Lam 'z' (App (App (Var 'x') (Var 'y')) (Var 'z'))))
