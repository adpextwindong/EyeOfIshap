import qualified Data.Map.Strict as M
import Data.Bifunctor
import Data.Foldable (foldr)

data Term v
  = Var v
  | Lam v (Term v)
  | App (Term v) (Term v)
  deriving Show

toDebruijn :: Term Char -> Term Int
toDebruijn = toDebruijn' M.empty

prettyPrint :: Term Char -> IO ()
prettyPrint = putStrLn . pretty

pretty :: Term Char -> String
pretty = pretty' . toDebruijn

pretty' :: Term Int -> String
pretty' (Var i)   = show i
pretty' (Lam _ t) = "λ." <> pretty' t
pretty' (App l r) = "(" <> pretty' l <> " " <> pretty' r <> ")"

toDebruijn' :: M.Map Char Int -> Term Char -> Term Int
toDebruijn' m (Var v) = Var (m M.! v)
toDebruijn' m (App l r) = App (toDebruijn' m l) (toDebruijn' m r)
toDebruijn' m (Lam b t) = Lam 0 (toDebruijn' (M.insert b 0 (incrM m)) t)
  where
    incrM = fmap (+ 1) :: M.Map Char Int -> M.Map Char Int

tx :: Term Char
tx = Lam 'x' (Lam 'y' (Var 'x'))

ty :: Term Char
ty = Lam 'x' (Lam 'y' (Lam 'z' (App (App (Var 'x') (Var 'y')) (Var 'z'))))

tz = App ty ty

y_comb = Lam 'f' (App (Lam 'x' (App (Var 'f') (App (Var 'x') (Var 'x')))) (Lam 'x' (App (Var 'f') (App (Var 'x') (Var 'x')))))
