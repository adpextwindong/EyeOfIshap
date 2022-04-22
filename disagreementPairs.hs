{- Lecture 18-3 Robinson algorithm for most general unifier
 - https://www.youtube.com/watch?v=P8ocbTJgeGo
 -}

import Data.Map
import Debug.Trace
import Data.Either

data Expr = Const Char
          | Fn Char [Expr]
          | Var String
          deriving (Show, Eq)

foo :: Expr
foo = Fn 'f' [Var "x1", Var "x3", Var "x2"]

bar :: Expr
bar = Fn 'f' [Fn 'g' [Var "x2"],
              Fn 'j' [Var "x4"],
              Fn 'h' [Var "x3", Const 'a']]

{- mgu( f(x1,x3,x2) , f(g(x2),j(x4), h(x3,a)) )
 -
 - x1 -> g(x2)
 - x3 -> j(X4)
 - x2 -> h(x3,a) --> h(j(x4),a)
 -               ---> x1 -> g(h(j(x4),a))
 -
 - Final submap should be
 -
 - x3 -> j(x4)
 - x2 -> h(j(x4,a))
 - x1 -> g(h(j(x4),a))
 -
 - Right (fromList [("x1",Fn 'g' [Fn 'h' [Fn 'j' [Var "x4"],Const 'a']]),("x2",Fn 'h' [Fn 'j' [Var "x4"],Const 'a']),("x3",Fn 'j' [Var "x4"])])
 -}

type Subst = Map String Expr

result :: Either String Subst
result = mgu foo bar

test = substitute (fromRight empty result) foo == substitute (fromRight empty result) bar

disagreementPairs :: Expr -> Expr -> [(Expr, Expr)]
disagreementPairs (Fn _ xs) (Fn _ ys) = [(x,y) | x <- xs, y <- ys, x /= y]
disagreementPairs x y | x /= y = [(x,y)]
                      | otherwise = []

occursIn :: String -> Expr -> Bool
occursIn s (Var s') = s == s'
occursIn s (Fn _ xs) = or $ occursIn s <$> xs
occursIn _ _ = False

disagreesWith :: Expr -> Expr -> Either String Subst
disagreesWith l r = Left $ show l <> " disagrees with " <> show r

mgu :: Expr -> Expr -> Either String Subst
mgu l@(Var s) r@(Var s') = if s == s'
                           then return empty
                           else l `disagreesWith` r

mgu l@(Var s) r | l == r = return empty
                | otherwise = if s `occursIn` r
                              then Left $ "Occurs of " <> s <> " check failed"
                              else Right $ singleton s r

mgu l r@(Var s) | l == r = return empty
                | otherwise = if s `occursIn` l
                              then Left $ "Occurs of " <> s <> " check failed"
                              else Right $ singleton s l

mgu l@(Fn s _) r@(Fn s' _) | s /= s' = l `disagreesWith` r
                           | otherwise = mgu' l r empty

mgu l r | l == r = return empty
        | otherwise = l `disagreesWith` r

mgu' :: Expr -> Expr -> Subst -> Either String Subst
--mgu' l r s | trace ("L: " <> show l <> "\nR: " <> show r <> "\nS: " <> show s <> "\n\n") False = undefined
mgu' (Fn s (x:xs)) (Fn s' (y:ys)) submap | x == y = mgu' (Fn s xs) (Fn s' ys) submap
                                         | otherwise = do
                                                          newSubs <- mgu x y
                                                          mgu' (Fn s xs) (Fn s' ys) $ updateSubmapWith submap $ fmap (substitute submap) newSubs


mgu' (Fn _ []) (Fn _ []) submap = return submap
mgu' l r _ = l `disagreesWith` r

updateSubmapWith :: Subst -> Subst -> Subst
updateSubmapWith submap newSubs = union (fmap (substitute newSubs) submap) newSubs

substitute :: Subst -> Expr -> Expr
substitute submap e@(Var s) = findWithDefault e s submap
substitute submap (Fn s xs) = Fn s $ fmap (substitute submap) xs
substitute _ e = e
