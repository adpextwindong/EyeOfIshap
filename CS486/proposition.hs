{-# LANGUAGE DeriveFunctor #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe ( fromJust )
import Control.Monad ( forM_ )

--https://www.youtube.com/watch?v=5fZ_RnhvGMQ

data Prop a = PVar a
            | POr (Prop a) (Prop a)
            | PAnd (Prop a) (Prop a)
            | PNot (Prop a)
            | PImplies (Prop a) (Prop a)
            deriving (Functor, Show)

--Throws error if prop var is missing
val :: M.Map Char Bool -> Prop Char -> Prop Bool
val table = fmap (fromJust . flip M.lookup table)

eval :: Prop Bool -> Bool
eval (PVar b) = b
eval (POr p q) = eval p || eval q
eval (PAnd p q) = eval p && eval q
eval (PNot p) = not $ eval p
eval (PImplies p q) = case eval p of
                        False -> True
                        True -> eval q

{-
    Falsifying p -> q requires p to be satisfied before even considering p -> q to be false

    "If you try hard for your exam, then you will succeed"

    We can only know this implication is false if you try hard in the firstplace.
-}

--https://www.youtube.com/watch?v=tACXuzfXzSI&list=PLBlnK6fEyqRhqJPDXcvYlLfXPh37L89g3&index=6

collectPvars :: Ord a => Prop a -> S.Set a
collectPvars (PVar c) = S.singleton c
collectPvars (PNot p) = collectPvars p
collectPvars (POr p q) = S.union (collectPvars p) (collectPvars q)
collectPvars (PAnd p q) = S.union (collectPvars p) (collectPvars q)
collectPvars (PImplies p q) = S.union (collectPvars p) (collectPvars q)

tx :: Prop Char
tx = POr (PImplies (PAnd (PVar 'p') (PVar 'q')) (PVar 'r'))
         (PImplies (PNot (PVar 'p')) (PVar 's'))

truthTable :: Prop Char -> IO ()
truthTable f = do
                 print pvars
                 forM_ (zip3 tablePermutations vals (fmap eval vals)) print
    where
        pvars = S.toList $ collectPvars f
        tablePermutations = allPropositions pvars
        vals = fmap ((\p -> p f) . val . M.fromList) tablePermutations 

--https://stackoverflow.com/a/29711470
allPropositions :: [a] -> [[(a, Bool)]]
allPropositions = mapM (\v -> [(v, True),(v, False)])