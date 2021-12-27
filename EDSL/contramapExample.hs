-- https://www.foxhound.systems/blog/composable-data-validation/

{-# LANGUAGE InstanceSigs #-}
newtype ValidationRule a = ValidationRule { validate :: a -> Bool }

{-
instance Functor ValidationRule where
    fmap :: (a -> b) -> ValidationRule a -> ValidationRule b
    fmap f (ValidationRule v) = ValidationRule $ \actual ->
        validate rule???

    Fmap doesn't work because ValidationRule takes an a and produces a bool.

        validate :: (a -> Bool)

    When we want

        validate' :: (b -> Bool)

    composed out of the previous validate

    So we need some function (b -> a) to compose with validate to make validate'
-}

contramapV :: (b -> a) -> ValidationRule a -> ValidationRule b
contramapV f rule = ValidationRule $ \actual ->
    validate rule (f actual)

{-

This works as previously outlined. Because ValidationRule is a Contravariant Functor (Cofunctor) we can think of it as "consuming" values instead of "containing" values.

Therefore we need to convert the input (apply f) before "consuming" it.

-}

lt_ :: Ord a => a -> ValidationRule a
lt_ ruleValue = ValidationRule $ \actual -> actual < ruleValue

negative_ :: ValidationRule Integer
negative_ = lt_ 0

data Account = Account
    { accountBalance :: Integer
    }

overdrawn :: ValidationRule Account
overdrawn = contramapV accountBalance negative_

-- accountBalance :: Account -> Integer
-- negative_ :: ValidationRule Integer -- (Integer -> Bool)
-- contramapV accountBalance :: ValidationRule Integer -> ValidationRule Account
-- validate overdrawn :: Account -> Bool
