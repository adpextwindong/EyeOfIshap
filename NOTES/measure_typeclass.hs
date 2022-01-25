{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}       --For 'Primitive
{-# LANGUAGE TypeFamilies #-}    --For KPrim
{-# LANGUAGE ConstraintKinds #-} --For KPrim t

data Image = Png
data Line = KLine
data Point = KPoint

data Primitive = 'Primitive

type family KPrim' a where
    KPrim' Line = 'Primitive
    KPrim' Point = 'Primitive 

type KPrim t = (KPrim' t ~ ' Primitive)

class Measure a
instance Measure Line

data KExpr a where
    K :: a -> KExpr a
    C :: ConstExpr a -> KExpr a
    KLength :: (Measure a) => KExpr a -> KExpr Float
    KRender :: (KPrim a) => KExpr a -> KExpr Image
    KConLine :: KExpr Point -> KExpr Point -> KExpr Line
    KAdd :: (Num a) => KExpr a -> KExpr a -> KExpr a

--Used with KExpr C constructor to provide labeled and typed constants.
--Pattern found from https://stackoverflow.com/a/37359542
data ConstExpr a where
    ConstF :: String -> ConstExpr Float