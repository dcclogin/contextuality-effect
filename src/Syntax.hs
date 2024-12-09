{-# LANGUAGE GADTs #-}

module Syntax where

type Name = String
data Expr where
    Var :: Name -> Expr
    Const :: Bool -> Expr
    -- Rand :: Int -> Expr
    NotE :: Expr -> Expr
    OrE :: Expr -> Expr -> Expr
    AndE :: Expr -> Expr -> Expr
    PairE :: Expr -> Expr -> Expr
    Fst :: Expr -> Expr
    Snd :: Expr -> Expr
    LetE :: Name -> Expr -> Expr -> Expr
    deriving (Show)

data Value where
    Error :: Value
    BoolV :: Bool -> Value
    PairV :: Value -> Value -> Value
    deriving (Show)

type Env = [(Name, Value)]
