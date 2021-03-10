module Syntax where

------------------------------------------------------------------------
--- Abstract Syntax Tree (extension from the Chapter 5 STLC application)
------------------------------------------------------------------------
type Name = String

type Decl = (String, Expr)

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop
  = Add
  | Sub
  | Mul
  | Eql
  deriving (Eq, Ord, Show)

data Program =
  Program [Decl] Expr
  deriving (Eq)
------------------------------------------------------------------------
