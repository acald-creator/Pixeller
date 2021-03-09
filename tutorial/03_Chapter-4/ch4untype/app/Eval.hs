module Eval where

import Syntax

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr (Eval.Scope)

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure {} = "<<closure>>"

data EvalState =
  EvalState
    { depth :: Int
    }
  deriving (Show)

