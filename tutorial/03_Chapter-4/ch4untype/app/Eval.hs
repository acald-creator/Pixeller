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

inc :: Eval a -> Eval a
inc m = do
  modify $ \s -> s {depth = (depth s) + 1}
  out <- m
  modify $ \s -> s {depth = (depth s) - 1}
  return out

red :: Expr -> Eval ()
red x = do
  d <- gets depth
  tell [(d, x)]
  return ()
