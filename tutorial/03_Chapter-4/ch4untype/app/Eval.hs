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

---------------------------------------------------------------------------------
type Step = (Int, Expr)

type Eval a = WriterT [Step] (State EvalState) a

type Scope = Map.Map String Value

eval :: Eval.Scope -> Expr -> Eval Value
eval env expr =
  case expr of
    Lit (LInt x) -> do
      return $ VInt (fromIntegral x)
    Lit (LBool x) -> do
      return $ VBool x
    Var x -> do
      red expr
      return $ env Map.! x
    Lam x body -> inc $ do return (VClosure x body env)
    App a b ->
      inc $ do
        x <- eval env a
        red a
        y <- eval env b
        red b
        apply x y
