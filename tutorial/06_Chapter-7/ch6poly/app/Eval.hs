module Eval where

import Syntax

import Control.Monad.Identity
import qualified Data.Map as Map

----------------------------------------------------------------------
type TermEnv = Map.Map String Value

type Interpreter t = Identity t

----------------------------------------------------------------------
data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr TermEnv

emtpyTmenv :: TermEnv
emtpyTmenv = Map.empty

----------------------------------------------------------------------
instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure {} = "<<closure>>"

----------------------------------------------------------------------
eval :: Eval.Scope -> Expr -> Identity Value
eval env expr =
  case expr of
    Lit (LInt x) -> return $ VInt (fromIntegral x)
    Lit (LBool x) -> return $ VBool x
    Var x -> return $ env Map.! x
    Lam x _ body -> return (VClosure x body env)
    App a b -> do
      x <- eval env a
      y <- eval env b
      apply x y

runEval :: Expr -> Value
runEval x = runIdentity (eval emptyScope x)
----------------------------------------------------------------------
