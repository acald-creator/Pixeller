{-# LANGUAGE FlexibleInstances #-}

module Pretty
  ( ppexpr
  ) where

-----------------------------------------------------------------------------------
import Syntax

import Text.PrettyPrint (Doc, (<+>), char, hsep, parens, render, sep, text)

-----------------------------------------------------------------------------------
class Pretty p where
  ppr :: Int -> p -> Doc

-----------------------------------------------------------------------------------
--- Define ppr
-----------------------------------------------------------------------------------
instance Pretty Name where
  ppr _ x = text x

instance Pretty Expr where
  ppr _ (Var x) = text x
  ppr _ (Lit (LInt a)) = text (show a)
  ppr _ (Lit (LBool b)) = text (show b)
  ppr p e@(App _ _) = parensIf (p > 0) (ppr p f <+> sep (map (ppr (p + 1)) xs))
    where
      (f, xs) = viewApp e
  ppr p e@(Lam _ _) =
    parensIf (p > 0) $ (char '\\' <> hsep vars) <+> text "." <+> body
    where
      vars = map (ppr 0) (viewVars e)
      body = ppr (p + 1) (viewBody e)

-----------------------------------------------------------------------------------
--- Create two helper functions that collapse lambda bindings
--- so they can be printed out as single lambda expressions
-----------------------------------------------------------------------------------
viewVars :: Expr -> [Name]
viewVars (Lam n a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x = x

viewApp :: Expr -> (Expr, [Expr])
viewApp (App e1 e2) = go e1 [e2]
  where
    go (App a b) xs = go a (b : xs)
    go f xs = (f, xs)
viewApp _ = error "not application"

-----------------------------------------------------------------------------------
--- Create a helper function for parenthesizing subexpressions
-----------------------------------------------------------------------------------
parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

-----------------------------------------------------------------------------------
ppexpr :: Expr -> String
ppexpr = render . ppr 0
-----------------------------------------------------------------------------------
