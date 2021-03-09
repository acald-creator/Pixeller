module Pretty
  ( ppexpr
  ) where

----------------------------------------------------------------
import Syntax

import Text.PrettyPrint

----------------------------------------------------------------
class Pretty p where
  ppr :: Int -> p -> Doc
  pp :: p -> Doc
  pp = ppr 0

----------------------------------------------------------------
--- Create two helper functions that collapse lambda bindings
--- so they can be printed out as single lambda expressions
----------------------------------------------------------------
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

----------------------------------------------------------------
--- Create a helper function for parenthesizing subexpressions
----------------------------------------------------------------
parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

----------------------------------------------------------------
--- Define ppr
----------------------------------------------------------------
instance Pretty Expr where
  ppr p e =
    case e of
      Lit (LInt a) -> text (show a)
      Lit (LBool b) -> text (show b)
      Var x -> text x
      App a b -> parensIf (p > 0) $ (ppr (p + 1) a) <+> (ppr p b)
      Lam x a ->
        parensIf (p > 0) $
        char '\\' <> hsep (flmap pp (viewVars e)) <+>
        "->" <+> ppr (p + 1) (viewBody e)

ppexpr :: Expr -> String
ppexpr = render . ppr 0
----------------------------------------------------------------
