module Parser where

import Text.Parsec

import qualified Text.Parsec.Token as Tok

langDef :: Tok.LanguageDef ()
langDef =
  Tok.LanguageDef
    { Tok.commentStart = "{-"
    , Tok.commentEnd = "-}"
    , Tok.commentLine = "--"
    , Tok.nestedComments = True
    , Tok.identStart = letter
    , Tok.identLetter = alphaNum <|> oneOf "_'"
    , Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedNames = reservedNames
    , Tok.reservedOpNames = reservedOps
    , Tok.caseSensitive = True
    }
