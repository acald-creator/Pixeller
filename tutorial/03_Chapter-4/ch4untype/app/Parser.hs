module Parser where

import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language

import qualified Text.Parsec.Token as Tok

import Syntax

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
	where ops = ["->","\\","+","*","-","="]
	names = []
	style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}
