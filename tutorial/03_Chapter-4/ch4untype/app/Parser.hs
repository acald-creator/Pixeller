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
---------------------------------------------------------------------------------
--- Lexer
---------------------------------------------------------------------------------
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
	Tok.whiteSpace lexer
	r <- p
	eof
	return r