module Parser.Combinator.Parser where

import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Void
import Lexer.Combinator.Lexer (lex')
import Lexer.Lexeme as L
import Lexer.Token as T
import qualified Parser.Ast as Ast
import Parser.AstVisualiser (visualiseAst)
import Parser.Combinator.TokenStream
import Text.Megaparsec

type Parser = Parsec Void TokenStream

liftToken :: L.Lexeme -> T.Token
liftToken lexeme = T.Token lexeme pos pos 0
  where
    pos = initialPos ""

expect :: L.Lexeme -> Parser L.Lexeme
expect lex = token test (Set.singleton . Tokens . nes . liftToken $ lex)
  where
    test (Token lex' _ _ _) =
      if lex' == lex
        then Just lex
        else Nothing
    nes x = x :| []

end :: Parser L.Lexeme
end = expect L.Eof

litInt :: Parser Ast.Expr
litInt = token test Set.empty <?> "integer literal"
  where
    test (Token (L.LitInt n) _ _ _) = Just (Ast.LitInt n)
    test _ = Nothing

litDouble :: Parser Ast.Expr
litDouble = token test Set.empty <?> "double literal"
  where
    test (Token (L.LitDouble n) _ _ _) = Just (Ast.LitDouble n)
    test _ = Nothing

streamify :: String -> TokenStream
streamify input = case lex' "" input of
  (Left error) -> TokenStream {tokenStreamInput = input, unTokenStream = []}
  (Right stream) -> TokenStream {tokenStreamInput = input, unTokenStream = stream}

program = do
  exp <- expr
  end
  return exp

expr :: Parser Ast.Expr
expr = litInt <|> litDouble

parseComb :: String -> Either (ParseErrorBundle TokenStream Void) Ast.Expr
parseComb = parse program "" . streamify

parseComb' :: String -> IO ()
parseComb' input = case parseComb input of
  (Left error) -> putStrLn (errorBundlePretty error)
  (Right ast) -> putStrLn $ visualiseAst ast