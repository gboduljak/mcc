{-# LANGUAGE RecordWildCards #-}

module Semant.Errors.SemantErrorBundle where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as Ne
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Data.Void
import Lexer.Lexeme (display)
import qualified Lexer.Lexeme
import Lexer.Token
import qualified Lexer.Token
import Parser.Ast
import Parser.Combinator.TokenStream
import Parser.Errors.Merger (mergeErrorsBasedOnPos)
import Semant.Errors.SemantError
import Text.Megaparsec
  ( ErrorFancy (ErrorFail),
    ParseError (FancyError),
    ParseErrorBundle (ParseErrorBundle),
    PosState (..),
    initialPos,
    mkPos,
  )

bundleSemantErrors :: String -> [Token] -> [SemantError] -> ParseErrorBundle TokenStream Void
bundleSemantErrors file tokens errors =
  ParseErrorBundle
    ( Ne.fromList
        ( mergeErrorsBasedOnPos fancyErrors
        )
    )
    initPosState
  where
    fancyErrors = map toFancyError errors
    initPosState =
      PosState
        { pstateInput =
            TokenStream
              { tokenStreamInput = intercalate ", " . map (display . lexeme) $ tokens,
                unTokenStream = tokens
              },
          pstateOffset = 0,
          pstateSourcePos = initialPos file,
          pstateTabWidth = mkPos 8,
          pstateLinePrefix = ""
        }

toFancyError :: SemantError -> ParseError TokenStream Void
toFancyError EmptyProgram = makeFancyError EmptyProgram 0
toFancyError NoMain = makeFancyError NoMain 0
toFancyError error@(InvalidMainReturnType _) = makeFancyError error 0
toFancyError error@(IllegalBinding _ _ _ (Var _ _ _ loc)) = makeFancyError error loc
toFancyError error@(IllegalBinding _ _ _ (VarDeclError loc)) = makeFancyError error loc
toFancyError error@(UndefinedSymbol _ _ _ loc) = makeFancyError error loc
toFancyError error@(VoidFormal _ _ loc) = makeFancyError error loc
toFancyError error@BinopTypeError {..} = makeFancyError error (getExprOff binopExpr)
toFancyError error@BinopArgTypeError {..} = makeFancyError error (getExprOff parentExpr)
toFancyError error@TypeError {..} = makeFancyError error (getExprOff typeErrorExpr)
toFancyError error@CastError {..} = makeFancyError error (getExprOff castErrorExpr)
toFancyError error@CallArgsNumberError {..} = makeFancyError error (getExprOff callExpr)
toFancyError error@CallArgsTypeError {..} = makeFancyError error (getExprOff callExpr')
toFancyError error@(Redeclaration _ _ loc) = makeFancyError error loc
toFancyError error@(AddressError expr) = makeFancyError error (getExprOff expr)
toFancyError error@ReturnTypeMismatchError {..} = makeFancyError error returnOff
toFancyError error@(DerefError expr) = makeFancyError error (getExprOff expr)
toFancyError error@(AssignmentError left _) = makeFancyError error (getExprOff left)
toFancyError error@FieldAccessError {..} = makeFancyError error (getExprOff field)
toFancyError error@ArrayAccessError {..} = makeFancyError error (getExprOff targetArray)
toFancyError error@(DeadCode _ _ loc) = makeFancyError error loc
toFancyError error@(RecursiveStructDecl _ _ loc) = makeFancyError error loc

makeFancyError :: SemantError -> Int -> ParseError TokenStream Void
makeFancyError error offset =
  FancyError
    offset
    (Set.fromList [fancyErrorMessage error])

fancyErrorMessage :: SemantError -> ErrorFancy e
fancyErrorMessage error = ErrorFail (prettyPrintSemantError error)

prettyPrintSemantError :: SemantError -> String
prettyPrintSemantError = renderString . layoutSmart defaultLayoutOptions . pretty