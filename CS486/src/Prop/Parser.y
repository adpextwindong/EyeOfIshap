{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Prop.Parser where

import Prop.Syntax
import Prop.Lexer

import Control.Monad.Except
import Control.Exception

}

-- Entry Point
%name propBatch

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    '('         { TLParen _ }
    ')'         { TRParen _ }

    --Logical Connectives
    TImplies    { TImplies _ }
    TAnd        { TAnd _ }
    TOr         { TOr _ }
    TNot        { TNot _ }
    TPVar       { TPVar _ $$ }
%%

-- Production Rules

--Non terminal : Expansions seperated by |

PropFormula : TPVar               { PVar (head $1) }
            | TNot PropFormula    { PNot $2 }
            | TNot '(' PropFormula ')' { PNot $3 }
            | '(' PropFormula TImplies PropFormula ')' { PImplies $2 $4}
            | '(' PropFormula TOr PropFormula ')'      { POr $2 $4 }
            | '(' PropFormula TAnd PropFormula ')'     { PAnd $2 $4 }
            | '(' PropFormula ')' { $2 }
{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"


parseProp :: String -> Prop Char
parseProp s = case parseProp' s of
                Left msg -> error ("parse error:" ++ msg)
                Right e  -> e

parseProp' input = runExcept $ do
   tokenStream <- scanTokens input
   propBatch tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens

}