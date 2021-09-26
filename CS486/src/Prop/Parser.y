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
            | '(' PropFormula TImplies PropFormula ')' { PCon PImplies $2 $4}
            | '(' PropFormula TOr PropFormula ')'      { PCon POr $2 $4 }
            | '(' PropFormula TAnd PropFormula ')'     { PCon PAnd $2 $4 }
            | '(' PropFormula ')' { $2 }
            | PropFormula TImplies PropFormula { PCon PImplies $1 $3}
            | PropFormula TOr PropFormula      { PCon POr $1 $3 }
            | PropFormula TAnd PropFormula     { PCon PAnd $1 $3 }
{

parseError :: [Token] -> Except String a
parseError ((TPVar _ c): (TPVar p d) : _) = throwError $ "Propositional Variables must be one character only. Occured at character \'" ++ d ++ "\' " ++ show p
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"


parseProp :: String -> Prop Char
parseProp s = case parseProp' s of
                Left msg -> error ("parse error:" ++ msg)
                Right e  -> e

parseProp' :: String -> Either String (Prop Char)
parseProp' input = runExcept $ do
   tokenStream <- scanTokens input
   propBatch tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens

}