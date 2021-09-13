{
{-# LANGUAGE FlexibleContexts #-}
module Prop.Lexer where

import Control.Monad.Except

}

%wrapper "posn"

$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
    $eol            ;
    $white+         ;

    -- Symbols
    \(              { \p _ -> TLParen p }
    \)              { \p _ -> TRParen p }

    -- https://en.wikipedia.org/wiki/List_of_logic_symbols
    -- Logic Symbols

    --Implies
    "⇒"             { \p _ -> TImplies p}
    "→"              { \p _ -> TImplies p}
    "⊃"             { \p _ -> TImplies p}
    "->"             { \p _ -> TImplies p}

    --Not
    "¬"             { \p _ -> TNot p}
    "˜"             { \p _ -> TNot p}
    "~"             { \p _ -> TNot p}
    "!"             { \p _ -> TNot p}

    --And
    "∧"             { \p _ -> TAnd p}
    "&"             { \p _ -> TAnd p}

    --Or
    "∨"             { \p _ -> TOr p}
    "∥"             { \p _ -> TOr p}
    "||"            { \p _ -> TOr p}

    --Propositional Variable
    $alpha          { \p s -> TPVar p s }

{

data Token = TLParen AlexPosn
           | TRParen AlexPosn
           | TImplies AlexPosn
           | TAnd AlexPosn
           | TOr AlexPosn
           | TNot AlexPosn
           | TPVar AlexPosn String
           deriving (Eq,Show)

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _ lineNum _) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _ _ colNum) = colNum

scanTokens :: String -> Except String [Token]
scanTokens str = go (alexStartPos,'\n',[],str)
  where
    go inp@(pos,_,_,str) =
      case alexScan inp 0 of
        AlexEOF -> return []
        AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
        AlexSkip  inp' _       -> go inp'
        AlexToken inp' len act -> do
          res <- go inp'
          let rest = act pos (take len str)
          return (rest : res)
}