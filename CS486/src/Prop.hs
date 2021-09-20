module Prop (parseProp, parseProp',
            Prop(..), val, eval, collectPvars, truthTable, evaldTruthTable, printTruthTable, tautological, logicallyEquivalent
            ) where

import Prop.Parser ( parseProp, parseProp' ) 
import Prop.Syntax
    ( Prop(..),
      val,
      eval,
      collectPvars,
      printTruthTable,
      truthTable,
      evaldTruthTable,
      tautological,
      logicallyEquivalent)

--TODO add "and,not,or" "AND,NOT,OR" literal parsing to the lexer
--TODO add a function that checks if two props are logically equivilant