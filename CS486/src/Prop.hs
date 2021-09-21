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
--TODO add quickcheck to make sure parsing regarding parens and NOT aren't swallowing the wrong amount of subformulas
--TODO pretty printer
