module Prop (parseProp, parseProp',
            Prop(..), val, eval, collectPvars, truthTable, evaldTruthTable, printTruthTable, tautological
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
      tautological )