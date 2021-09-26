module Kasriel where

import Prop

ex1 = tautological $ parseProp "((!q) -> (!p)) -> (p -> q)"
ex2 = tautological $ parseProp "((p -> q) & (q -> r)) -> (p -> r)"
ex3 = tautological $ parseProp "(p & q) -> (p || q)"