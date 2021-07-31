module Semant.Operators.Cond where

import Semant.Semant

(<||>) :: (Bool, Semant a) -> (Bool, Semant a) -> (Bool, Semant a)
(<||>) (True, p) _ = (True, p)
(<||>) _ q = q

(||>) :: (Bool, Semant a) -> Semant a -> (Bool, Semant a)
(||>) p q = p <||> (True, q)

(|>) :: (Bool, Semant a) -> Semant a
(|>) (_, p) = p