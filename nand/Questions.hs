{- Copyright 2019 The Project Oak Authors
  
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
  
       http://www.apache.org/licenses/LICENSE-2.0
  
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE StandaloneDeriving #-}

module Questions
where

import qualified Datatypes
import qualified Netlist
import Netlist2VHDL
import ComputerNetlist

deriving instance Eq Datatypes.Coq_bool
deriving instance Show Datatypes.Coq_bool

deriving instance (Eq a, Eq b) => Eq (Datatypes.Coq_prod a b)
deriving instance (Show a, Show b) => Show (Datatypes.Coq_prod a b)

q1 = g_invert (Netlist.Input "a") 

q2 = g_and (Netlist.Input "a") (Netlist.Input "b")

q3 = g_half_adder (Netlist.Input "a") (Netlist.Input "b")

q4 = g_full_adder (Netlist.Input "a") (Netlist.Input "b") (Netlist.Input "c")

n3 = writePairToVHDL "g_half_adder" q3

n4 = writePairToVHDL "g_full_adder" q4