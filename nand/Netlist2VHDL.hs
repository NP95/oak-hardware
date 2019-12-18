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

module Netlist2VHDL (writePairToVHDL) where

import Data.List (nub)

import qualified Datatypes

import qualified Netlist

--------------------------------------------------------------------------------
-- writePairToVHDL generates a VHDL component with the specified name by
-- elaborating the pair output  expr and writes out generated VHDL to a file
-- with the same name and a ".vhd" suffix.
--------------------------------------------------------------------------------

writePairToVHDL :: String ->
                   Datatypes.Coq_prod Netlist.Coq_bool Netlist.Coq_bool -> IO ()
writePairToVHDL name (Datatypes.Coq_pair a b)
  = do writeFile (name ++ ".vhd") (unlines (genVHDL name [a, b]))
       putStrLn ("Generate VHDL file " ++ name ++ ".vhd")

--------------------------------------------------------------------------------
-- genVHDL will generate a package declaration for a component and an
-- entity defining a component for the given circuit expression.
--------------------------------------------------------------------------------

genVHDL :: String -> [Netlist.Coq_bool] -> [String]
genVHDL name exprs
  =  vhdlPackage name circuitInputs circuitOutputs ++ [""] ++
     vhdlEntity name circuitInputs circuitOutputs ++ [""] ++
     vhdlArchitecture name (zip circuitOutputs exprs)
     where
     circuitInputs = nub (concat (map findInputs exprs))
     circuitOutputs = ["x", "y"]


findInputs :: Netlist.Coq_bool -> [String]
findInputs (Netlist.Input name) = [name]
findInputs (Netlist.NAND a b) = findInputs a ++ findInputs b
findInputs Netlist.Coq_false = []
findInputs Netlist.Coq_true = []


assignment :: String -> Netlist.Coq_bool -> String
assignment target expr
  = "  " ++ target ++ " <= " ++ exprToVHDL expr ++ ";"

exprToVHDL :: Netlist.Coq_bool -> String
exprToVHDL expr
  = case expr of
       Netlist.Coq_false -> " false "
       Netlist.Coq_true -> " true "
       Netlist.NAND a b -> "(" ++ exprToVHDL a ++ ") nand (" ++ exprToVHDL b ++ ")"
       Netlist.Input name -> name

--------------------------------------------------------------------------------
-- VHDL packages, entities and architecture.
--------------------------------------------------------------------------------

vhdlPackage :: String -> [String] -> [String] -> [String]
vhdlPackage name inputs outputs
  = ["package " ++ name ++ "_package is",
    "  component " ++ name ++ " is",
    "    port("] ++
      insertSemicolons (map vhdlInput inputs ++ map vhdlOutput outputs) ++
    [
    "  );",
    "  end component " ++ name ++ ";",
    "end package " ++ name ++ "_package;"
    ]

vhdlEntity ::  String -> [String] -> [String] -> [String]
vhdlEntity name inputs outputs
  = [
    "entity " ++ name ++ " is",
    "  port("] ++
    insertSemicolons (map vhdlInput inputs ++ map vhdlOutput outputs) ++
    [
    "  );",
    "end entity " ++ name ++ ";"
    ]

vhdlArchitecture :: String -> [(String, Netlist.Coq_bool)]-> [String]
vhdlArchitecture name exprs
  = ["architecture ben of " ++ name ++ " is",
     "begin"] ++
    [assignment t e | (t, e) <- exprs] ++
    ["end architecture ben ;"
    ]
 
vhdlInput :: String -> String
vhdlInput name = "    signal " ++ name ++ " : in boolean"

vhdlOutput :: String -> String
vhdlOutput name = "    signal " ++ name ++ " : out boolean"

insertSemicolons :: [String] -> [String]
insertSemicolons [] = []
insertSemicolons [x] = [x]
insertSemicolons (x:xs) = (x ++ ";") : insertSemicolons xs

insertCommas :: [String] -> [String]
insertCommas [] = []
insertCommas [x] = [x]
insertCommas (x:xs) = (x ++ ",") : insertCommas xs  