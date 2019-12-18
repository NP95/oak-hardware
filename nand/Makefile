#
# Copyright 2019 The Project Oak Authors
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
.PHONY: all clean haskell

all:		haskell ComputerNetlist.hs

haskell:	Makefile.coq
		$(MAKE) -f Makefile.coq

Makefile.coq:	_CoqProject
		coq_makefile -f _CoqProject -o Makefile.coq


ComputerNetlist.hs:	Makefile.coq
		sed -e 's/Datatypes.Coq_bool/Netlist.Coq_bool/g' \
		    -e 's/Datatypes.Coq_false/Netlist.Coq_false/g' \
		    -e 's/Datatypes.Coq_true/Netlist.Coq_true'/g \
                    -e 's/import qualified Nand/import qualified Netlist/' \
                    -e 's/ComputerGates/ComputerNetlist/' \
                    -e 's/Nand/Netlist/g' ComputerGates.hs > $@


clean:		Makefile.coq
		$(MAKE) -f Makefile.coq clean
		rm -rf Makefile.coq ComputerNetlist.hs ComputerGates.hs \
                       Datatypes.hs Decimal.hs List.hs Logic.hs Nand.hs \
                       Nat.hs Specif.hs Makefile.coq.conf
