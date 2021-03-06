(****************************************************************************)
(* Copyright 2019 The Project Oak Authors                                   *)
(*                                                                          *)
(* Licensed under the Apache License, Version 2.0 (the "License")           *)
(* you may not use this file except in compliance with the License.         *)
(* You may obtain a copy of the License at                                  *)
(*                                                                          *)
(*     http://www.apache.org/licenses/LICENSE-2.0                           *)
(*                                                                          *)
(* Unless required by applicable law or agreed to in writing, software      *)
(* distributed under the License is distributed on an "AS IS" BASIS,        *)
(* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *)
(* See the License for the specific language governing permissions and      *)
(* limitations under the License.                                           *)
(****************************************************************************)

(* A codification of the Lava embedded DSL develope for Haskell into
   Coq for the specification, implementaiton and formal verification of circuits.
   Experimental work, very much in flux, as Satnam learns Coq!
*)

Require Import Program.Basics.
From Coq Require Import Bool.Bool.
From Coq Require Import Ascii String.
From Coq Require Import Lists.List.
Import ListNotations.

Require Import Hask.Control.Monad.
Require Import Hask.Control.Monad.State.

Local Open Scope list_scope.
Local Open Scope monad_scope.

Set Printing Implicit.
Set Printing All.

(* The Cava class represents circuit graphs with Coq-level inputs and
   outputs, but does not represent the IO ports of circuits. This allows
   us to define both circuit netlist interpretations for the Cava class
   as well as behavioural interpretations for attributing semantics. *)
Class Cava m t `{Monad m} := {
  (* Basic gates *)
  inv : t -> m t;
  and2 : t * t -> m t;
}.

Record Instance : Type := mkInstance {
  inst_name : string;
  inst_args : list nat;
}.

Record CavaState : Type := mkCavaState {
  netNumber : nat;
  instances : list Instance;
  inputs : list (string * nat);
  outputs : list (string * nat);
}.

Definition initState : CavaState
  := mkCavaState 0 [] [] [].

Definition invNet (i:nat) : State CavaState nat :=
  cs <- get;
  match cs with
  | mkCavaState o insts inputs outputs
      => put (mkCavaState (o+1) (cons (mkInstance "inv" [i; o]) insts) inputs outputs) ;;
         return_ o
  end. 

Definition and2Net (i0i1 : nat * nat) : State CavaState nat :=
  cs <- get;
  match cs with
  | mkCavaState o insts inputs outputs
      => put (mkCavaState (o+1) (cons (mkInstance "and2" [fst i0i1; snd i0i1; o]) insts) inputs outputs) ;;
         return_ o
  end.


Instance CavaNet : Cava (State CavaState) nat :=
  { inv := invNet;
    and2 := and2Net;
}.


Definition inputNet (name : string) : State CavaState nat := 
  cs <- get;
  match cs with
  | mkCavaState o insts inputs outputs
     => put (mkCavaState (o+1) insts (cons (name, o) inputs) outputs) ;;
        return_ o
  end.

Definition outputNet (name : string) (i : nat) : State CavaState nat :=
  cs <- get;
  match cs with
  | mkCavaState o insts inputs outputs
     => put (mkCavaState o insts inputs (cons (name, i) outputs)) ;;
        return_ i
  end.

Class CavaTop m t `{Cava m t} := {
  (* Input and output ports. *)
  input : string -> m t;
  output : string -> t -> m t;
}.

Instance CavaTopNet : CavaTop (State CavaState) nat :=
  { input := inputNet;
    output := outputNet;
}.

Definition invBool (i : bool) : State unit bool :=
  return_ (negb i).

Definition and2Bool (i0i1 : bool * bool) : State unit bool :=
  let (i0, i1) := i0i1 in
  return_ (i0 && i1).


Definition inputBool (name : string) : State unit bool :=
  return_ false.

Definition outputBool (name : string) (i : bool) : State unit bool :=
  return_ i.

Instance CavaBool : Cava (State unit) bool :=
  { inv := invBool;
    and2 := and2Bool;
}.






