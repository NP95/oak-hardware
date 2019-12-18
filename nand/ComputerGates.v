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

(* Implement and prove the computer from http://nandgame.com/ *)



Require Import Bool.
Require Import Nand.

Definition g_invert(a : bool) : bool := g_nand a a.


Definition g_and(a b : bool) : bool := g_invert (g_nand a b).


Definition g_or(a b : bool) : bool := g_nand (g_invert a) (g_invert b).

Definition g_xor(a b : bool) : bool := let nab := g_nand a b in g_nand (g_nand a nab) (g_nand nab b).


(* All bit lists are little endian, so a function returning a carry returns that last for consistency *)

Require Import List.

Definition g_half_adder(a b : bool) : bool * bool := (g_xor a b, g_and a b).

Definition g_full_adder(a b c : bool) : bool * bool :=
  let (abl, abh) := g_half_adder a b in
  let (abcl, abch) := g_half_adder abl c in
(*  let (l, h) := g_half_adder abh abch in
  (abcl, l). or.. *)
  (abcl, g_or abh abch).  (* should be g_xor, but the true, true case is impossible and g_or is cheaper *)

Compute g_full_adder true true true.

Definition g_bit2_adder(a0 a1 b0 b1 c : bool) : bool * bool * bool :=
  let (l0, h0) := g_full_adder a0 b0 c in
  let (l1, h1) := g_full_adder a1 b1 h0 in
  (l0, l1, h1).

Compute g_bit2_adder true true true true true.

(* a one as a list of bools *)
Fixpoint g_one (l : nat) : list bool :=
  match l with
  | 0 => nil
  | 1 => true::nil
  | S l' => g_one l' ++ false::nil
  end.

Fixpoint g_bitn_adder (a b : list bool) (c : bool) : list bool :=
  match a with
    | nil => nil
    | a0::nil => let (l, h) := g_full_adder (hd false a) (hd false b) c in
           l::h::nil
    | a0::t => let (l, h) := g_full_adder (hd false a) (hd false b) c in
              let r := g_bitn_adder t (tl b) h in
              l::r
      (* let (h, l) := g_full_adder (last a false) (last b false) c in
              let r := g_bitn_adder n' (removelast a) (removelast b) h in
              r ++ l::nil *)
  end.

(* Naive incrementer - just use an adder to add one *)
Definition g_inc (a : list bool) : list bool :=
  g_bitn_adder a (g_one (length a)) false.

Fixpoint g_bitn_inverter (n : nat) (a : list bool) : list bool :=
  match n with
  | 0 => nil
  | S n => g_invert (hd false a) :: g_bitn_inverter n (tl a)
  end.

Definition g_inv16(a : list bool) := g_bitn_inverter 16 a.
