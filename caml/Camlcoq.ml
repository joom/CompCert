(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

(* Library of useful Caml <-> Coq conversions *)

open Datatypes
open CList
open BinPos
open BinInt

(* Integers *)

let rec camlint_of_positive = function
  | Coq_xI p -> Int32.add (Int32.shift_left (camlint_of_positive p) 1) 1l
  | Coq_xO p -> Int32.shift_left (camlint_of_positive p) 1
  | Coq_xH -> 1l

let camlint_of_z = function
  | Z0 -> 0l
  | Zpos p -> camlint_of_positive p
  | Zneg p -> Int32.neg (camlint_of_positive p)

let camlint_of_coqint : Integers.int -> int32 = camlint_of_z

let rec camlint_of_nat = function
  | O -> 0
  | S n -> camlint_of_nat n + 1

let rec nat_of_camlint n =
  assert (n >= 0l);
  if n = 0l then O else S (nat_of_camlint (Int32.sub n 1l))

let rec positive_of_camlint n =
  if n = 0l then assert false else
  if n = 1l then Coq_xH else
  if Int32.logand n 1l = 0l
  then Coq_xO (positive_of_camlint (Int32.shift_right_logical n 1))
  else Coq_xI (positive_of_camlint (Int32.shift_right_logical n 1))

let z_of_camlint n =
  if n = 0l then Z0 else
  if n > 0l then Zpos (positive_of_camlint n)
  else Zneg (positive_of_camlint (Int32.neg n))

let coqint_of_camlint (n: int32) : Integers.int = 
  (* Interpret n as unsigned so that resulting Z is in range *)
  if n = 0l then Z0 else Zpos (positive_of_camlint n)

(* Atoms (positive integers representing strings) *)

let atom_of_string = (Hashtbl.create 17 : (string, positive) Hashtbl.t)
let string_of_atom = (Hashtbl.create 17 : (positive, string) Hashtbl.t)
let next_atom = ref Coq_xH

let intern_string s =
  try
    Hashtbl.find atom_of_string s
  with Not_found ->
    let a = !next_atom in
    next_atom := coq_Psucc !next_atom;
    Hashtbl.add atom_of_string s a;
    Hashtbl.add string_of_atom a s;
    a

let extern_atom a =
  try
    Hashtbl.find string_of_atom a
  with Not_found ->
    Printf.sprintf "<unknown atom %ld>" (camlint_of_positive a)

(* Lists *)

let rec coqlist_iter f = function
    Coq_nil -> ()
  | Coq_cons(a,l) -> f a; coqlist_iter f l

let rec length_coqlist = function
  | Coq_nil -> 0
  | Coq_cons (x, l) -> 1 + length_coqlist l

let array_of_coqlist = function
  | Coq_nil -> [||]
  | Coq_cons(hd, tl) as l ->
      let a = Array.create (length_coqlist l) hd in
      let rec fill i = function
        | Coq_nil -> a
        | Coq_cons(hd, tl) -> a.(i) <- hd; fill (i + 1) tl in
      fill 1 tl

(* Strings *)

let char_of_ascii (Ascii.Ascii(a0, a1, a2, a3, a4, a5, a6, a7)) =
  Char.chr(  (if a0 then 1 else 0)
           + (if a1 then 2 else 0)
           + (if a2 then 4 else 0)
           + (if a3 then 8 else 0)
           + (if a4 then 16 else 0)
           + (if a5 then 32 else 0)
           + (if a6 then 64 else 0)
           + (if a7 then 128 else 0))

let coqstring_length s =
  let rec len accu = function
  | CString.EmptyString -> accu
  | CString.CString(_, s) -> len (accu + 1) s
  in len 0 s

let camlstring_of_coqstring s =
  let r = String.create (coqstring_length s) in
  let rec fill pos = function
  | CString.EmptyString -> r
  | CString.CString(c, s) -> r.[pos] <- char_of_ascii c; fill (pos + 1) s
  in fill 0 s

(* Timing facility *)

(*
let timers = (Hashtbl.create 9 : (string, float) Hashtbl.t)

let add_to_timer name time =
  let old = try Hashtbl.find timers name with Not_found -> 0.0 in
  Hashtbl.replace timers name (old +. time)

let time name fn arg =
  let start = Unix.gettimeofday() in
  try
    let res = fn arg in
    add_to_timer name (Unix.gettimeofday() -. start);
    res
  with x ->
    add_to_timer name (Unix.gettimeofday() -. start);
    raise x

let print_timers () =
  Hashtbl.iter
    (fun name time -> Printf.printf "%-20s %.3f\n" name time)
    timers

let _ = at_exit print_timers
*)
