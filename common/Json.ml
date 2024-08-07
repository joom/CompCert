(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Bernhard Schommer, AbsInt Angewandte Informatik GmbH       *)
(*                                                                     *)
(*  AbsInt Angewandte Informatik GmbH. All rights reserved. This file  *)
(*  is distributed under the terms of the INRIA Non-Commercial         *)
(*  License Agreement.                                                 *)
(*                                                                     *)
(* *********************************************************************)

open Camlcoq


(* Simple functions for JSON printing *)

(* Print a string as json string *)
let pp_jstring oc s =
  output_string oc "\"";
  output_string oc s;
  output_string oc "\""

(* Print a bool as json bool *)
let pp_jbool oc b = output_string oc (string_of_bool b)

(* Print an int as json int *)
let pp_jint oc i = output_string oc (string_of_int i)

(* Print an int32 as json int *)
let pp_jint32 oc i = output_string oc (Int32.to_string i)

(* Print an int64 as json int *)
let pp_jint64 oc i = output_string oc (Int64.to_string i)

(* Print optional value *)
let pp_jopt pp_elem oc = function
  | None -> output_string oc "null"
  | Some i -> pp_elem oc i

(* Print opening and closing curly braces for json dictionaries *)
let pp_jobject_start pp =
  output_string pp "{"

let pp_jobject_end pp =
  output_string pp "}"

(* Print a member of a json dictionary *)
let pp_jmember_kv ?(first=false) pp pp_key key pp_value value =
  if not first then output_string pp ",";
  output_string pp " ";
  pp_key pp key;
  output_string pp " : ";
  pp_value pp value;
  ()

(* Print a member of a json dictionary *)
let pp_jmember ?(first=false) pp name pp_mem mem =
  pp_jmember_kv ~first:first pp pp_jstring name pp_mem mem

(* Print singleton object *)
let pp_jsingle_object pp name pp_mem mem =
  pp_jobject_start pp;
  pp_jmember ~first:true pp name pp_mem mem;
  pp_jobject_end pp

(* Print a list as json array *)
let pp_jarray elem pp l =
  let pp_sep () = output_string pp ", " in
  output_string pp "[";
  begin match l with
  | []  -> ()
  | hd::tail ->
    elem pp hd;
    List.iter (fun l -> pp_sep (); elem pp l) tail;
  end;
  output_string pp "]"

(* Print an association list as json object *)
let pp_jobject pp_key pp_value pp l =
  pp_jobject_start pp;
  begin match l with
  | [] -> ()
  | (k, v) :: tail ->
    pp_jmember_kv ~first:true pp pp_key k pp_value v;
    List.iter (fun (k', v') -> pp_jmember_kv pp pp_key k' pp_value v') tail;
  end;
  pp_jobject_end pp

(* Helper functions for printing coq integer and floats *)
let pp_int pp i =
  pp_jint32 pp (camlint_of_coqint i)

let pp_int64 pp i =
  pp_jint64 pp (camlint64_of_coqint i)

let pp_float32 pp f =
  pp_jint32 pp (camlint_of_coqint (Floats.Float32.to_bits f))

let pp_float64 pp f =
  pp_jint64 pp (camlint64_of_coqint (Floats.Float.to_bits f))

let pp_z pp z =
  output_string pp (Z.to_string z)

(* Helper functions for printing assembler constructs *)
let pp_atom pp a =
  pp_jstring pp (extern_atom a)

let pp_atom_constant pp a =
  pp_jsingle_object pp "Atom" pp_atom a

let pp_int_constant pp i =
  pp_jsingle_object pp "Integer" pp_int i

let pp_float64_constant pp f =
  pp_jsingle_object pp "Float" pp_float64 f

let pp_float32_constant pp f =
  pp_jsingle_object pp "Float" pp_float32 f

let id = ref 0

let next_id () =
  let tmp = !id in
  incr id;
  tmp

let reset_id () =
  id := 0

let pp_id_const pp () =
  let i = next_id () in
  pp_jsingle_object pp "Integer" pp_jint i

type json =
  | Null
  | Bool of bool
  | Number of Float.t
  | String of string
  | Array of json list
  | Object of (string * json) list

let rec pp_json pp = function
| Null -> output_string pp "null"
| Bool b -> pp_jbool pp b
| Number f -> output_string pp (Float.to_string f)
| String s -> pp_jstring pp s
| Array l -> pp_jarray pp_json pp l
| Object l -> pp_jobject pp_jstring pp_json pp l
