(* Interpreting CompCert C sources *)

(* open Format *)
open Camlcoq
open AST
(* open! Integers *)
(* open Values *)
open Memory
open Globalenvs
open Events
open! Ctypes
open Csyntax
open Csem
open Json

(* Configuration *)
let interactive = ref false (* Runs interpreter one step at a time if true *)
let latest_output = ref None

module BuildFromJson = struct

exception BadValueJson of json
exception BadQuantityJson of json
exception BadMemvalJson of json
exception BadMemoryJson of json

let parse_quantity = function
| Number 32. -> Memdata.Q32
| Number 64. -> Memdata.Q64
| j -> raise (BadQuantityJson j)

let parse_value = function
| Object [("val", String "Vundef")] -> Values.Vundef
| Object [("val", String "Vint");
          ("i", Number i)] -> Values.Vint (Z.of_sint (int_of_float i))
| Object [("val", String "Vlong");
          ("l", Number l)] -> Values.Vlong (Z.of_sint (int_of_float l))
| Object [("val", String "Vfloat");
          ("f", Number f)] -> Values.Vfloat (coqfloat_of_camlfloat f)
| Object [("val", String "Vsingle");
          ("f", Number f)] -> Values.Vfloat (coqfloat32_of_camlfloat f)
| Object [("val", String "Vptr");
          ("b", Number b);
          ("ofs", Number ofs)] ->
    Values.Vptr (P.of_int (int_of_float b), coqint_of_camlint (Int32.of_float ofs))
| j -> raise (BadValueJson j)

let parse_memval = function
| Object [("memval", String "Undef")] -> Memdata.Undef
| Object [("memval", String "Byte"); ("b", Number b)] ->
    Memdata.Byte (Z.of_sint (int_of_float b))
| Object [("memval", String "Fragment");
          ("v", v); ("q", q); ("n", Number n)] ->
    Memdata.Fragment (parse_value v, parse_quantity q,
                      Camlcoq.Nat.of_int (int_of_float n))
| j -> raise (BadMemvalJson j)

let parse_memory j =
  let rec inner m l =
    begin match l with
    | [] -> m
    | (k, v) :: rest ->
        let k' = P.of_int (int_of_string k) in
        let v' = parse_value v in
        inner (Maps.PTree.set k' v' m) rest
    end in
  let rec outer m l =
    begin match l with
    | [] -> m
    | (k, Object v) :: rest ->
        let k' = P.of_int (int_of_string k) in
        let v' = inner Maps.PTree.empty v in
        outer (Maps.PTree.set k' v' m) rest
    | _ -> raise (BadMemoryJson (Object l))
    end in
  begin match j with
  | Object l -> outer Maps.PTree.empty l
  | _ -> raise (BadMemoryJson j)
  end

end

module JsonPrettyPrinter = struct

let pp_id_ofs pp (id, ofs) =
  pp_jmember pp "id" pp_jstring (extern_atom id);
  pp_jmember pp "ofs" pp_int ofs

let pp_eventval pp eval =
  pp_jobject_start pp;
  begin match eval with
  | EVint n ->
      pp_jmember ~first:true pp "eventval" pp_jstring "EVint";
      pp_jmember pp "n" pp_int n
  | EVfloat f ->
      pp_jmember ~first:true pp "eventval" pp_jstring "EVfloat";
      pp_jmember pp "f" pp_float64 f
  | EVsingle f ->
      pp_jmember ~first:true pp "eventval" pp_jstring "EVsingle";
      pp_jmember pp "f" pp_float32 f
  | EVlong n ->
      pp_jmember ~first:true pp "eventval" pp_jstring "EVlong";
      pp_jmember pp "n" pp_int64 n
  | EVptr_global(id, ofs) ->
      pp_jmember ~first:true pp "eventval" pp_jstring "EVptr_global";
      pp_id_ofs pp (id, ofs)
  end;
  pp_jobject_end pp

let pp_event pp ev =
  pp_jobject_start pp;
  begin match ev with
  | Event_syscall(id, args, res) ->
      pp_jmember ~first:true pp "type" pp_jstring "Event_syscall";
      pp_jmember pp "id" pp_jstring (camlstring_of_coqstring id);
      pp_jmember pp "args" (pp_jarray pp_eventval) args;
      pp_jmember pp "res" pp_eventval res
  | Event_vload(chunk, id, ofs, res) ->
      pp_jmember ~first:true pp "type" pp_jstring "Event_vload";
      pp_jmember pp "chunk" pp_jstring (PrintAST.name_of_chunk chunk);
      pp_jmember pp "id" pp_jstring (extern_atom id);
      pp_jmember pp "ofs" pp_int ofs;
      pp_jmember pp "res" pp_eventval res
  | Event_vstore(chunk, id, ofs, arg) ->
      pp_jmember ~first:true pp "type" pp_jstring "Event_vstore";
      pp_jmember pp "id" pp_jstring (extern_atom id);
      pp_jmember pp "ofs" pp_int ofs;
      pp_jmember pp "arg" pp_eventval arg
  | Event_annot(text, args) ->
      pp_jmember ~first:true pp "type" pp_jstring "Event_annot";
      pp_jmember pp "text" pp_jstring (camlstring_of_coqstring text);
      pp_jmember pp "args" (pp_jarray pp_eventval) args;
  end;
  pp_jobject_end pp

let pp_value ge e pp v =
  pp_jobject_start pp;
  begin match v with
  | Values.Vundef ->
      pp_jmember ~first:true pp "val" pp_jstring "Vundef"
  | Values.Vint i ->
      pp_jmember ~first:true pp "val" pp_jstring "Vint";
      pp_jmember pp "i" pp_jint (Z.to_int i)
  | Values.Vlong l ->
      pp_jmember ~first:true pp "val" pp_jstring "Vlong";
      pp_jmember pp "l" pp_jint (Z.to_int l)
  | Values.Vfloat f ->
      pp_jmember ~first:true pp "val" pp_jstring "Vfloat";
      pp_jmember pp "f" output_string (Printf.sprintf "%.15F" (camlfloat_of_coqfloat f))
  | Values.Vsingle f ->
      pp_jmember ~first:true pp "val" pp_jstring "Vsingle";
      pp_jmember pp "l" output_string (Printf.sprintf "%.15Ff" (camlfloat_of_coqfloat32 f))
  | Values.Vptr (b, ofs) ->
      pp_jmember ~first:true pp "val" pp_jstring "Vptr";
      pp_jmember pp "b" pp_jint (P.to_int b);
      pp_jmember pp "ofs" pp_jint32 (camlint_of_coqint ofs);
      match Interp.invert_local_variable e b with
      | Some id -> pp_jmember pp "name" pp_jstring (extern_atom id)
      | None ->
          match Genv.invert_symbol ge b with
          | Some id -> pp_jmember pp "name" pp_jstring (extern_atom id)
          | None -> ()
  end;
  pp_jobject_end pp

let pp_quantity pp = function
| Memdata.Q32 -> pp_jint pp 32
| Memdata.Q64 -> pp_jint pp 64

let pp_memval ge e pp p =
  pp_jobject_start pp;
  begin match p with
  | Memdata.Undef ->
      pp_jmember ~first:true pp "memval" pp_jstring "Undef"
  | Memdata.Byte b ->
      pp_jmember ~first:true pp "memval" pp_jstring "Byte";
      pp_jmember pp "b" pp_jint (Z.to_int b);
  | Memdata.Fragment(v, q, n) ->
      pp_jmember ~first:true pp "memval" pp_jstring "Fragment";
      pp_jmember pp "v" (pp_value ge e) v;
      pp_jmember pp "q" pp_quantity q;
      pp_jmember pp "n" pp_jint (Camlcoq.Nat.to_int n);
  end;
  pp_jobject_end pp

let sort_memory m = List.sort (fun x y -> compare (P.to_int (fst x)) (P.to_int (fst y))) m

let pp_zmap ge e pp m =
  let elts = Maps.PTree.elements (snd m) in
  pp_jobject (fun pp k -> pp_jstring pp (string_of_int (P.to_int k)))
             (pp_memval ge e)
             pp
             (sort_memory elts)

let pp_memory ge e pp m =
  pp_jobject (fun pp k -> pp_jstring pp (string_of_int (P.to_int k)))
             (pp_zmap ge e)
             pp
             (sort_memory (Maps.PTree.elements (snd (Mem.mem_contents m))))

let pp_state pp (prog, ge, s) =
  pp_jobject_start pp;
  begin match s with
  | State(f, s, k, e, m) ->
      (* fprintf p "in function %s, statement@ @[<hv 0>%a@]" (name_of_function prog f) PrintCsyntax.print_stmt s *)
      pp_jmember ~first:true pp "state" pp_jstring "State";
      pp_jmember pp "memory" (pp_memory ge e) m
  | ExprState(f, r, k, e, m) ->
      (* fprintf p "in function %s, expression@ @[<hv 0>%a@]" (name_of_function prog f) PrintCsyntax.print_expr r *)
      pp_jmember ~first:true pp "state" pp_jstring "ExprState";
      pp_jmember pp "memory" (pp_memory ge e) m
  | Callstate(fd, args, k, m) ->
      (* fprintf p "calling@ @[<hov 2>%s(%a)@]" (name_of_fundef prog fd) print_val_list args *)
      pp_jmember ~first:true pp "state" pp_jstring "Callstate"
  | Returnstate(res, k, m) ->
      (* fprintf p "returning@ %a" print_val res *)
      pp_jmember ~first:true pp "state" pp_jstring "Returnstate"
  | Stuckstate ->
      pp_jmember ~first:true pp "state" pp_jstring "Stuckstate"
  end;
  pp_jobject_end pp

let pp_success pp time red state events =
  pp_jobject_start pp;
  pp_jmember ~first:true pp "time" pp_jint time;
  pp_jmember pp "reduction" pp_jstring (camlstring_of_coqstring red);
  pp_jmember pp "state" pp_state state;
  begin match events with
  | [] -> ()
  | _ -> pp_jmember pp "events" (pp_jarray pp_event) events;
  end;
  begin match !latest_output with
  | None -> ()
  | Some s -> pp_jmember pp "output" pp_jstring s;
  end;
  pp_jobject_end pp

let pp_error pp time msg code state =
  pp_jobject_start pp;
  pp_jmember ~first:true pp "time" pp_jint time;
  pp_jmember pp "message" pp_jstring msg;
  begin match code with
  | None -> ()
  | Some c -> pp_jmember pp "code" pp_jint32 c;
  end;
  pp_jobject_end pp

end

let do_event ge time w ev =
  (* Return new world after external action *)
  match ev with
  | Event_vstore(chunk, id, ofs, v) ->
      begin match Determinism.nextworld_vstore w chunk id ofs v with
      | None -> assert false
      | Some w' -> w'
      end
  | _ -> w

let rec do_events ge time w t =
  match t with
  | [] -> w
  | ev :: t' -> do_events ge time (do_event ge time w ev) t'

let (>>=) opt f = match opt with None -> None | Some arg -> f arg

let do_external_function id sg ge w args m =
  match camlstring_of_coqstring id, args with
  | "printf", Values.Vptr(b, ofs) :: args' ->
      Interp.extract_string m b ofs >>= fun fmt ->
      let fmt' = Interp.do_printf m fmt args' in
      let len = coqint_of_camlint (Int32.of_int (String.length fmt')) in
      latest_output := Some fmt';
      Interp.convert_external_args ge args sg.sig_args >>= fun eargs ->
      Some(((w, [Event_syscall(id, eargs, EVint len)]), Values.Vint len), m)
  | _ -> None

(* Execution of a single step, interactively.
   Return list of 5-tuples (reduction rule, next state, next world, events, output). *)
let interactive_step pp prog ge time s w =
  match Cexec.at_final_state s with
  | Some r ->
      JsonPrettyPrinter.pp_error pp time "terminated" (Some (camlint_of_coqint r)) s;
      exit (Int32.to_int (camlint_of_coqint r))
  | None ->
      latest_output := None;
      let l = Cexec.do_step ge do_external_function Interp.do_inline_assembly w s in
      if l = []
      || List.exists (fun (Cexec.TR(r,t,s)) -> s = Stuckstate) l
      then begin
        JsonPrettyPrinter.pp_error pp time "stuck" None s;
        exit 126
      end else begin
        List.map (fun (Cexec.TR(r, t, s')) -> (r, s', do_events ge time w t, t)) l
      end

let fixup_main pp p =
  match Interp.find_main_function p.Ctypes.prog_main p.Ctypes.prog_defs with
  | None ->
      Printf.fprintf pp "ERROR: no entry function %s()@."
                            (extern_atom p.Ctypes.prog_main);
      None
  | Some main_fd ->
      match type_of_fundef main_fd with
      | Tfunction([], Ctypes.Tint(I32, Signed, _), _) ->
          Some p
      | Tfunction([Ctypes.Tint _; Tpointer(Tpointer(Ctypes.Tint(I8,_,_),_),_)],
                  Ctypes.Tint _, _) as ty ->
          Some (Interp.change_main_function p
                   (Interp.call_main3_function p.Ctypes.prog_main ty))
      | Tfunction([], ty_res, _) as ty ->
          Some (Interp.change_main_function p
                   (Interp.call_other_main_function p.Ctypes.prog_main ty ty_res))
      | _ ->
          Printf.fprintf pp
             "ERROR: wrong type for entry function %s()@."
             (extern_atom p.Ctypes.prog_main);
          None

let run_interactive prog =
  let pp = stdout in
  match fixup_main pp prog with
  | None -> exit 126
  | Some prog1 ->
      let wprog = Interp.world_program prog1 in
      let wge = globalenv wprog in
      match Genv.init_mem (program_of_program wprog) with
      | None ->
          Printf.fprintf pp "ERROR: World memory state undefined@."; exit 126
      | Some wm ->
      match Cexec.do_initial_state prog1 with
      | None ->
          Printf.fprintf pp "ERROR: Initial state undefined@."; exit 126
      | Some(ge, s) ->
          let rec loop time s w =
            let input = read_line () in
            try begin
              let j = JsonParser.value JsonLexer.read (Lexing.from_string input) in
              pp_json pp j;
              begin match interactive_step pp prog1 ge time s w with
              | [] -> ()
              | (red, s', w', events) :: _ ->
                JsonPrettyPrinter.pp_success pp time red (prog1, ge.genv_genv, s') events;
                loop (time + 1) s' w'
              end
            end with _ ->
              Printf.fprintf pp "ERROR: Can't parse JSON input."; exit 126
          in
          ignore (loop 0 s (Interp.world wge wm))
