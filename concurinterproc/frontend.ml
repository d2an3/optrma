(** *)
 
(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Ga�l Lalire, Bertrand Jeannet 2007.
*)

open Format
open Option
open Equation

(*  ********************************************************************** *)
(** {2 Continuations} *)
(*  ********************************************************************** *)

type 'b continuation1 = {
  apron : 'a . 'a Apron.Manager.t -> 'b;
}
type 'd continuation2 = {
  bddapron: 'a 'b 'c. (Syntax.var,'a,'b,'c) Bddapron.Domain1.man -> 'd;
}

let make_apron_and_continue continue
    =
  begin match !Option.domain with
  | Box ->
      continue.apron (Box.manager_alloc ())
  | Octagon ->
      continue.apron (Oct.manager_alloc ())
  | PolkaLoose ->
      continue.apron (Polka.manager_alloc_loose ())
  | PolkaStrict ->
      continue.apron (Polka.manager_alloc_strict ())
  | PolkaEq ->
      continue.apron (Polka.manager_alloc_equalities ())
  | Taylor1plus ->
      continue.apron (T1p.manager_alloc ())
(*  | PplPolyLoose ->
      continue.apron (Ppl.manager_alloc_loose ())
  | PplPolyStrict ->
      continue.apron (Ppl.manager_alloc_strict ())
  | PplGrid ->
      continue.apron (Ppl.manager_alloc_grid ())
  | PolkaGrid ->
      let man1 = Polka.manager_alloc_loose () in
      let man2 = Ppl.manager_alloc_grid () in
      continue.apron (PolkaGrid.manager_alloc man1 man2) *)
  | _ -> failwith "make_apron_and_continue: unhandled domain"
  end

let make_bddapron_and_continue apron continue
    =
  begin match !Option.bddapron with
  | Bdd ->
      let man = Bddapron.Domain1.make_bdd apron in
      continue.bddapron man
  | Mtbdd ->
      let man = Bddapron.Domain1.make_mtbdd apron in
      continue.bddapron man
  end

let make_apron_bddapron_and_continue continue
    =
  make_apron_and_continue
    { apron = fun apron -> make_bddapron_and_continue apron continue }

(*  ********************************************************************** *)
(** {2 Printing functions} *)
(*  ********************************************************************** *)

(*  ********************************************************************** *)
(** {2 Analyzing and displaying the solution} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Alternating the analyses and displaying the results} *)
(*  ====================================================================== *)

let cuddenvcond () =
  let cudd = Cudd.Man.make_v ~numVars:30 () in
  Cudd.Man.set_gc 500000000 Gc.major Gc.full_major;
(*
  (fun () -> printf "  garbage  @?"; Gc.major (); printf "done@?")
  (fun () -> printf "  reordering  @?"; Gc.full_major(); printf "done@?")
*)
  Cudd.Man.print_limit := 1000;
  let symbol =
    Bddapron.Env.make_symbol
      ~compare:Syntax.compare_var
      ~marshal:Syntax.marshal_var
      ~unmarshal:Syntax.unmarshal_var
      Syntax.print_var
  in
  let bddindex0 = ref 400 in
  let bddsize = ref 400 in
  let env0 = Bddapron.Env.make
    ~symbol:symbol
    ~bddindex0:(!bddindex0) ~bddsize:(!bddsize * 2)
    ~relational:true cudd
  in
  let cond0 = Bddapron.Cond.make
    ~symbol:symbol
    ~bddindex0:0 ~bddsize:(!bddindex0) cudd
  in
  Cudd.Man.group cudd 0 (!bddindex0 + (2 * !bddsize))  Cudd.Man.MTR_FIXED;
  Cudd.Man.group cudd 0 !bddindex0 Cudd.Man.MTR_DEFAULT;
  Cudd.Man.group cudd !bddindex0 (2 * !bddsize) Cudd.Man.MTR_DEFAULT;
  for i = 0 to pred (!bddsize) do
    Cudd.Man.group cudd (!bddindex0 + 2*i) 2 Cudd.Man.MTR_DEFAULT
  done;
  (env0,cond0)

let compute_and_display
    ~(fmt:Format.formatter)
    ~(removed:Syntax.point PSette.t ref)
    ~(prog:Syntax.var Syntax.program)
    (fequation:Equation.dynamic Equation.equation)
    (man:(Syntax.var,'a,'b,'c) Bddapron.Domain0.man)
    :
    unit
    =
  if !Option.debug>0 then
    fprintf fmt "@{<B>Program with control points@}@.@[<v>%a@]@."
      (Syntax.print_program
	Syntax.print_var
	(fun fmt point ->
	  fprintf fmt "@{<R>%a@}"
	    Syntax.print_point point
	))
      prog
  ;
  begin match !Option.analysis_method with
  | `Solving1 ->
      let output =
	Solving.Solving1.compute_combination
	  ~fmt ~removed:(!removed) ~prog
	  fequation man
      in
     ignore output
  | `Solving2 ->
      let output =
	Solving.Solving2.compute_combination
	  ~fmt ~removed:(!removed) ~prog
	  fequation man
      in
      ignore output
  end

(*  ********************************************************************** *)
(** {2 Main function} *)
(*  ********************************************************************** *)

let analyze_and_display
    (fmt:Format.formatter)
    (prog:Syntax.var Syntax.program)
    :
    unit
    =
  (* 1. Inlining some procedures *)
  let tobeinlined = match !Option.inline with
    | None -> List.rev_map fst prog.Syntax.procedures
    | Some [] -> []
    | Some list -> list
  in
  let prog =
    if tobeinlined<>[] then
      let set =
	List.fold_left (fun set pname -> Sette.add pname set)
	  Sette.empty tobeinlined
      in
      Inlining.main prog set
    else
      prog
  in
  (* 1b. Perform loop unrolling *)
  let prog = Loopunrolling.main prog (!Option.unroll) in
  (* 2. Initializing technical details *)
  let (env0,cond0) = cuddenvcond () in

  (* 3. Preprocessing program *)
  let dbprog = Syn2db.program env0 cond0 prog in
  Instrum.prog_add_IOcopy dbprog;
  let dbprog = Dbconcur.rename_program dbprog in
  let dbprog = Db2cfgproc.translate_program ~scheduling:(!Option.scheduling) dbprog in
  let removed = ref (PSette.empty Syntax.point_compare) in
  Cfgproc_compress.program
    ~removed
    ~internalize:(!Option.compress_internalize)
    ~truebranch:(!Option.compress_truebranch)
    ~basicblock:(!Option.compress_basicblock)
    ~scheduling:(!Option.scheduling)
    dbprog
  ;
  let fequation = Db2equation.Dynamic.forward dbprog in
  if (List.length dbprog.Db.lthreads)=1 then
    Option.print_raw := false
  ;
  if false then
    printf "fequation=@.  %a@." Equation.print_dynamic_equation fequation;

(*  Man.enable_autodyn cudd Man.REORDER_SIFT; *)
  let oldsize = Cudd.Man.get_bddvar_nb env0.Bdd.Env.cudd in
  make_apron_bddapron_and_continue
    { bddapron = begin
      fun bddapron ->
	compute_and_display
	  ~fmt ~removed ~prog fequation bddapron
    end
    }
  ;
  printf "manager.size=%i,%i@." oldsize (Cudd.Man.get_bddvar_nb env0.Bdd.Env.cudd);
  ()
