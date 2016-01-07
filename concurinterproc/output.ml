(** Output the result of analyses *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2012.
*)

open Format
open Equation

type 'a threadoutput = (Syntax.point, (Syntax.point array * 'a) list) PHashhe.t

(*  ********************************************************************* *)
(** {2 Project results of a multithreaded program on a single thread} *)
(*  ********************************************************************* *)

let threadoutput_of_output
    ~(is_bottom:'a -> bool)
    ~(equation:'b Equation.equation)
    output
    (threadname:string)
    :
    'a threadoutput
    =
  let index = Equation.index_of_threadname ~equation threadname in
  let res = PHashhe.create_compare Syntax.point_hashhe_compare 31 in
  PSHGraph.iter_vertex output
    (begin fun vertex abs ~pred ~succ ->
      if not (is_bottom abs) then begin
	begin match vertex with
	| Top tpoint ->
	    let list =
	      try PHashhe.find res tpoint.(index)
	      with Not_found -> []
	    in
	    PHashhe.replace res
	      tpoint.(index) ((tpoint,abs)::list)
	| Tail _ -> ()
	end
      end;
    end)
  ;
  res

(*  ********************************************************************* *)
(** {2 Printing} *)
(*  ********************************************************************* *)

let print_output ~analysis ~print_abstract fmt output
    =
  fprintf fmt "@{<B>Raw result of %s analysis@}@ "
    (match analysis with `Forward -> "forward" | `Backward -> "backward")
  ;
  let lvertextabs =
    PSHGraph.fold_vertex output
      (fun vertex tabs ~pred ~succ res -> (vertex,tabs)::res)
      []
  in
  let lvertextabs = List.fast_sort
    (fun (x,_) (y,_) -> Equation.vertex_compare x y)
    lvertextabs
  in
  Print.list
    ~first:"@[<v>" ~sep:"@ " ~last:"@]@."
    (begin fun fmt (vertex,abs) ->
      fprintf fmt "@{<R>%a@} %a"
	Equation.print_vertex vertex
	print_abstract abs
      end)
      fmt
      lvertextabs
    ;
  ()

let print_threadoutput
    ~analysis ~removed ~print_abstract
    ~(prog:Syntax.var Syntax.program) fmt threadname threadoutput
    =
  let rec tln n list =
    if n=0 then list else tln (n-1) (List.tl list)
  in
  (** Remove auxiliary type and variables for program counters *)
  let prog = { prog with
    Syntax.typenumdef = List.tl prog.Syntax.typenumdef;
    Syntax.global = tln (List.length prog.Syntax.threads) prog.Syntax.global
  }
  in
  fprintf fmt "@{<B>Result of %s analysis projected on %s@}@ "
    (match analysis with `Forward -> "forward" | `Backward -> "backward")
    (if threadname=""
    then "single program thread"
    else "thread "^threadname)
  ;
  fprintf fmt "@[<v>%a@]@."
    (Syntax.print_program
      Syntax.print_var
      begin fun fmt (point:Syntax.point) ->
	try
	  let ltpointabs = PHashhe.find threadoutput point in
	  fprintf fmt "@[<hv>@{<R>%a@}@]"
	    (Equation.print_ltpointtabs ~print_abstract) ltpointabs
	with Not_found ->
	  let str =
	    if PSette.mem point removed then "unknown" else "bottom"
	  in
	  fprintf fmt "@{<R>%a %s@}"
	    Syntax.print_point point str
      end)
    prog
  ;
  ()

(*  ===================================================================== *)
(** {3 Generic printing function} *)
(*  ===================================================================== *)

let print_result
    ~is_bottom ~print_abstract
    ~threadoutput_of_output
    ~threadoutput_project
    ~removed ~analysis ~prog ~equation
    (man:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
    fmt
    output
    =
  if !Option.print_raw then
    print_output ~print_abstract ~analysis fmt output
  ;
  if !Option.print_thread<>"" || (Array.length equation.Equation.tcfgs)=1 then begin
    let threadoutput =
      threadoutput_of_output ~is_bottom ~equation output !Option.print_thread
    in
    let threadoutput = threadoutput_project
      ~remove_var0:(not !Option.print_auxvar)
      ~remove_concurrency:(not !Option.print_concurrency)
      ~equation
      man threadoutput !Option.print_thread
    in
    print_threadoutput ~analysis ~removed ~print_abstract ~prog
      fmt !Option.print_thread threadoutput
  end;
  let nbvars = PSHGraph.fold_vertex output
    (fun vertex abs ~pred ~succ res -> res+1) 0
  in
  let nbhedges = PSHGraph.fold_hedge output
    (fun vertex abs ~pred ~succ res -> res+1) 0
  in
  fprintf fmt "Size of (explored) equation system: %d variables, %d functions@." nbvars nbhedges
  ;
  fprintf fmt "Time = %f@." (PSHGraph.info output).Fixpoint.time;
  ()
