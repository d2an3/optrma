(** Abstract semantics of CFG instructions, cartesian product version *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2009.
*)

open Format
open Db
open Cfg

type 'd abstract = 'd Abssemantic1.abstract array

let print_abstract man fmt tabs =
  Print.array ~first:"[|@[<v>" ~last:"@]|]"
    (Abssemantic1.print_abstract man)
    fmt tabs

let forget_otherthreads_globals
    (man:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
    (abs:'d Abssemantic1.abstract)
    tname
    :
    'd Abssemantic1.abstract
    =
  let (env:Syntax.var Bddapron.Env.t) = abs.Bddapron.Env.env in
  let vars = Bddapron.Env.vars env in
  let lvar =
    PSette.fold
      (begin fun var res ->
	match var with
	| `Global(_,`Input tname2)
	| `Global(_,`Output tname2) when tname2<>tname ->
	    var::res
	| _ ->
	    res
      end)
      vars []
  in
  if lvar<>[] then
    Bddapron.Domain1.forget_list man abs lvar
  else
    abs

let abs_of_vertex cons equation man vertex =
  let abs = cons man (Equation.env_of_vertex equation vertex) in
  match vertex with
  | Equation.Top(tpoint) -> Array.make (Array.length tpoint) abs
  | Equation.Tail _ -> [|abs|]

let is_bottom man tabs = Bddapron.Domain1.is_bottom man tabs.(0)
let bottom_of_vertex eq man vtx =
  abs_of_vertex Bddapron.Domain1.bottom eq man vtx
let top_of_vertex eq man vtx =
  abs_of_vertex Bddapron.Domain1.top eq man vtx


(*  ********************************************************************** *)
(** {2 Forward interprocedural semantics} *)
(*  ********************************************************************** *)

(* We assume that actual input/output parameters are NEVER global variables *)

module Forward = struct

  let apply_push
      (db:(Cfg.syntax_t,unit) Db.program)
      (i:int)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstract:'d abstract)
      (nenv:Syntax.var Bddapron.Env.t)
      (odest:'d abstract option)
      =
    let odest = match odest with
      | None -> None
      | Some tab -> Some (tab.(0))
    in
    let res =
      Abssemantic1.Forward.apply_push
	db.threads.(i) callinstr manager abstract.(0) nenv odest
    in
    [|res|]

  let apply_call
      (db:(Cfg.syntax_t,unit) Db.program)
      (i:int)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstract:'d abstract)
      (nenv:Syntax.var Bddapron.Env.t)
      (odest:'d abstract option)
      :
      'd abstract
      =
    let odest = match odest with
      | None -> Array.make (Array.length abstract) None
      | Some dest -> Array.map (fun x -> Some x) dest
    in
    Equation.array_mapi2
      (begin fun j abstractj odestj ->
	Abssemantic1.Forward.apply_call
	  ~instrum:(i=j)
	  db.threads.(i) callinstr manager abstractj nenv odestj
      end)
      abstract odest

  let apply_return
      (db:(Cfg.syntax_t,unit) Db.program)
      (i:int)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstail:'d Abssemantic1.abstract) (tabstop:'d abstract)
      (nenv:Syntax.var Bddapron.Env.t)
      (odest:'d abstract option)
      =
    let odest = match odest with
      | None -> Array.make (Array.length tabstop) None
      | Some dest -> Array.map (fun x -> Some x) dest
    in
    let abstopi = tabstop.(i) in
    let odesti = odest.(i) in

    Equation.array_mapi2
      (begin fun j abstopj odestj ->
	if j=i then
	  Abssemantic1.Forward.apply_return
	    db db.threads.(j) callinstr manager abstail abstopj nenv odestj
	else begin
	  let abstopji = Bddapron.Domain1.meet manager abstopj abstopi in
	  let odestji = match (odestj,odesti) with
	    | Some(destj),Some(desti) ->
		Some(Bddapron.Domain1.meet manager destj desti)
	    | _,_ -> None
	  in
	  let resji =
	    Abssemantic1.Forward.apply_return
	      db db.threads.(i) callinstr manager abstail abstopji nenv odestji
	  in
	  (* Forget all instrumentation variables of other threads *)
	  let resj =
	    forget_otherthreads_globals manager resji db.threads.(j).tname
	  in
	  resj
	end
      end)
      tabstop odest
end

(*  ********************************************************************** *)
(** {2 Backward interprocedural semantics} *)
(*  ********************************************************************** *)

module Backward = struct

  let apply_push
      (db:(Cfg.syntax_t,unit) Db.program)
      (i:int)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstract:'d abstract)
      (nenv:Syntax.var Bddapron.Env.t)
      (odest:'d abstract option)
      =
    let odest = match odest with
      | None -> None
      | Some tab -> Some (tab.(0))
    in
    let res =
      Abssemantic1.Backward.apply_push
	db db.threads.(i) callinstr manager abstract.(0) nenv odest
    in
    [|res|]

  let apply_return
      (db:(Cfg.syntax_t,unit) Db.program)
      (i:int)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstract:'d abstract)
      (nenv:Syntax.var Bddapron.Env.t)
      (odest:'d abstract option)
      :
      'd abstract
      =
    let odest = match odest with
      | None -> Array.make (Array.length abstract) None
      | Some dest -> Array.map (fun x -> Some x) dest
    in
    Equation.array_mapi2
      (begin fun j abstractj odestj ->
	Abssemantic1.Backward.apply_return
	  ~instrum:(i=j)
	  db.threads.(i) callinstr manager abstractj nenv odestj
      end)
      abstract odest

  let apply_call
      (db:(Cfg.syntax_t,unit) Db.program)
      (i:int)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstail:'d Abssemantic1.abstract) (tabstop:'d abstract)
      (nenv:Syntax.var Bddapron.Env.t)
      (odest:'d abstract option)
      =
    let odest = match odest with
      | None -> Array.make (Array.length tabstop) None
      | Some dest -> Array.map (fun x -> Some x) dest
    in
    let abstopi = tabstop.(i) in
    let odesti = odest.(i) in
    Equation.array_mapi2
      (begin fun j abstopj odestj ->
	if j=i then
	  Abssemantic1.Backward.apply_call
	    db db.threads.(j) callinstr manager abstail abstopj nenv odestj
	else begin
	  let abstopji = Bddapron.Domain1.meet manager abstopj abstopi in
	  let odestji = match (odestj,odesti) with
	    | Some(destj),Some(desti) ->
		Some(Bddapron.Domain1.meet manager destj desti)
	    | _,_ -> None
	  in
	  let resji =
	    Abssemantic1.Backward.apply_call
	      db db.threads.(i) callinstr manager abstail abstopji nenv odestji
	  in
	  (* Forget all instrumentation variables of other threads *)
	  let resj =
	    forget_otherthreads_globals manager resji db.threads.(j).tname
	  in
	  resj
	end
      end)
      tabstop odest

end

(*  ********************************************************************** *)
(** {2 Block of basic instruction and general instruction} *)
(*  ********************************************************************** *)

let apply_block
    ~analysis
    block
    (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
    (abstract:'d abstract)
    (odest:'d abstract option)
    :
    'd abstract
    =
  let odest = match odest with
    | None -> Array.make (Array.length abstract) None
    | Some dest -> Array.map (fun x -> Some x) dest
  in
  Equation.array_map2
    (fun abstract odest ->
      Abssemantic1.apply_block ~analysis manager abstract block odest)
    abstract odest

let apply_instr
    ~analysis
    (db:(Cfg.syntax_t,unit) Db.program)
    (i:int)
    (instr:Cfg.bddapron_instr)
    (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
    (tabs:'d abstract array)
    nenv
    (odest:'d abstract option)
    :
    'd abstract
    =
  let res =
    match instr with
    | Block block ->
	apply_block ~analysis block manager tabs.(0) odest
    | Call(CallStart,callinstr) ->
	begin match analysis with
	| `Forward ->
	    Forward.apply_call db i callinstr manager tabs.(0) nenv odest
	| `Backward ->
	    Backward.apply_call db i callinstr manager tabs.(0).(0) tabs.(1) nenv odest
	end;
    | Call(ExitReturn,callinstr) ->
	begin match analysis with
	| `Forward ->
	    Forward.apply_return db i callinstr manager tabs.(0).(0) tabs.(1) nenv odest
	| `Backward ->
	    Backward.apply_return db i callinstr manager tabs.(0) nenv odest
	end
    | Call(Push,callinstr) ->
	  begin match analysis with
	  | `Forward -> Forward.apply_push
	  | `Backward -> Backward.apply_push
	  end
	    db i callinstr manager tabs.(0) nenv odest
    | Call(CallReturn,_) -> failwith ""
  in
  res
