open Format
open Equation
open Db
open Cfg

module Cfg = struct

  let translate_binstr env cond0 binstr =
    let nbinstr =
      match binstr with
      | Cfg.Forget lvar -> Forget lvar
      | Cfg.Intro ldecl -> Intro ldecl
      | Cfg.Elim ldecl -> Elim ldecl
      | Cfg.Assign(lvar,lexpr) ->
	  let cond = Bdd.Cond.copy cond0 in
	  let lexpr0 =
	    List.map
	      (Bddapron.Syntax.to_expr0 env cond)
	      lexpr
	  in
	  let listexpr2 =
	    Bddapron.Expr2.List.of_lexpr0
	      ~normalize:true ~reduce:true ~careset:true
	      env cond lexpr0
	  in
	  Assign(lvar, listexpr2)
      | Cfg.Condition(e) ->
	  let cond = Bdd.Cond.copy cond0 in
	  let bexpr2 = Bddapron.Syntax.to_boolexpr2 env cond e in
	  Condition bexpr2
    in
    let nenv = match binstr with
      | Cfg.Intro ldecl -> Bddapron.Env.add_vars env ldecl
      | Cfg.Elim ldecl -> Bddapron.Env.remove_vars env (List.map fst ldecl)
      | _ -> env
    in
    (nbinstr,nenv)

  let translate_lbinstr env cond0 lbinstr =
    let (rev_lbinstr,nenv) =
      List.fold_left
	(begin fun (rev_lbinstr,env) binstr ->
	  let (binstr,nenv) = translate_binstr env cond0 binstr in
	  (binstr::rev_lbinstr,nenv)
	end)
	([],env)
	lbinstr
    in
    List.rev rev_lbinstr

  let thread ~cond0 (thread:(Cfg.syntax_t,'a) Db.thread) : Cfg.bddapron_t =
    let tcfg = Cfg.make() in
    let tinfo = PSHGraph.info tcfg in
    Mappe.iter
      (begin fun pname proc ->
	let cfg = proc.pext in
	PSHGraph.iter_vertex cfg
	  (begin fun point attr ~pred ~succ ->
	    PSHGraph.add_vertex tcfg point attr;
	  end)
	;
	let info = PSHGraph.info cfg in
	PHashhe.iter
	  (fun point env -> PHashhe.add tinfo.pointenv point env)
	  info.pointenv
      end)
      thread.tprocedures
    ;
    Mappe.iter
      (begin fun pname proc ->
	PSHGraph.iter_hedge proc.pext
	  (begin fun hedge instr ~pred ~succ ->
	    let env = PHashhe.find tinfo.pointenv pred.(0) in
	    begin match instr with
	    | Block lbinstr ->
		let ninstr = Block (translate_lbinstr env cond0 lbinstr) in
		PSHGraph.add_hedge tcfg hedge ninstr ~pred ~succ
	    | Call(call,callinstr) ->
		assert(call=CallReturn);
		let pcallee = Mappe.find callinstr.callee thread.tprocedures in
		let pcalleeinfo = PSHGraph.info pcallee.pext in
		let callpoint = pred.(0) in
		let retpoint = succ.(0) in
		let startpoint = pcalleeinfo.pstart in
		let exitpoint = pcalleeinfo.pexit in
		let pushpoint = Syntax.Push(callpoint) in
		PSHGraph.add_vertex tcfg pushpoint `Push;
		PSHGraph.add_hedge tcfg
		  (Cfg.unique()) (Call(Push,callinstr))
		  ~pred ~succ:[|pushpoint|];
		PSHGraph.add_hedge tcfg
		  (Cfg.unique()) (Call(CallStart,callinstr))
		  ~pred ~succ:[|startpoint|];
		PSHGraph.add_hedge tcfg
		  (Cfg.unique()) (Call(ExitReturn,callinstr))
		  ~pred:[|pushpoint;exitpoint|] ~succ:[|retpoint|]
	    end
	  end)
      end)
      thread.tprocedures
    ;
    let mainproc = Mappe.find thread.tname thread.tprocedures in
    let maincfg = mainproc.pext in
    let maininfo = PSHGraph.info maincfg in
    tinfo.pstart <- maininfo.pstart;
    tinfo.pexit <- maininfo.pexit;
    if !Option.debug>=6 && !Option.dot_fmt<>None then begin
      let (Some dot_fmt) = !Option.dot_fmt in
      Cfg.print_dot_bddapron ~title:(Print.sprintf "CFG of thread %s" thread.tname)
	dot_fmt
	tcfg
      ;
    end;
    tcfg

(*
  let backward_of_forward (db:('a,'b) Db.program) (thread:('c,'d) Db.thread) (forward:Cfg.bddapron_t) : Cfg.bddapron_t =
    let backward =
      PSHGraph.transpose
	(fun vertex attr -> attr)
	(fun hedge attr -> attr)
	(fun info -> info)
	forward
    in
    PSHGraph.iter_hedge forward
      (begin fun hedge instr ~pred ~succ ->
	begin match instr with
	  | Block _ -> ()
	  | Call(call,callinstr) ->
	      PSHGraph.remove_hedge backward hedge;
	      begin match call with
		| Push  ->
		    let callpoint = pred.(0) in
		    let retpoint = PDHashhe.y_of_x db.callret callpoint in
		    PSHGraph.add_hedge backward hedge instr
		      ~pred:[|retpoint|] ~succ
		| CallStart ->
		    let callpoint = pred.(0) in
		    let startpoint = succ.(0) in
		    let pushpoint = Syntax.Push(callpoint) in
		    PSHGraph.add_hedge backward hedge instr
		      ~pred:[|pushpoint;startpoint|] ~succ:pred;
		| ExitReturn ->
		    let exitpoint = pred.(1) in
		    PSHGraph.add_hedge backward hedge instr
		      ~pred:succ ~succ:[|exitpoint|]
		| CallReturn -> failwith ""
	      end
	end
      end);
    backward
*)
end

(** Implictly forward mode *)
module Dynamic = struct
  open Equation

  let texitpoints_add db texitpoints tpoint
      =
    Array.iteri
      (begin fun i point ->
	if PDHashhe.memy db.startexit point then
	  PHashhe.replace texitpoints.(i) tpoint ()
	;
      end)
      tpoint
    ;
    if false then
      printf "texitpoints_add tpoint=%a@.  @[<v>texitpoints=%a@]@."
	Equation.print_tpoint tpoint
	(Print.array
	  (PHashhe.print Equation.print_tpoint (fun fmt _ -> ())))
	texitpoints
    ;
    ()

  let is_tpoint_oneinitial tcfgs tpoint =
    let res = ref false in
    begin try
      Array.iteri
	(begin fun i cfg ->
	  let info = PSHGraph.info cfg in
	  if Syntax.point_equal info.pstart tpoint.(i) then begin
	    res := true; raise Exit
	  end
	end)
	tcfgs
    with Exit -> ()
    end;
    !res

  let activethread_of_tpoint tcfgs tpoint
      =
    let res = ref None in
    begin try
      if is_tpoint_oneinitial tcfgs tpoint then begin
	(* First thread, unless there is a commute *)
	res := Some 0;
	Array.iteri
	  (begin fun i cfg ->
	    let attr = PSHGraph.attrvertex cfg tpoint.(i) in
	    if attr = `Commute then begin
	      res := None;
	      raise Exit
	    end
	  end)
	  tcfgs
      end
      else begin
	Array.iteri
	  (begin fun i cfg ->
	    (* First one which is not a commute *)
	    let attr = PSHGraph.attrvertex cfg tpoint.(i) in
	    match attr with
	      | `Commute -> ()
	      | `Internal | `Fail ->
		  if !res = None then
		    res := Some i
	      | `Push -> failwith ""
	  end)
	  tcfgs
      end
    with Exit -> ()
    end;
    !res

  let succ_topvertex
      db texitpoints tcfgs vertex
      :
      (hedge, vertex array * vertex) PMappe.t
      =
    let tpoint = match vertex with
      | Top(tpoint) -> tpoint
      | _ -> failwith ""
    in
    let res = ref (PMappe.empty Pervasives.compare) in

    let succhedge (i:int) : unit =
      let pointi = tpoint.(i) in
      let sethedge = PSHGraph.succhedge tcfgs.(i) pointi in
      PSette.iter
	(begin fun hedge ->
	  let tpred = PSHGraph.predvertex tcfgs.(i) hedge in
	  let succ =
	    let succ = (PSHGraph.succvertex tcfgs.(i) hedge).(0) in
	    let attrsucc = PSHGraph.attrvertex tcfgs.(i) succ in
	    match attrsucc with
	    | `Push ->
		Tail(i,succ)
	    | _ ->
		let tsucc = Array.copy tpoint in
		tsucc.(i) <- succ;
		texitpoints_add db texitpoints tsucc;
		Top(tsucc)
	  in
	  let hedge = { index=i; hedge=hedge; tpoint = tpoint; } in
	  if (Array.length tpred)=1 then
	    res := PMappe.add hedge ([|vertex|],succ) !res
	  else begin
	    let tail = Tail(i,tpred.(0)) in
	    res := PMappe.add hedge ([|tail;vertex|],succ) !res
	  end
	end)
	sethedge
    in

    begin match activethread_of_tpoint tcfgs tpoint with
      | None ->
	  for i=0 to pred(Array.length tpoint) do
	    succhedge i
	  done
      | Some i ->
	  succhedge i
    end;
    !res

  let succ_tailvertex
      db texitpoints tcfgs vertex
      :
      (hedge, vertex array * vertex) PMappe.t
      =
    let (i,pushpoint) = match vertex with
      | Tail(i,pushpoint) -> (i,pushpoint)
      | _ -> failwith ""
    in
    let res = ref (PMappe.empty Pervasives.compare) in
    let sethedge = PSHGraph.succhedge tcfgs.(i) pushpoint in
    PSette.iter
      (begin fun hedge ->
	let tpred = PSHGraph.predvertex tcfgs.(i) hedge in
	assert(tpred.(0)=pushpoint);
	let exitstartpoint = tpred.(1) in
	PHashhe.iter
	  (begin fun exitstarttpoint () ->
	    if exitstarttpoint.(i) = exitstartpoint then begin
	      let tpred = [|vertex; Top exitstarttpoint|] in
	      let succ =
		let succi = (PSHGraph.succvertex tcfgs.(i) hedge).(0) in
		let tsucc = Array.copy exitstarttpoint in
		tsucc.(i) <- succi;
		texitpoints_add db texitpoints tsucc;
		Top(tsucc)
	      in
	      let hedge = { index=i; hedge=hedge; tpoint = exitstarttpoint } in
	      res := PMappe.add hedge (tpred,succ) !res
	    end
	  end)
	  texitpoints.(i)
      end)
      sethedge
    ;
    !res

  let succ_vertex
      db texitpoints tcfgs vertex
      :
      (hedge, vertex array * vertex) PMappe.t
      =
    let res =
      (match vertex with
	| Top _ -> succ_topvertex
	| Tail _ -> succ_tailvertex
      )
	db texitpoints tcfgs vertex
    in
    if false then
      printf "succ_vertex @[<v>vertex=%a@ active=%a@ ==>%a]@."
	Equation.print_vertex vertex
	(Print.array
	  (PHashhe.print Equation.print_tpoint (fun fmt _ -> ())))
	texitpoints
	(PMappe.print Equation.print_hedge
	  (fun fmt (tpred,succ) ->
	    fprintf fmt "pred=%a,succ=%a"
	      (Print.array Equation.print_vertex) tpred
	      Equation.print_vertex succ))
	res
    ;
    res

  let bexpr2_of_osyntax env cond oexpr =
    let bexpr0 = match oexpr with
      | None -> Bddapron.Expr0.Bool.dtrue env cond
      | Some e ->
	  let e = Bddapron.Syntax.to_expr0 env cond e in
	  Bddapron.Expr0.Bool.of_expr e
    in
    let bexpr2 =
      Bddapron.Expr2.Bool.of_expr0
	~normalize:true ~reduce:true ~careset:true env cond bexpr0
    in
    bexpr2

  let forward db =
    let initial =
      let cond = Bdd.Cond.copy db.cond0 in
      try bexpr2_of_osyntax db.env cond db.Db.initial
      with Syntax.Error s | Failure s ->
	Syntax.error "In initial condition: %s" s
    in
    let final =
      let cond = Bdd.Cond.copy db.cond0 in
      let env = Syn2db.env_add_vars db.env (db.pcs:>(Syntax.var,Syntax.var Bddapron.Env.typ) Mappe.t) in
      try bexpr2_of_osyntax env cond db.Db.final
      with Syntax.Error s | Failure s ->
	Syntax.error "In final condition: %s" s
    in
    let tcfgs = Array.map (Cfg.thread ~cond0:db.cond0) db.threads in
    let vertexenv = PHashhe.create_compare Equation.vertex_hashhe_compare 23 in
    let texitpoints = Array.map (fun _ -> PHashhe.create_compare Equation.tpoint_hashhe_compare 17) tcfgs in
    let dynamic = {
      texitpoints;
      succvertex = succ_vertex db texitpoints tcfgs
    }
    in
    let starttpoint =
      Array.map
	(begin fun thread ->
	  let proc = Mappe.find thread.tname thread.tprocedures in
	  let info = PSHGraph.info proc.pext in
	  info.pstart
	end)
	db.threads
    in
    let sinit = PSette.singleton Equation.vertex_compare (Top starttpoint) in
    ignore (texitpoints_add db texitpoints starttpoint);
    let equation = {
      db = { db with
	Db.threads = Array.map (fun thread -> { thread with text = () }) db.threads
      };
      Equation.initial = initial;
      Equation.final = final;
      tcfgs = tcfgs;
      vertexenv = vertexenv;
      sinit = sinit;
      graph = dynamic;
      threshold = PHashhe.create_compare Equation.vertex_hashhe_compare 11;
    }
    in
    equation

  let succ_hedge tcfgs hedge =
    let index = hedge.index in
    let cfg = tcfgs.(index) in
    let succ = PSHGraph.succvertex cfg hedge.hedge in
    assert((Array.length succ) = 1);
    let succ = succ.(0) in
    match succ with
      | Syntax.Push pointindex ->
	  Tail(index,succ)
      | _ ->
	  let tpoint = Array.copy hedge.tpoint in
	  tpoint.(index) <- succ;
	  Top(tpoint)

end

let static_of_dynamic (dynamic:dynamic equation) (graph:('a,'b,'c) static) =
  { dynamic with graph = graph }

let backward_of_forward
    ~(switchfb: Equation.vertex -> 'a -> 'a)
    (fequation: ('a,'b,'c) Equation.static Equation.equation) :
    ('a,'b,'c) Equation.static Equation.equation
    =
  let forward = fequation.graph in
  let backward =
    PSHGraph.transpose
      switchfb
      (fun hedge attr -> attr)
      (fun info -> info)
      forward
  in
  PSHGraph.iter_hedge forward
    (begin fun hedge attrhedge ~pred ~succ ->
      let index = hedge.index in
      let instr = PSHGraph.attrhedge fequation.tcfgs.(index) hedge.hedge in
       begin match instr with
	| Block _ -> ()
	| Call(call,callinstr) ->
	    PSHGraph.remove_hedge backward hedge;
	    begin match call with
	      | Push ->
		  let callpoint = pred.(0) in
		  let calltpoint = match callpoint with
		    | Top tpoint -> tpoint
		    | Tail _ -> failwith ""
		  in
		  let rettpoint = Array.copy calltpoint in
		  rettpoint.(index) <- PDHashhe.y_of_x fequation.db.callret calltpoint.(index);
		  let retpoint = Top rettpoint in
		  if PSHGraph.is_vertex backward retpoint then
		    PSHGraph.add_hedge backward hedge attrhedge
		      ~pred:[|retpoint|] ~succ;
	      | CallStart ->
		  let callpoint = pred.(0) in
		  let calltpoint = match callpoint with
		    | Top tpoint -> tpoint
		    | Tail _ -> failwith ""
		  in
		  let pushpoint = Tail(index,Syntax.Push calltpoint.(index)) in
		  let startpoint = succ.(0) in
		  PSHGraph.add_hedge backward hedge attrhedge
		    ~pred:[|pushpoint;startpoint|] ~succ:pred;
	      | ExitReturn ->
		  let exitpoint = pred.(1) in
		  PSHGraph.add_hedge backward hedge attrhedge
		    ~pred:succ ~succ:[|exitpoint|]
	      | CallReturn ->
		  failwith ""
	    end
      end
    end);
  let sfinal = ref (PSette.empty Equation.vertex_compare) in
  PSHGraph.iter_vertex forward
    (begin fun vertex abs ~pred ~succ ->
      match vertex with
	| Tail _ ->
	    sfinal := PSette.add vertex !sfinal;
	| Top(tpoint) ->
	    let fail = ref false in
	    Array.iteri
	      (begin fun i point ->
		let attr = PSHGraph.attrvertex fequation.tcfgs.(i) point in
		fail := !fail || attr=`Fail
	      end)
	      tpoint
	    ;
	    if !fail then
	      sfinal := PSette.add vertex !sfinal;
    end);
  { fequation with sinit = !sfinal; graph = backward }
