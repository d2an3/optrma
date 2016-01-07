(** Simplify the graph of a thread *)

open Format
open Db
open Cfg

(*  ********************************************************************** *)
(** {2 Simplifying basic blocks} *)
(*  ********************************************************************** *)

module Block = struct
  type binstr =
    (Syntax.var Bddapron.Syntax.expr,
    Syntax.var Bddapron.Syntax.expr list) Cfg.binstr

  let rec substitute map (e:'a Bddapron.Syntax.expr) : 'b Bddapron.Syntax.expr
      =
    match e with
    | `Cst x -> `Cst x
    | `Ref var ->
	begin
	  try Mappe.find var map
	  with Not_found -> e
	end
    | `Unop(op,e) -> `Unop(op, substitute map e)
    | `Binop(op,e1,e2) -> `Binop(op, (substitute map e1), (substitute map e2))
    | `If(e1,e2,e3) -> `If((substitute map e1),(substitute map e2),(substitute map e3))
    | `In(e,le) -> `In((substitute map e),List.map (substitute map) le)

  let compress_assign (lv1,le1) (lv2,le2) =
    let map =
      List.fold_left2
	(fun map v1 e1 -> Mappe.add v1 e1 map)
	Mappe.empty lv1 le1
    in
    let (lv2,le2) =
      List.fold_left2
	(begin fun (lv,le) v2 e2 ->
	  ((v2::lv), ((substitute map e2)::le))
	end)
	([],[]) lv2 le2
    in
    let (lv,le) =
      List.fold_left2
	(begin fun (lv,le) v1 e1 ->
	  if List.mem v1 lv2 then
	    (lv,le)
	  else
	    (v1::lv,e1::le)
	end)
	(lv2,le2) lv1 le1
    in
    (lv,le)

  let rec consecutive change res = function
    | x1::((x2::l2) as l1) ->
	begin match (x1,x2) with
	| (Forget lv1, Forget lv2) ->
	    consecutive true res ((Forget (lv1@lv2))::l2)
	| (Condition e1, Condition e2) ->
	    consecutive true res ((Condition (Syntax.eand e1 e2))::l2)
	| (Intro d1, Intro d2) ->
	    consecutive true res ((Intro (d1@d2))::l2)
	| (Elim d1, Elim d2) ->
	    consecutive true res ((Elim (d1@d2))::l2)
	| (Assign(lv1,le1),Assign(lv2,le2)) ->
	    if true then
	      let (lv,le) = compress_assign (lv1,le1) (lv2,le2) in
	      consecutive true res (Assign(lv,le)::l2)
	    else
	      let supp_le2 =
		List.fold_left
		  (Syntax.support ?filter:None)
		  Sette.empty le2
	      in
	      if List.exists (fun v1 -> Sette.mem v1 supp_le2) lv1 then
		consecutive change (x1::res) l1
	      else
		consecutive true res (Assign(lv1@lv2, le1@le2)::l2)
	| (Condition _, Intro d)
	| (Assign _, Intro d) ->
	    consecutive change res (x2::x1::l2)
	| (Elim d1, Intro d2) ->
	    let (lforget1,lelim1) =
	      List.partition
		(fun vartyp1 -> List.mem vartyp1 d2)
		d1
	    in
	    if lforget1=[] then
	      consecutive change (x1::res) l1
	    else if lelim1 = [] then
	      let lintro2 =
		List.filter
		  (fun vartyp -> not (List.mem vartyp lforget1))
		  d2
	      in
	      let lforget1 = List.map fst lforget1 in
	      if lintro2=[] then
		consecutive true res ((Forget lforget1)::l2)
	      else
		consecutive true ((Forget lforget1)::res) ((Intro lintro2)::l2)
	    else
	      consecutive change (x1::res) l1

	| _ ->
	    consecutive change (x1::res) l1
	end
    | [x1] -> consecutive change (x1::res) []
    | [] -> (change, List.rev res)

  let rec consecutive3 change res = function
    | x1::((x2::x3::l3) as l1) ->
	begin match (x1,x2,x3) with
	| (Intro d1, Condition e, Intro d2) ->
	    consecutive3 true res ((Intro (d1@d2))::(Condition e)::l3)
	| _ ->
	    consecutive3 change (x1::res) l1
	end
    | x1::l1 -> consecutive3 change (x1::res) l1
    | [] -> (change, List.rev res)

  let simplify list =
    let loop = ref true in
    let list = ref list in
    while !loop do
      let (change1,res) = consecutive  false [] !list in
      let (change2,res) = consecutive3 false [] res in
      loop := change1 || change2;
      list := res
    done;
    !list

end


(*  ********************************************************************** *)
(** {2 Remove false condition edges} *)
(*  ********************************************************************** *)

let is_binstr_falsecondition = function
  | Condition expr when Syntax.is_false expr -> true
  | _ -> false
let is_binstr_truecondition = function
  | Condition expr when Syntax.is_true expr -> true
  | _ -> false

let falsecondition cfg : unit
    =
  PSHGraph.iter_hedge cfg
    (begin fun hedge instr ~pred ~succ ->
      match instr with
      | Block lbinstr ->
	  if List.exists is_binstr_falsecondition lbinstr then
	    PSHGraph.remove_hedge cfg hedge
      | _ -> ()
    end)
  ;
  ()

(*  ********************************************************************** *)
(** {2 Compress true condition edges} *)
(*  ********************************************************************** *)

let is_userlabel = function
  | Syntax.Lab(_,_,true) -> true
  | _ -> false

(* Such a vertex can be removed *)
let is_vertex_followed_by_unique_true_condition (cfg:Cfg.syntax_t) vertex
    =
  let succhedge = PSHGraph.succhedge cfg vertex in
  if (PSette.cardinal succhedge) = 1 then begin
    let hedge = PSette.choose succhedge in
    let instr = PSHGraph.attrhedge cfg hedge in
    match instr with
    | Block lbinstr when List.for_all is_binstr_truecondition lbinstr ->
	let succ = PSHGraph.succvertex cfg hedge in
	assert(Array.length succ = 1);
	Some(succ.(0))
    | _ ->
	None
  end
  else
    None

let is_vertex_preceded_by_unique_true_condition (cfg:Cfg.syntax_t) vertex
    =
  let predhedge = PSHGraph.predhedge cfg vertex in
  if (PSette.cardinal predhedge) = 1 then begin
    let hedge = PSette.choose predhedge in
    let instr = PSHGraph.attrhedge cfg hedge in
    match instr with
    | Block lbinstr when List.for_all is_binstr_truecondition lbinstr ->
	let pred = PSHGraph.predvertex cfg hedge in
	assert(Array.length pred = 1);
	Some(pred.(0))
    | _ ->
	None
  end
  else
    None

let cfgproc_truecondition ~removed ~scheduling prog (cfg:Cfg.syntax_t)
    =
  let info = PSHGraph.info cfg in
  let loop = ref true in
  let vertex_removed = ref false in
  while !loop do
    loop := false;
    (* Set of candidate vertices that might be removed by compression *)
    let vertices =
      PSHGraph.fold_vertex cfg
	(fun vertex _ ~pred ~succ res ->
	  let notcandidate =
	    is_userlabel vertex ||
	      Cfg.compare.SHGraph.hashv.Hashhe.equal vertex info.pstart
	  in
	  if notcandidate then res else vertex::res
	)
	[]
    in
    (* Iterates on candidate vertices *)
    List.iter
      (begin fun vertex ->
	vertex_removed := false;
	(* Case of edges generated by if-then-else or while loops *)
	begin match is_vertex_followed_by_unique_true_condition cfg vertex with
	| None -> ()
	| Some succvertex ->
	    let typvertex = PSHGraph.attrvertex cfg vertex in
	    let typsuccvertex = PSHGraph.attrvertex cfg succvertex in
	    let case1 = (typvertex=typsuccvertex) in
	    let case2 = not case1 &&
	      match scheduling with
	      | `Cooperative ->
		  false
	      | `Preemptive ->
		  (typvertex=`Internal || typvertex=`Commute) &&
		    (typsuccvertex=`Internal || typsuccvertex=`Commute)
	    in
	    if case1 || case2 then begin
	      (* redirect all incoming edges of vertex to succvertex *)
	      let predhedge = PSHGraph.predhedge cfg vertex in
	      PSette.iter
		(begin fun hedge ->
		  let instr = PSHGraph.attrhedge cfg hedge in
		  let pred = PSHGraph.predvertex cfg hedge in
		  PSHGraph.remove_hedge cfg hedge;
		  PSHGraph.add_hedge cfg hedge instr
		    ~pred ~succ:[|succvertex|]
		  ;
		end)
		predhedge
	      ;
	      PSHGraph.remove_vertex cfg vertex;
	      removed := PSette.add vertex !removed;
	      if case2 then
		PSHGraph.replace_attrvertex cfg succvertex `Commute
	      ;
	      if PDHashhe.memy prog.callret vertex then begin
		let call = PDHashhe.x_of_y prog.callret vertex in
		PDHashhe.removey prog.callret vertex;
		PDHashhe.add prog.callret call succvertex
	      end;
	      vertex_removed := true;
	      loop := true;
	    end
	end;
	(* Case of edges generated by yield *)
	if not !vertex_removed then begin
	  match is_vertex_preceded_by_unique_true_condition cfg vertex with
	  | None -> ()
	  | Some predvertex ->
	      let typvertex = PSHGraph.attrvertex cfg vertex in
	      let typpredvertex = PSHGraph.attrvertex cfg predvertex in
	      let case1 = (typvertex=typpredvertex) in
	      let case2 = not case1 && scheduling=`Cooperative && typvertex=`Internal in
	      let case3 = not case1 && scheduling=`Preemptive &&
		(typvertex=`Internal || typvertex=`Commute) &&
		(typpredvertex=`Internal || typpredvertex=`Commute)
	      in
	      if case1 || case2 || case3 then begin
		(* resource all outgoing edges of vertex to predvertex *)
		let succhedge = PSHGraph.succhedge cfg vertex in
		PSette.iter
		  (begin fun hedge ->
		    let instr = PSHGraph.attrhedge cfg hedge in
		    let succ = PSHGraph.succvertex cfg hedge in
		    PSHGraph.remove_hedge cfg hedge;
		    PSHGraph.add_hedge cfg hedge instr
		      ~pred:[|predvertex|] ~succ
		    ;
		  end)
		  succhedge
		;
		PSHGraph.remove_vertex cfg vertex;
		removed := PSette.add vertex !removed;
		if Cfg.compare.SHGraph.hashv.Hashhe.equal vertex info.pexit then
		  info.pexit <- predvertex
		;
		if case3 then
		  PSHGraph.replace_attrvertex cfg predvertex `Commute
		;
                if PDHashhe.memx prog.callret vertex then begin
                  let ret = PDHashhe.y_of_x prog.callret vertex in
                  PDHashhe.removex prog.callret vertex;
                  PDHashhe.add prog.callret predvertex ret 
                end;
		vertex_removed := true;
		loop := true;
	      end
	end
      end)
      vertices
    ;
  done;
  ()


(*  ********************************************************************** *)
(** {2 Identify points that can be labelled as internal} *)
(*  ********************************************************************** *)

(* Valid only in preemptive mode ! *)

let sette_add ?filter res var =
  if filter=None then failwith"";
  let b = match filter with
    | None -> true
    | Some f -> f var
  in
  if b then Sette.add var res else res

let instr_readwritevar row ?filter res instr =
  match instr with
  | Call(_,callinstr) ->
      List.fold_left (sette_add ?filter) res
	(begin match row with
	| `Read -> callinstr.cinput
	| `Write -> callinstr.coutput
	end)
  | Block(lbinstr) ->
      List.fold_left
	(begin fun res binstr ->
	  match binstr with
	  | Forget lvar ->
	      begin match row with
	      | `Read -> res
	      | `Write -> List.fold_left (sette_add ?filter) res lvar
	      end
	  | Condition(expr) ->
	      begin match row with
		| `Read -> Syntax.support ?filter res expr
		| `Write -> res
	      end
	  | Assign(lvar,lexpr) ->
	      begin match row with
	      | `Read ->
		  List.fold_left (Syntax.support ?filter) res lexpr
	      | `Write ->
		  List.fold_left (sette_add ?filter) res lvar
	      end
	  | Intro _| Elim _ -> res
	end)
	res lbinstr

let cfgproc_internalize_points ~readfilter ~writefilter (cfg:Cfg.syntax_t) =
  let info = PSHGraph.info cfg in
  if false then printf "info.pstart=%a@." Syntax.print_point info.pstart;

  PSHGraph.replace_attrvertex cfg info.pstart `Internal;
  let vertices =
    PSHGraph.fold_vertex cfg
      (fun vertex _ ~pred ~succ res -> vertex::res)
      []
  in
  List.iter
    (begin fun vertex ->
      let attr = PSHGraph.attrvertex cfg vertex in
      match attr with
      | `Commute ->
	  let predhedge = PSHGraph.predhedge cfg vertex in
	  let internal =
	    PSette.for_all
	      (begin fun hedge ->
		let instr = PSHGraph.attrhedge cfg hedge in
		begin
		  let writesupport = instr_readwritevar `Write ~filter:writefilter Sette.empty instr in
		  Sette.is_empty writesupport
		end
		&&
		  begin
		    let readsupport = instr_readwritevar `Read ~filter:readfilter Sette.empty instr in
		    Sette.is_empty readsupport
		  end
	      end)
	      predhedge
	  in
	  if internal then
	    PSHGraph.replace_attrvertex cfg vertex `Internal;
      | `Internal | `Fail | `Push -> ()
    end)
    vertices
  ;
  ()

(*  ********************************************************************** *)
(** {2 Identify basic blocks (linear chains of assignements and conditions} *)
(*  ********************************************************************** *)

(* Is the point internal, and does it have an unique predecessor
   and successor, labeled with assignements or conditions ?  (in
   such a case the point belong to a basic block)
*)

let is_point_linear (cfg:Cfg.syntax_t) point
    =
  let info = PSHGraph.info cfg in
  if is_userlabel point ||
    Cfg.compare.SHGraph.hashv.Hashhe.equal point info.pstart ||
    Cfg.compare.SHGraph.hashv.Hashhe.equal point info.pexit
  then
    None
  else begin
    let typ = PSHGraph.attrvertex cfg point in
    if typ = `Internal then begin
      let predhedge = PSHGraph.predhedge cfg point in
      if (PSette.cardinal predhedge) = 1 then begin
	let predhedge = PSette.choose predhedge in
	let predinstr = PSHGraph.attrhedge cfg predhedge in
	begin match predinstr with
	| Block predbinstr ->
	    let succhedge = PSHGraph.succhedge cfg point in
	    if (PSette.cardinal succhedge) = 1 then begin
	      let succhedge = PSette.choose succhedge in
	      let succinstr = PSHGraph.attrhedge cfg succhedge in
	      begin match succinstr with
	      | Block succbinstr ->
		  let predpoint = PSHGraph.predvertex cfg predhedge in
		  assert(Array.length predpoint = 1);
		  let succpoint = PSHGraph.succvertex cfg succhedge in
		  assert(Array.length succpoint = 1);
		  Some(predpoint.(0),predbinstr,point,succbinstr,succpoint.(0))
	      | _ -> None
	      end
	    end
	    else
	      None
	| _ -> None
	end
      end
      else None
    end
    else
      None
  end

let cfgproc_localblock ~removed (cfg:Cfg.syntax_t)
    =
  let seen = ref (PSette.empty Cfg.compare.SHGraph.comparev) in

  let rec explore_backward point res
      =
    let nres = is_point_linear cfg point in
    match nres with
    | Some(predpoint,predbinstr,point,succbinstr,succpoint) ->
	explore_backward predpoint nres
    | _ -> res
  in
  let rec explore_forward point rchain
      =
    if PSette.mem point !seen then
      (rchain,point)
    else begin
      seen := PSette.add point !seen;
      match is_point_linear cfg point with
      | Some(predpoint,predbinstr,point,succbinstr,succpoint) ->
	  explore_forward succpoint ((point,succbinstr)::rchain)
      | _ -> (rchain,point)
    end
  in

  let vertices =
    PSHGraph.fold_vertex cfg
      (fun point _ ~pred ~succ res -> point::res)
      []
  in
  List.iter
    (begin fun point ->
      if not (PSette.mem point !seen) then begin
	match explore_backward point None with
	| None -> ()
	| Some(predpoint,predbinstr,point,succbinstr,succpoint) ->
	    let (rchain,last) = explore_forward point [] in
	    if rchain<>[] then begin
	      let sequence =
		List.fold_left
		  (begin fun sequence (point,binstr) ->
		    PSHGraph.remove_vertex cfg point;
		    removed := PSette.add point !removed;
		    binstr @ sequence
		  end)
		  [] rchain
	      in
	      let sequence = predbinstr @ sequence in
	      let sequence =
		List.filter
		  (fun binstr -> not (is_binstr_truecondition binstr))
		  sequence
	      in
	      PSHGraph.add_hedge cfg (Cfg.unique()) (Block sequence)
		~pred:[|predpoint|] ~succ:[|last|]
	    end
      end
    end)
    vertices
  ;
  ()

(*  ********************************************************************** *)
(** {2 Main} *)
(*  ********************************************************************** *)

let procedure
    ~removed ~internalize ~truebranch ~basicblock ~scheduling
    ~writefilter ~readfilter
    prog proc
    =
  let cfg = proc.pext in
  if false then printf "cfg=%a@." Cfg.print_syntax cfg;
  begin match scheduling with
  | `Preemptive ->
      if internalize then
	cfgproc_internalize_points
	  ~readfilter ~writefilter
	  cfg
      ;
      if List.mem proc.pname prog.lthreads then begin
	let info = PSHGraph.info cfg in
	let attrvertex = PSHGraph.attrvertex cfg info.pstart in
	if attrvertex==`Internal then
	  PSHGraph.replace_attrvertex cfg info.pstart `Commute
	;
	let attrvertex = PSHGraph.attrvertex cfg info.pexit in
	if attrvertex==`Internal then
	  PSHGraph.replace_attrvertex cfg info.pexit `Commute
      end
  | `Cooperative ->
      ()
  end;
  if truebranch then
    cfgproc_truecondition ~removed ~scheduling prog cfg;
  if basicblock then
    cfgproc_localblock ~removed cfg;
  proc.pext <-
    (PSHGraph.map
      cfg
      (fun _ attr -> attr)
      (begin fun hedge instr ->
	begin match instr with
	| Cfg.Call _ -> instr
	| Cfg.Block list -> Cfg.Block (Block.simplify list)
	end
      end)
      (fun x -> x)
    );
  ()

let program
    ~removed ~internalize ~truebranch ~basicblock ~scheduling prog
    =
  Array.iter
    (begin fun thread ->
      Mappe.iter
	(begin fun pname proc ->
	  let writefilter = function
	    | `Global(v,`Current) when Sette.mem v thread.text.Dbconcur.globalreadother -> true
	    | _ -> false
	  in
	  let readfilter = function
	    | `Global(v,`Current) when Sette.mem v thread.text.Dbconcur.globalwriteother -> true
	    | _ -> false
	  in
	  procedure
	    ~writefilter ~readfilter
	    ~removed ~internalize ~truebranch ~basicblock ~scheduling
	    prog proc
	end)
	thread.tprocedures
    end)
    prog.threads
