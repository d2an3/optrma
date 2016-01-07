(** Unrolling loops *)

open Format
open Syntax

let freelabel clabel prefix =
  let rec parcours suffix =
    let candidate = prefix^(string_of_int suffix) in
    if not (Sette.mem candidate !clabel)
    then candidate
    else parcours (suffix+1)
  in
  let res = parcours 0 in
  clabel := Sette.add res !clabel;
  res

let freelabels clabel = 
  let beforegoto = freelabel clabel "roll" in
  let aftergoto = freelabel clabel "roll" in
  (beforegoto,aftergoto)

let point_rename index point = 
  point_substitute 
    (fun tab -> Array.append [|index|] tab) 
    point

let block_rename index block =
  Syntax.block_substitute
    ~env:()
    ~name:(fun () var -> var)
    ~point:(fun tab -> Array.append [|index|] tab) 
    ~scope:(fun () decl -> ())
    block

let build_if ~beforegoto ~aftergoto ~afterif ~loopexit index e =
  let note = match e with
    | None -> None
    | Some e -> Some (Syntax.enot e)
  in
  let instruction = IF(
    note,
    {
      bpoint = Lab(beforegoto,[|index|],false);
      instrs = [{
	instruction = GOTO(loopexit);
	ipoint = Lab(aftergoto,[|index|],false);
      }]
    },
    None
  ) in
  { instruction = instruction; ipoint = point_rename index afterif }

type res = 
  | Std of Syntax.var instr
  | Loop of Syntax.point * Syntax.var instr list

let rec map_instr
    ~clabel
    lpair
    bpoint (instr:Syntax.var instr)
    =
  match lpair with
  | [] -> Std(instr)
  | queue::body::l -> 
      begin match instr.instruction with
      | YIELD
      | SKIP
      | HALT
      | FAIL
      | ASSUME _
      | ASSIGN _
      | GOTO _
      | CALL _ -> Std(instr)
      | IF(e,b,ob) ->
	  let b = map_block ~clabel lpair b in
	  let ob = match ob with
	    | None -> None
	    | Some(b) -> Some(map_block ~clabel lpair b)
	  in
	  Std({ instr with instruction = IF(e,b,ob); })
      | ATOMIC(b) -> 
	  Std({ instr with instruction = ATOMIC(map_block ~clabel lpair b); })
      | LOCAL(flag,decl,b) ->
	  Std({ instr with instruction = LOCAL(flag,decl,map_block ~clabel lpair b); })
      | LOOP(e,b) when queue=0 && body=1 ->
	  Std({ instr with instruction = LOOP(e, map_block ~clabel l b) })
      | LOOP(e,b) ->	  
	  assert(queue>=0 && body>=1);
	  let b = map_block ~clabel l b in
	  let loopexit = instr.ipoint in
	  let (beforegoto,aftergoto) = freelabels clabel in
	  let afterif = b.bpoint in
	  let res = ref [] in
	  (* Prefix part *)
	  for index=0 to queue-1 do
	    let ifinstr = 
	      build_if
		~beforegoto ~aftergoto ~afterif ~loopexit index e
	    in
	    res := ifinstr :: !res;
	    let block = block_rename index b in
	    res := List.rev_append block.instrs !res;
	    res := { instruction = SKIP; ipoint = point_rename (index+1) bpoint} :: !res;
	  done;
	  (* loop body *)
	  let loopbody = 
	    (* first iteration *)
	    let block = block_rename queue b in
	    let loopbody = ref (List.rev block.instrs) in
	    if body>1 then
	       loopbody := { instruction = SKIP; ipoint = point_rename (queue+1) bpoint} :: !loopbody;
	    loopbody
	  in
	  for index=queue+1 to queue+body-1 do
	    let ifinstr = 
	      build_if
		~beforegoto ~aftergoto ~afterif ~loopexit index e
	    in
	    loopbody := ifinstr :: !loopbody;
	    let block = block_rename index b in
	    loopbody := List.rev_append block.instrs !loopbody;
	    if index<queue+body-1 then
	      loopbody := { instruction = SKIP; ipoint = point_rename (index+1) bpoint} :: !loopbody;
	  done;
	  let loopbody = { 
	    bpoint = point_rename queue b.bpoint;
	    instrs = List.rev !loopbody
	  }
	  in
	  let loop = {
	    instruction = LOOP(e,loopbody);
	    ipoint = loopexit;
	  }
	  in
	  res := loop :: !res;
	  Loop(point_rename 0 bpoint, List.rev !res)
      end  
  | _ -> failwith ""

and map_block ~clabel lpair block =
  let (_,blockpoint,revninstrs) =
    List.fold_left
      (begin fun (bpoint,blockpoint,revninstrs) instr ->
	let res = map_instr ~clabel lpair bpoint instr in
	match res with
	| Std(ninstr) ->
	    (instr.ipoint,blockpoint,ninstr::revninstrs)
	| Loop(nblockpoint,lninstrs) ->
	    let (nblockpoint,revninstrs) = match revninstrs with
	      | last::rest -> (blockpoint, { last with ipoint = nblockpoint }::rest)
	      | [] -> (nblockpoint,[])
	    in
	    (instr.ipoint, nblockpoint, List.rev_append lninstrs revninstrs)
      end)
      (block.bpoint,block.bpoint,[])
      block.instrs
  in
  { 
    bpoint = blockpoint;
    instrs = List.rev revninstrs
  }

(*  ====================================================================== *)
(** {2 Main} *)
(*  ====================================================================== *)

let main
    (prog:Syntax.var Syntax.program)
    (lpair:int list)
    :
    Syntax.var Syntax.program
    =
  let (clabel,_) = Check.controllabels prog in
  let clabel = ref clabel in
  let nprog =
    { prog with procedures =
	List.map
	  (begin fun (pname,proc) ->
	    let nproc = 
	      { proc with
		pcode = map_block ~clabel lpair proc.pcode }
	    in
	    (pname,nproc)
	  end)
	  prog.procedures
    }
  in
  nprog
