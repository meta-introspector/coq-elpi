(* coq-elpi: Coq terms as the object language of elpi                        *)
(* license: GNU Lesser General Public License Version 2.1 or later           *)
(* ------------------------------------------------------------------------- *)

module API = Elpi.API
module E = API.RawData
module State = API.State
module P = API.RawPp
module Conv = API.Conversion
module CConv = API.ContextualConversion
module B = struct
  include API.BuiltInData
  include Elpi.Builtin
  let ioarg = API.BuiltInPredicate.ioarg
  let ioargC = API.BuiltInPredicate.ioargC
end
module Pred = API.BuiltInPredicate

module G = Globnames
module CNotation = Notation

open Names

open Coq_elpi_utils
open Coq_elpi_HOAS

let tactic_mode = ref false
let on_global_state api thunk = (); (fun state ->
  if !tactic_mode then
    Coq_elpi_utils.err Pp.(strbrk ("API " ^ api ^ " cannot be used in tactics"));
  let state, result, gls = thunk state in
  Coq_elpi_HOAS.grab_global_env state, result, gls)

(* This is for stuff that is not monotonic in the env, eg section closing *)
let on_global_state_does_rewind_env api thunk = (); (fun state ->
  if !tactic_mode then
    Coq_elpi_utils.err Pp.(strbrk ("API " ^ api ^ " cannot be used in tactics"));
  let state, result, gls = thunk state in
  Coq_elpi_HOAS.grab_global_env_drop_univs state, result, gls)

let warn_if_contains_univ_levels ~depth t =
  let fold f acc ~depth t =
    match t with
    | E.Const _ | E.Nil | E.CData _ -> acc
    | E.App(_,x,xs) -> List.fold_left (f ~depth) (f ~depth acc x) xs
    | E.Cons(x,xs) -> f ~depth (f ~depth acc x) xs
    | E.Builtin(_,xs) -> List.fold_left (f ~depth) acc xs
    | E.Lam x -> f ~depth:(depth+1) acc x
    | E.UnifVar(_,xs) -> List.fold_left (f ~depth) acc xs
  in
  let global_univs = UGraph.domain (Environ.universes (Global.env ())) in
  let is_global u =
    match Univ.Universe.level u with
    | None -> true
    | Some l -> Univ.LSet.mem l global_univs in
  let rec aux ~depth acc t =
    match E.look ~depth t with
    | E.CData c when isuniv c -> let u = univout c in if is_global u then acc else u :: acc
    | x -> fold aux acc ~depth x
  in
  let univs = aux ~depth [] t in
  if univs <> [] then
    err Pp.(strbrk "The hypothetical clause contains terms of type univ which are not global, you should abstract them out or replace them by global ones: " ++
            prlist_with_sep spc Univ.Universe.pr univs)
;;

let bool = B.bool
let int = B.int
let list = B.list
let pair = B.pair
let option = B.option

let constraint_leq u1 u2 =
  let open UnivProblem in
  ULe (u1, u2)

let constraint_eq u1 u2 =
  let open UnivProblem in
  ULe (u1, u2)

let add_universe_constraint state c =
  let open UnivProblem in
  try add_constraints state (Set.singleton c)
  with
  | Univ.UniverseInconsistency p ->
      Feedback.msg_debug
        (Univ.explain_universe_inconsistency
           UnivNames.pr_with_global_universes p);
      raise Pred.No_clause
  | Evd.UniversesDiffer | UState.UniversesDiffer ->
      Feedback.msg_debug Pp.(str"UniversesDiffer");
      raise Pred.No_clause

let mk_fresh_univ state = new_univ state

let mk_algebraic_super x = Univ.super x
let mk_algebraic_max x y = Univ.Universe.sup x y

(* I don't want the user to even know that algebraic universes exist *)
let purge_1_algebraic_universe state u =
  if Univ.Universe.is_level u then state, u
  else
    let state, v = mk_fresh_univ state in
    add_universe_constraint state (constraint_leq u v), v

let purge_algebraic_univs state t =
  let sigma = get_sigma state in
  (* no map_fold iterator :-/ *)
  let state = ref state in
  let rec aux t =
    match EConstr.kind sigma t with
    | Constr.Sort s -> begin
        match EConstr.ESorts.kind sigma s with
        | Sorts.Type u ->
            let new_state, v = purge_1_algebraic_universe !state u in
            state := new_state;
            EConstr.mkType v
        | _ -> EConstr.map sigma aux t
        end
    | _ -> EConstr.map sigma aux t in
  let t = aux t in
  !state, t

let univ_super state u v =
  let state, u =
    if Univ.Universe.is_level u then state, u
    else
      let state, w = mk_fresh_univ state in
      add_universe_constraint state (constraint_leq u w), w in
    add_universe_constraint state (constraint_leq (mk_algebraic_super u) v)

let univ_max state u1 u2 =
  let state, v = mk_fresh_univ state in
  let state =
    add_universe_constraint state (constraint_leq (mk_algebraic_max u1 u2) v) in
  state, v

let constr2lp ~depth hyps constraints state t =
  let state, t = purge_algebraic_univs state t in
  constr2lp ~depth hyps constraints state t

let constr2lp_closed ~depth hyps constraints state t =
  let state, t = purge_algebraic_univs state t in
  constr2lp_closed ~depth hyps constraints state t

let constr2lp_closed_ground ~depth state t =
  let state, t = purge_algebraic_univs state t in
  constr2lp_closed_ground ~depth state t

let clauses_for_later =
  State.declare ~name:"coq-elpi:clauses_for_later"
    ~init:(fun () -> [])
    ~pp:(fun fmt l ->
       List.iter (fun (dbname, code) ->
         Format.fprintf fmt "db:%s code:%a\n"
              (String.concat "." dbname)
            Elpi.API.Pp.Ast.program code) l)
;;

let univ = { univ with
  Conv.readback = (fun ~depth state x ->
    let state, u, gl = univ.Conv.readback ~depth state x in
    let state, u = purge_1_algebraic_universe state u in
    state, u, gl);
  embed = (fun ~depth state x ->
    let state, x = purge_1_algebraic_universe state x in
    let state, u, gl = univ.Conv.embed ~depth state x in
    state, u, gl);
}

let term = {
  CConv.ty = Conv.TyName "term";
  pp_doc = (fun fmt () -> Format.fprintf fmt "A Coq term containing evars");
  pp = (fun fmt t -> Format.fprintf fmt "%s" (Pp.string_of_ppcmds (Printer.pr_econstr_env (Global.env()) Evd.empty t)));
  readback = lp2constr;
  embed = constr2lp;
}
let proof_context : (full coq_context, API.Data.constraints) CConv.ctx_readback =
  fun ~depth hyps constraints state ->
    let state, proof_context, _, gls = get_current_env_sigma ~depth hyps constraints state in
    state, proof_context, constraints, gls

let closed_term = {
  CConv.ty = Conv.TyName "term";
  pp_doc = (fun fmt () -> Format.fprintf fmt "A closed Coq term");
  pp = (fun fmt t -> Format.fprintf fmt "%s" (Pp.string_of_ppcmds (Printer.pr_econstr_env (Global.env()) Evd.empty t)));
  readback = lp2constr_closed;
  embed = constr2lp_closed
}
let global : (empty coq_context, API.Data.constraints) CConv.ctx_readback =
  fun ~depth hyps constraints state ->
    let env, _ = get_global_env_sigma state in (* this env has more univ constraints *)
    let coq_ctx = mk_coq_context ~options:(get_options ~depth hyps) state in
    state, { coq_ctx with env }, constraints, []

let closed_ground_term = {
  Conv.ty = Conv.TyName "term";
  pp_doc = (fun fmt () -> Format.fprintf fmt "A ground, closed, Coq term");
  pp = (fun fmt t -> Format.fprintf fmt "%s" (Pp.string_of_ppcmds (Printer.pr_econstr_env (Global.env()) Evd.empty t)));
  readback = lp2constr_closed_ground;
  embed = constr2lp_closed_ground
}

let prop = { B.any with Conv.ty = Conv.TyName "prop" }
let raw_goal = { B.any with Conv.ty = Conv.TyName "goal" }

let id = { B.string with
  API.Conversion.ty = Conv.TyName "id";
  pp_doc = (fun fmt () ->
    Format.fprintf fmt "%% [id] is a name that matters, we piggy back on Elpi's strings.@\n";
    Format.fprintf fmt "%% Note: [name] is a name that does not matter.@\n";
    Format.fprintf fmt "typeabbrev id string.@\n@\n")
}


let flag name = { (unspec bool) with Conv.ty = Conv.TyName name }

(* Unfortunately the data tye is not symmeteric *)
let indt_decl_in = {
  Conv.ty = Conv.TyName "indt-decl";
  pp_doc = (fun fmt () -> Format.fprintf fmt "Declaration of an inductive type");
  pp = (fun fmt _ -> Format.fprintf fmt "mutual_inductive_entry");
  readback = (fun ~depth state t -> lp2inductive_entry ~depth (mk_coq_context ~options:API.Data.StrMap.empty state) E.no_constraints state t);
  embed = (fun ~depth state t -> assert false);
}
let indt_decl_out = {
  Conv.ty = Conv.TyName "indt-decl";
  pp_doc = (fun fmt () -> Format.fprintf fmt "Declaration of an inductive type");
  pp = (fun fmt _ -> Format.fprintf fmt "mutual_inductive_entry");
  readback = (fun ~depth state t -> assert false);
  embed = (fun ~depth state t -> inductive_decl2lp ~depth (mk_coq_context ~options:API.Data.StrMap.empty state) E.no_constraints state t);
}

let is_ground sigma t = Evar.Set.is_empty (Evd.evars_of_term sigma t)
let is_ground_one_inductive_entry sigma { Entries.mind_entry_arity; mind_entry_lc } =
  is_ground sigma (EConstr.of_constr mind_entry_arity) &&
  List.for_all (is_ground sigma) @@ List.map EConstr.of_constr mind_entry_lc
let is_ground_rel_ctx_entry sigma rc =
  is_ground sigma @@ EConstr.of_constr @@ Context.Rel.Declaration.get_type rc &&
  Option.cata (fun x -> is_ground sigma @@ EConstr.of_constr x) true @@ Context.Rel.Declaration.get_value rc
let is_mutual_inductive_entry_ground { Entries.mind_entry_params; mind_entry_inds } sigma =
  List.for_all (is_ground_rel_ctx_entry sigma) mind_entry_params &&
  List.for_all (is_ground_one_inductive_entry sigma) mind_entry_inds

type located =
  | LocGref of Names.GlobRef.t
  | LocModule of Names.ModPath.t
  | LocModuleType of Names.ModPath.t
  | LocAbbreviation of Globnames.syndef_name

let located = let open Conv in let open API.AlgebraicData in declare {
  ty = TyName "located";
  doc = "Result of coq.locate-all";
  pp = (fun fmt _ -> Format.fprintf fmt "<todo>");
  constructors = [
    K("loc-gref","",A(gref,N),
        B (fun x -> LocGref x),
        M (fun ~ok ~ko -> function LocGref x -> ok x | _ -> ko ()));
    K("loc-modpath","",A(modpath,N),
        B (fun x -> LocModule x),
        M (fun ~ok ~ko -> function LocModule x -> ok x | _ -> ko ()));
    K("loc-modtypath","",A(modtypath,N),
        B (fun x -> LocModuleType x),
        M (fun ~ok ~ko -> function LocModuleType x -> ok x | _ -> ko ()));
    K("loc-abbreviation","",A(abbreviation,N),
        B (fun x -> LocAbbreviation x),
        M (fun ~ok ~ko -> function LocAbbreviation x -> ok x | _ -> ko ()));
  ]
} |> CConv.(!<)

(* FIXME PARTIAL API
 *
 * Record foo A1..Am := K { f1; .. fn }.   -- m params, n fields
 * Canonical c (x1 : b1)..(xk : bk) := K p1..pm t1..tn.
 *
 *   fi v1..vm ? rest1  ==  (ci w1..wr) rest2
 *
 *   ?i : bi
 *   vi =?= pi[xi/?i]
 *   wi =?= ui[xi/?i]
 *   ?  == c ?1 .. ?k
 *   rest1 == rest2
 *   ?j =<= (ci w1..wr)    -- default proj, ti = xj
 *   ci == gr
 *
 *   unif (const fi) [V1,..VM, C | R1] (const ci) [W1,..WR| R2] M U :-
 *     of (app[c, ?1,..?k]) _ CR, -- saturate
 *     hd-beta CR [] (indc _) [P1,..PM,T1,..TN],
 *     unify-list-U Vi Pi,
 *     Ti = app[const ci|U1,..,UN],
 *     unify-list-U Wi Ui,
 *     unify-eq C CR,
 *     unify-list-eq R1 R2.
 *
 *)



let cs_pattern =
  let open Conv in let open API.AlgebraicData in let open Recordops in declare {
  ty = TyName "cs-pattern";
  doc = "Pattern for canonical values";
  pp = (fun fmt -> function
    | Const_cs x -> Format.fprintf fmt "Const_cs %s" "<todo>"
    | Prod_cs -> Format.fprintf fmt "Prod_cs"
    | Sort_cs _ ->  Format.fprintf fmt "Sort_cs"
    | Default_cs -> Format.fprintf fmt "Default_cs");
  constructors = [
    K("cs-gref","",A(gref,N),
      B (fun x -> Const_cs x),
      M (fun ~ok ~ko -> function Const_cs x -> ok x | _ -> ko ()));
    K("cs-prod","",N,
      B Prod_cs,
      M (fun ~ok ~ko -> function Prod_cs -> ok | _ -> ko ()));
    K("cs-default","",N,
      B Default_cs,
      M (fun ~ok ~ko -> function Default_cs -> ok | _ -> ko ()));
    K("cs-sort","",A(universe,N),
      B (fun s -> Sort_cs (Sorts.family s)),
      MS (fun ~ok ~ko p state -> match p with
        | Sort_cs Sorts.InSet -> ok Sorts.set state
        | Sort_cs Sorts.InProp -> ok Sorts.prop state
        | Sort_cs Sorts.InType ->
              let state, u = mk_fresh_univ state in
              ok (Sorts.sort_of_univ u) state
        | _ -> ko state))
  ]
} |> CConv.(!<)

let cs_instance = let open Conv in let open API.AlgebraicData in let open Recordops in declare {
  ty = TyName "cs-instance";
  doc = "Canonical Structure instances: (cs-instance Proj ValPat Inst)";
  pp = (fun fmt (_,{ o_DEF }) -> Format.fprintf fmt "%s" Pp.(string_of_ppcmds (Printer.pr_constr_env (Global.env()) Evd.empty o_DEF)));
  constructors = [
    K("cs-instance","",A(gref,A(cs_pattern,A(closed_ground_term,N))), (* XXX should be a gref *)
      B (fun p v i -> assert false),
      M (fun ~ok ~ko ((proj_gr,patt), {
  o_DEF = solution;       (* c *)
  o_CTX = uctx_set;
  o_INJ = def_val_pos;    (* Some (j \in [0-n]) if ti = xj *)
  o_TABS = types;         (* b1 .. bk *)
  o_TPARAMS = params;     (* p1 .. pm *)
  o_NPARAMS = nparams;    (* m *)
  o_TCOMPS = cval_args }) -> ok proj_gr patt (EConstr.of_constr solution)))
  ]
} |> CConv.(!<)

let tc_instance = let open Conv in let open API.AlgebraicData in let open Typeclasses in declare {
  ty = TyName "tc-instance";
  doc = "Type class instance with priority";
  pp = (fun fmt _ -> Format.fprintf fmt "<todo>");
  constructors = [
    K("tc-instance","",A(gref,A(int,N)),
      B (fun g p -> nYI "lp2instance"),
      M (fun ~ok ~ko i ->
          ok (instance_impl i) (Option.default 0 (hint_priority i))));
]} |> CConv.(!<)

type scope = ExecutionSite | CurrentModule

let scope = let open Conv in let open API.AlgebraicData in declare {
  ty = TyName "scope";
  doc = "Specify to which module the clause should be attached to";
  pp = (fun fmt _ -> Format.fprintf fmt "<todo>");
  constructors = [
    K("execution-site","The module inside which the Elpi program is run",N,
      B ExecutionSite,
      M (fun ~ok ~ko -> function ExecutionSite -> ok | _ -> ko ()));
    K("current","The module being defined (see begin/end-module)",N,
      B CurrentModule,
      M (fun ~ok ~ko -> function CurrentModule -> ok | _ -> ko ()))
  ]
} |> CConv.(!<)

let grafting = let open Conv in let open API.AlgebraicData in declare {
  ty = TyName "grafting";
  doc = "Specify if the clause has to be grafted before or after a named clause";
  pp = (fun fmt _ -> Format.fprintf fmt "<todo>");
  constructors = [
    K("before","",A(id,N),
        B (fun x -> (`Before,x)),
        M (fun ~ok ~ko -> function (`Before,x) -> ok x | _ -> ko ()));
    K("after","",A(id,N),
        B (fun x -> (`After,x)),
        M (fun ~ok ~ko -> function (`After,x) -> ok x | _ -> ko ()));
  ]
} |> CConv.(!<)

let clause = let open Conv in let open API.AlgebraicData in declare {
  ty = TyName "clause";
  doc = {|clauses

A clause like
 :name "foo" :before "bar" foo X Y :- bar X Z, baz Z Y
is represented as
 clause _ "foo" (before "bar") (pi x y z\ foo x y :- bar x z, baz z y)
that is exactly what one would load in the context using =>.

The name and the grafting specification can be left unspecified.|};
  pp = (fun fmt _ -> Format.fprintf fmt "<todo>");
  constructors = [
    K("clause","",A(unspec id,A(unspec grafting,A(prop,N))),
      B (fun id graft c -> unspec2opt id, unspec2opt graft, c),
      M (fun ~ok ~ko (id,graft,c) -> ok (opt2unspec id) (opt2unspec graft) c));
  ]
} |> CConv.(!<)

let set_accumulate_to_db, get_accumulate_to_db =
  let f = ref ((fun () -> assert false),(fun _ -> assert false)) in
  (fun x -> f := x),
  (fun () -> !f)

let class_ = let open Conv in let open API.AlgebraicData in let open Classops in declare {
  ty = TyName "class";
  doc = "Node of the coercion graph";
  pp = (fun fmt _ -> Format.fprintf fmt "<todo>");
  constructors = [
   K("funclass","",N,
     B CL_FUN,
     M (fun ~ok ~ko -> function Classops.CL_FUN -> ok | _ -> ko ()));
   K("sortclass","",N,
     B CL_SORT,
     M (fun ~ok ~ko -> function CL_SORT -> ok | _ -> ko ()));
   K("grefclass","",A(gref,N),
     B Class.class_of_global,
     M (fun ~ok ~ko -> function
     | CL_SECVAR v -> ok (GlobRef.VarRef v)
     | CL_CONST c -> ok (GlobRef.ConstRef c)
     | CL_IND i -> ok (GlobRef.IndRef i)
     | CL_PROJ p -> ok (GlobRef.ConstRef (Projection.Repr.constant p))
     | _ -> ko ()))
]
} |> CConv.(!<)

let src_class_of_class = function
  | (Classops.CL_FUN | Classops.CL_SORT) -> CErrors.anomaly Pp.(str "src_class_of_class on a non source coercion class")
  | Classops.CL_SECVAR v -> GlobRef.VarRef v
  | Classops.CL_CONST c -> GlobRef.ConstRef c
  | Classops.CL_IND i -> GlobRef.IndRef i
  | Classops.CL_PROJ p -> GlobRef.ConstRef (Projection.Repr.constant p)

let coercion = let open Conv in let open API.AlgebraicData in declare {
  ty = TyName "coercion";
  doc = "Edge of the coercion graph";
  pp = (fun fmt _ -> Format.fprintf fmt "<todo>");
  constructors =  [
    K("coercion","ref, nparams, src, tgt", A(gref,A(unspec int,A(gref,A(class_,N)))),
      B (fun t np src tgt -> t,np,src,tgt),
      M (fun ~ok ~ko:_ -> function (t,np,src,tgt) -> ok t np src tgt))
  ]
} |> CConv.(!<)

let implicit_kind_of_status = function
  | None -> Impargs.NotImplicit
  | Some (_,_,(maximal,_)) ->
      if maximal then Impargs.MaximallyImplicit else Impargs.Implicit


let simplification_strategy = let open API.AlgebraicData in let open Reductionops.ReductionBehaviour in declare {
  ty = Conv.TyName "simplification_strategy";
  doc = "Strategy for simplification tactics";
  pp = (fun fmt (x : t) -> Format.fprintf fmt "TODO");
  constructors = [
    K ("never", "Arguments foo : simpl never",N,
      B NeverUnfold,
      M (fun ~ok ~ko -> function NeverUnfold -> ok | _ -> ko ()));
    K("when","Arguments foo .. / .. ! ..",A(B.list B.int, A(Elpi.Builtin.option B.int, N)),
      B (fun recargs nargs -> UnfoldWhen { recargs; nargs }),
      M (fun ~ok ~ko -> function UnfoldWhen { recargs; nargs } -> ok recargs nargs | _ -> ko ()));
    K("when-nomatch","Arguments foo .. / .. ! .. : simpl moatch",A(B.list B.int, A(Elpi.Builtin.option B.int, N)),
      B (fun recargs nargs -> UnfoldWhenNoMatch { recargs; nargs }),
      M (fun ~ok ~ko -> function UnfoldWhenNoMatch { recargs; nargs } -> ok recargs nargs | _ -> ko ()));
  ]
} |> CConv.(!<)

let attribute a = let open API.AlgebraicData in declare {
  ty = Conv.TyName "attribute";
  doc = "Generic attribute";
  pp = (fun fmt a -> Format.fprintf fmt "TODO");
  constructors = [
    K("attribute","",A(B.string,A(a,N)),
      B (fun s a -> s,a),
      M (fun ~ok ~ko -> function (s,a) -> ok s a));
  ]
} |> CConv.(!<)

let attribute_value = let open API.AlgebraicData in let open Attributes in let open CConv in declare {
  ty = Conv.TyName "attribute-value";
  doc = "Generic attribute value";
  pp = (fun fmt a -> Format.fprintf fmt "TODO");
  constructors = [
    K("leaf","",A(B.string,N),
      B (fun s -> if s = "" then VernacFlagEmpty else VernacFlagLeaf s),
      M (fun ~ok ~ko -> function VernacFlagEmpty -> ok "" | VernacFlagLeaf x -> ok x | _ -> ko ()));
    K("node","",C((fun self -> !> (B.list (attribute (!< self)))),N),
      B (fun l -> VernacFlagList l),
      M (fun ~ok ~ko -> function VernacFlagList l -> ok l | _ -> ko ())
    )
  ]
} |> CConv.(!<)

let attribute = attribute attribute_value

let warning = CWarnings.create ~name:"lib" ~category:"elpi" Pp.str

let if_keep x f =
  match x with
  | Pred.Discard -> None
  | Pred.Keep -> Some (f ())

let if_keep_acc x state f =
  match x with
  | Pred.Discard -> state, None
  | Pred.Keep ->
       let state, x = f state in
       state, Some x

let detype env sigma t =
    (* To avoid turning named universes into unnamed ones *)
    Flags.with_option Constrextern.print_universes
      (Detyping.detype Detyping.Now false Id.Set.empty env sigma) t

let version_parser version =
  let (!!) x = try int_of_string x with Failure _ -> -100 in
  match Str.split (Str.regexp_string ".") version with
  | major :: minor :: patch :: _ -> !!major, !!minor, !!patch
  | [ major ] -> !!major,0,0
  | [] -> 0,0,0
  | [ major; minor ] ->
      match Str.split (Str.regexp_string "+") minor with
      | [ minor ] -> !!major, !!minor, 0
      | [ ] -> !!major, !!minor, 0
      | minor :: prerelease :: _ ->
          if Str.string_match (Str.regexp_string "beta") prerelease 0 then
            !!major, !!minor, !!("-"^String.sub prerelease 4 (String.length prerelease - 4))
          else if Str.string_match (Str.regexp_string "alpha") prerelease 0 then
            !!major, !!minor, !!("-"^String.sub prerelease 5 (String.length prerelease - 5))
          else !!major, !!minor, -100

let gr2path state gr =
    let rec mp2sl = function
      | MPfile dp -> CList.rev_map Id.to_string (DirPath.repr dp)
      | MPbound id ->
          let _,id,dp = MBId.repr id in
          mp2sl (MPfile dp) @ [ Id.to_string id ]
      | MPdot (mp,lbl) -> mp2sl mp @ [Label.to_string lbl] in
    match gr with
    | Names.GlobRef.VarRef v -> [Id.to_string v]
    | Names.GlobRef.ConstRef c -> (mp2sl @@ Constant.modpath c)
    | Names.GlobRef.IndRef (i,0) ->
        let open Declarations in
        let env, sigma = get_global_env_sigma state in
        let { mind_packets } = Environ.lookup_mind i env in
         ((mp2sl @@ MutInd.modpath i) @ [ Id.to_string (mind_packets.(0).mind_typename)])
    | Names.GlobRef.ConstructRef ((i,0),j) ->
        let env, sigma = get_global_env_sigma state in
        let open Declarations in
        let { mind_packets } = Environ.lookup_mind i env in
        let klbl = Id.to_string (mind_packets.(0).mind_consnames.(j-1)) in
        ((mp2sl @@ MutInd.modpath i) @ [klbl])
    | Names.GlobRef.IndRef _  | Names.GlobRef.ConstructRef _ ->
          nYI "mutual inductive (make-derived...)"

let coq_builtins =
  let open API.BuiltIn in
  let open Pred in
  let open Notation in
  let open CConv in
  let pp ~depth = P.term depth in
  [LPCode
{|% Coq terms as the object language of elpi and basic API to access Coq
% license: GNU Lesser General Public License Version 2.1 or later
% -------------------------------------------------------------------------

% This file is automatically generated from
%  - coq-HOAS.elpi
%  - coq_elpi_builtin.ml
% and contains the descritpion of the data type of Coq terms and the
% API to access Coq.

|};
  LPCode Coq_elpi_builtins_HOAS.code;
  MLData Coq_elpi_HOAS.record_field_att;
  LPCode {|
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% builtins %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This section contains the API to access Coq
% The marker *E* means *experimental*, i.e. use at your own risk, it may change
% substantially or even disappear in future versions.
|};

  LPDoc "-- Misc ---------------------------------------------------------";

  MLCode(Pred("coq.say",
    VariadicIn(unit_ctx, !> B.any, "Prints an info message"),
  (fun args ~depth _hyps _constraints state ->
     let pp = pp ~depth in
     Feedback.msg_notice Pp.(str (pp2string (P.list ~boxed:true pp " ") args));
     state, ())),
  DocAbove);

  MLCode(Pred("coq.warn",
    VariadicIn(unit_ctx, !> B.any, "Prints a warning message"),
  (fun args ~depth _hyps _constraints state ->
     let pp = pp ~depth in
     let loc, args =
       if args = [] then None, args
       else
         let x, args = List.hd args, List.tl args in
         match E.look ~depth x with
         | E.CData loc when API.RawOpaqueData.is_loc loc ->
           Some (Coq_elpi_utils.to_coq_loc (API.RawOpaqueData.to_loc loc)), args
         | _ -> None, x :: args
     in
     warning ?loc (pp2string (P.list ~boxed:true pp " ") args);
     state, ())),
  DocAbove);

  MLCode(Pred("coq.error",
    VariadicIn(unit_ctx, !> B.any, "Prints and *aborts* the program (it's a fatal error)"),
  (fun args ~depth _hyps _constraints _state ->
     let pp = pp ~depth in
     err Pp.(str (pp2string (P.list ~boxed:true pp " ") args)))),
  DocAbove);

  MLCode(Pred("coq.version",
    Out(B.string, "VersionString",
    Out(int, "Major",
    Out(int, "Minor",
    Out(int, "Patch",
    Easy "Fetches the version of Coq, as a string and as 3 numbers")))),
    (fun _ _ _ _ ~depth:_ ->
      let version = Coq_config.version in
      let major, minor, patch = version_parser version in
      !: version +! major +! minor +! patch)),
  DocAbove);
  LPCode {|
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API for objects belonging to the logic
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|};
  LPDoc "-- Environment: names -----------------------------------------------";
  LPDoc {|To make the API more precise we use different data types for the names of global objects.
Note: [ctype \"bla\"] is an opaque data type and by convention it is written [@bla].|};

  MLData constant;
  MLData inductive;
  MLData constructor;
  MLData gref;
  MLData id;
  MLData modpath;
  MLData modtypath; ] @

  [
  LPDoc "-- Environment: read ------------------------------------------------";
  LPDoc "Note: The type [term] is defined in coq-HOAS.elpi";

  MLData located;

  MLCode(Pred("coq.locate-all",
    In(id, "Name",
    Out(B.list located,  "Located",
    Easy {|finds all posible meanings of a string. Does not fail.|})),
  (fun s _ ~depth ->
    let qualid = Libnames.qualid_of_string s in
    let l = ref [] in
    let add x = l := !l @ [x] in
    begin
      match Nametab.locate_extended qualid with
      | G.TrueGlobal gr -> add @@ LocGref gr
      | G.SynDef sd ->
          begin match Syntax_def.search_syntactic_definition sd with
          | _, Notation_term.NRef gr -> add @@ LocGref gr
          | _ -> add @@ LocAbbreviation sd
          | exception Not_found -> () end
      | exception Not_found -> ()
    end;
    begin
      try add @@ LocModule (Nametab.locate_module qualid)
      with Not_found -> ()
    end;
    begin
      try add @@ LocModuleType (Nametab.locate_modtype qualid)
      with Not_found -> ()
    end;
    !: !l)),
  DocAbove);

  MLCode(Pred("coq.locate",
    In(id, "Name",
    Out(gref,  "GlobalReference",
    Easy {|locates a global definition, inductive type or constructor via its name.
It unfolds syntactic notations, e.g. "Notation old_name := new_name."
It undestands qualified names, e.g. "Nat.t". It's a fatal error if Name cannot be located.|})),
  (fun s _ ~depth ->
    let qualid = Libnames.qualid_of_string s in
    let gr =
      try
        match Nametab.locate_extended qualid with
        | G.TrueGlobal gr -> gr
        | G.SynDef sd ->
           match Syntax_def.search_syntactic_definition sd with
           | _, Notation_term.NRef gr -> gr
           | _ -> nYI "complex call to Locate"
        with Not_found ->
            err Pp.(str "Global reference not found: " ++ Libnames.pr_qualid qualid) in
    !: gr)),
  DocAbove);


  MLCode(Pred("coq.env.typeof",
    In(gref, "GR",
    Out(closed_ground_term, "Ty",
    Full(unit_ctx, "reads the type Ty of a global reference."))),
  (fun gr _ ~depth _ _ state ->
    let state, ty = type_of_global state gr in
    state, !:ty, [])),
  DocAbove);

  MLCode(Pred("coq.env.indt",
    In(inductive, "reference to the inductive type",
    Out(bool, "tt if the type is inductive (ff for co-inductive)",
    Out(int,  "number of parameters",
    Out(int,  "number of parameters that are uniform (<= parameters)",
    Out(closed_ground_term, "type of the inductive type constructor including parameters",
    Out(list constructor, "list of constructor names",
    Out(list closed_ground_term, "list of the types of the constructors (type of KNames) including parameters",
    Full(global, "reads the inductive type declaration for the environment")))))))),
  (fun i _ _ _ arity knames ktypes ~depth { env } _ state ->
     let open Declarations in
     let mind, indbo as ind = lookup_inductive env i in
     let co  = mind.mind_finite <> Declarations.CoFinite in
     let lno = mind.mind_nparams in
     let luno = mind.mind_nparams_rec in
     let arity = if_keep arity (fun () ->
       Inductive.type_of_inductive env (ind,Univ.Instance.empty)
       |> EConstr.of_constr) in
     let knames = if_keep knames (fun () ->
       CList.(init Declarations.(indbo.mind_nb_constant + indbo.mind_nb_args)
           (fun k -> i,k+1))) in
     let ktypes = if_keep ktypes (fun () ->
       Inductive.type_of_constructors (i,Univ.Instance.empty) ind
       |> CArray.map_to_list EConstr.of_constr) in
     state, !: co +! lno +! luno +? arity +? knames +? ktypes, [])),
  DocNext);

  MLCode(Pred("coq.env.indt-decl",
    In(inductive, "reference to the inductive type",
    Out(indt_decl_out,"HOAS description of the inductive type",
    Full(global,"reads the inductive type declaration for the environment"))),
  (fun i _ ~depth { env } _ state  ->
     let mind, indbo = lookup_inductive env i in
     let knames = CList.(init Declarations.(indbo.mind_nb_constant + indbo.mind_nb_args) (fun k -> GlobRef.ConstructRef(i,k+1))) in
     let k_impls = List.map (fun x -> Impargs.extract_impargs_data (Impargs.implicits_of_global x)) knames in
     let hd x = match x with [] -> [] | (_,x) :: _ -> List.map implicit_kind_of_status x in
     let k_impls = List.map hd k_impls in
     let i_impls = Impargs.extract_impargs_data @@ Impargs.implicits_of_global (GlobRef.IndRef i) in
     let i_impls = hd i_impls in
     state, !: ((mind,indbo), (i_impls,k_impls)), [])),
  DocNext);

  MLCode(Pred("coq.env.indc",
    In(constructor, "GR",
    Out(int, "ParamNo",
    Out(int, "UnifParamNo",
    Out(int, "Kno",
    Out(closed_ground_term,"Ty",
    Full (global, "reads the type Ty of an inductive constructor GR, as well as "^
          "the number of parameters ParamNo and uniform parameters "^
          "UnifParamNo and the number of the constructor Kno (0 based)")))))),
  (fun (i,k as kon) _ _ _ ty ~depth { env } _ state ->
    let open Declarations in
    let mind, indbo as ind = Inductive.lookup_mind_specif env i in
    let lno = mind.mind_nparams in
    let luno = mind.mind_nparams_rec in
    let ty = if_keep ty (fun () ->
      Inductive.type_of_constructor (kon,Univ.Instance.empty) ind
      |> EConstr.of_constr) in
    state, !: lno +! luno +! (k-1) +? ty, [])),
  DocAbove);

  MLCode(Pred("coq.env.const-opaque?",
    In(constant, "GR",
    Read(global, "checks if GR is an opaque constant")),
  (fun c ~depth {env} _ state ->
    match c with
    | Constant c ->
        let open Declareops in
        let cb = Environ.lookup_constant c env in
        if is_opaque cb || not(constant_has_body cb) then ()
        else raise Pred.No_clause
    | Variable v ->
        match Environ.lookup_named v env with
        | Context.Named.Declaration.LocalDef _ -> raise Pred.No_clause
        | Context.Named.Declaration.LocalAssum _ -> ())),
  DocAbove);

  MLCode(Pred("coq.env.const",
    In(constant,  "GR",
    Out(option closed_ground_term, "Bo",
    Out(closed_ground_term, "Ty",
    Full (global, "reads the type Ty and the body Bo of constant GR. "^
          "Opaque constants have Bo = none.")))),
  (fun c bo ty ~depth {env} _ state ->
    match c with
    | Constant c ->
        let state, ty = if_keep_acc ty state (fun s -> type_of_global s (GlobRef.ConstRef c)) in
        let state, bo = if_keep_acc bo state (fun state ->
          if Declareops.is_opaque (Environ.lookup_constant c env)
          then state, None
          else
            body_of_constant state c) in
        state, ?: bo +? ty, []
    | Variable v ->
        let state, ty = if_keep_acc ty state (fun s -> type_of_global s (GlobRef.VarRef v)) in
        let bo = if_keep bo (fun () ->
          match Environ.lookup_named v env with
          | Context.Named.Declaration.LocalDef(_,bo,_) -> Some (bo |> EConstr.of_constr)
          | Context.Named.Declaration.LocalAssum _ -> None) in
        state, ?: bo +? ty, [])),
  DocAbove);

  MLCode(Pred("coq.env.const-body",
    In(constant,  "GR",
    Out(option closed_ground_term, "Bo",
    Full (global, "reads the body of a constant, even if it is opaque. "^
          "If such body is none, then the constant is a true axiom"))),
  (fun c _ ~depth {env} _ state ->
    match c with
    | Constant c ->
         let state, bo = body_of_constant state c in
         state, !: bo, []
    | Variable v ->
         state, !: begin
         match Environ.lookup_named v env with
         | Context.Named.Declaration.LocalDef(_,bo,_) -> Some (bo |> EConstr.of_constr)
         | Context.Named.Declaration.LocalAssum _ -> None
         end, [])),
  DocAbove);

  MLCode(Pred("coq.locate-module",
    In(id, "ModName",
    Out(modpath, "ModPath",
    Easy "locates a module.  It's a fatal error if ModName cannot be located. *E*")),
  (fun s _ ~depth ->
    let qualid = Libnames.qualid_of_string s in
    let mp =
      try Nametab.locate_module qualid
      with Not_found ->
        err Pp.(str "Module not found: " ++ Libnames.pr_qualid qualid) in
    !:mp)),
  DocAbove);

  MLCode(Pred("coq.locate-module-type",
    In(id, "ModName",
    Out(modtypath, "ModPath",
    Easy "locates a module.  It's a fatal error if ModName cannot be located. *E*")),
  (fun s _ ~depth ->
    let qualid = Libnames.qualid_of_string s in
    let mp =
      try Nametab.locate_modtype qualid
      with Not_found ->
        err Pp.(str "Module type not found: " ++ Libnames.pr_qualid qualid) in
    !:mp)),
  DocAbove);

  MLCode(Pred("coq.env.module",
    In(modpath, "MP",
    Out(list gref, "Contents",
    Read(global, "lists the contents of a module (recurses on submodules) *E*"))),
  (fun mp _ ~depth {env} _ state ->
    let t = in_elpi_module ~depth state (Environ.lookup_module mp env) in
    !: t)),
  DocAbove);

  MLCode(Pred("coq.env.module-type",
    In(modtypath, "MTP",
    Out(list id,  "Entries",
    Read (global, "lists the items made visible by module type "^
          "(does not recurse on submodules) *E*"))),
  (fun mp _ ~depth {env} _ state ->
    !: (in_elpi_module_type (Environ.lookup_modtype mp env)))),
  DocAbove);

  MLCode(Pred("coq.env.section",
    Out(list constant, "GlobalObjects",
    Read(unit_ctx, "lists the global objects that are marked as to be abstracted at the end of the enclosing sections")),
  (fun _ ~depth _ _ state ->
     let { section } = mk_coq_context ~options:API.Data.StrMap.empty state in
     !: (section |> List.map (fun x -> Variable x)) )),
  DocAbove);

  LPDoc "-- Environment: write -----------------------------------------------";

  LPDoc ("Note: universe constraints are taken from ELPI's constraints "^
         "store. Use coq.univ-* in order to add constraints (or any higher "^
         "level facility as coq.typecheck)");

  MLCode(Pred("coq.env.add-const",
    In(id,   "Name",
    In(unspec closed_ground_term, "Bo",
    In(unspec closed_ground_term, "Ty",
    In(flag "opaque?", "Opaque",
    In(flag "local?", "SectionLocal",
    Out(constant, "C",
    Full (global, "declare a new constant: C gets a constant derived "^
          "from Name and the current module; Ty can be left unspecified "^
          "and in that case the inferred one is taken (as in writing "^
          "Definition x := t); Bo can be left unspecified and in that case "^
          "an axiom is added (or a section variable, if a section is open). "^
          "Omitting the body and the type is an error."))))))),
  (fun id bo ty opaque local _ ~depth env _ -> on_global_state "coq.env.add-const" (fun state ->
    let local = local = Given true in
    let sigma = get_sigma state in
     match bo with
     | Unspec -> (* axiom *)
       begin match ty with
       | Unspec ->
         err Pp.(str "coq.env.add-const: both Type and Body are unspecified")
       | Given ty ->
       let used = EConstr.universes_of_constr sigma ty in
       let sigma = Evd.restrict_universe_context sigma used in
       let ubinders = Evd.universe_binders sigma in
       let uentry = Evd.univ_entry ~poly:false sigma in
       let kind = Decls.Logical in
       let impargs = [] in
       let variable = CAst.(make @@ Id.of_string id) in
       let gr, _ =
         if local then begin
           let uctx =
              let context_set_of_entry = function
                | Entries.Polymorphic_entry (_,uctx) -> Univ.ContextSet.of_context uctx
                | Entries.Monomorphic_entry uctx -> uctx in
              context_set_of_entry uentry in
           Declare.declare_universe_context ~poly:false uctx;
           ComAssumption.declare_variable false ~kind (EConstr.to_constr sigma ty) impargs Glob_term.Explicit variable;
           GlobRef.VarRef(Id.of_string id), Univ.Instance.empty
         end else
           ComAssumption.declare_axiom false ~local:Declare.ImportDefaultBehavior ~poly:false ~kind (EConstr.to_constr sigma ty)
             (uentry, ubinders) impargs Declaremods.NoInline
             variable
       in
       state, !: (global_constant_of_globref gr), []
     end
    | Given bo ->
       let ty =
         match ty with
         | Unspec -> None
         | Given ty -> Some ty in
       let bo, ty = EConstr.(to_constr sigma bo, Option.map (to_constr sigma) ty) in
        let ce =
          let sigma = Evd.minimize_universes sigma in
          let fold uvars c =
            Univ.LSet.union uvars
              (EConstr.universes_of_constr sigma (EConstr.of_constr c))
          in
          let univ_vars =
            List.fold_left fold Univ.LSet.empty (Option.List.cons ty [bo]) in
          let sigma = Evd.restrict_universe_context sigma univ_vars in
          (* Check we conform to declared universes *)
          let uctx =
             Evd.check_univ_decl ~poly:false sigma UState.default_univ_decl in
          Declare.definition_entry
            ~opaque:(opaque = Given true) ?types:ty ~univs:uctx bo in
       let scope = if local then DeclareDef.Discharge else DeclareDef.Global Declare.ImportDefaultBehavior in
       let kind = Decls.(IsDefinition Definition) in
       let gr =
         DeclareDef.declare_definition
           ~name:(Id.of_string id) ~scope ~kind
           UnivNames.empty_binders ce [] in
       state, !: (global_constant_of_globref gr), []))),
  DocAbove);

  MLCode(Pred("coq.env.add-indt",
    In(indt_decl_in, "Decl",
    Out(inductive, "I",
    Full(global, "Declares an inductive type"))),
  (fun (me, record_info, ind_impls) _ ~depth env _ -> on_global_state "coq.env.add-indt" (fun state ->
     let sigma = get_sigma state in
     if not (is_mutual_inductive_entry_ground me sigma) then
       err Pp.(str"coq.env.add-indt: the inductive type declaration must be ground. Did you forge to call coq.typecheck-indt-decl?");
     let mind =
       DeclareInd.declare_mutual_inductive_with_eliminations me UnivNames.empty_binders ind_impls in
     let ind = mind, 0 in
     begin match record_info with
     | None -> () (* regular inductive *)
     | Some field_specs -> (* record: projection... *)
         let names, flags =
           List.(split (map (fun { name; is_coercion; is_canonical } -> name,
               { Record.pf_subclass = is_coercion ; pf_canonical = is_canonical })
             field_specs)) in
         let is_implicit = List.map (fun _ -> []) names in
         let rsp = ind in
         let cstr = (rsp,1) in
         let open Entries in
         let k_ty = List.(hd (hd me.mind_entry_inds).mind_entry_lc) in
         let fields_as_relctx = Term.prod_assum k_ty in
         let kinds, sp_projs =
           Record.declare_projections rsp ~kind:Decls.Definition
             (Evd.univ_entry ~poly:false sigma)
             (Names.Id.of_string "record")
             flags is_implicit fields_as_relctx
         in
         Record.declare_structure_entry
           (cstr, List.rev kinds, List.rev sp_projs);
     end;
     state, !: ind, []))),
  DocAbove);

  LPDoc "Interactive module construction";

  (* XXX When Coq's API allows it, call vernacentries directly *)
  MLCode(Pred("coq.env.begin-module",
    In(id, "Name",
    In(option modtypath, "ModTyPath",
    Full(unit_ctx, "Starts a module, the modtype can be omitted *E*"))),
  (fun name mp ~depth _ _ -> on_global_state "coq.env.begin-module" (fun state ->
     if Global.sections_are_opened () then
       err Pp.(str"This elpi code cannot be run within a section since it opens a module");
     let ty =
       match mp with
       | None -> Declaremods.Check []
       | Some mp ->
           let fpath = Nametab.path_of_modtype mp in
           let tname = Constrexpr.CMident (Libnames.qualid_of_path fpath) in
           Declaremods.(Enforce (CAst.make tname, DefaultInline)) in
     let id = Id.of_string name in
     let _mp = Declaremods.start_module None id [] ty in
     state, (), []))),
  DocAbove);

  (* XXX When Coq's API allows it, call vernacentries directly *)
  MLCode(Pred("coq.env.end-module",
    Out(modpath, "ModPath",
    Full(unit_ctx, "end the current module that becomes known as ModPath *E*")),
  (fun _ ~depth _ _ -> on_global_state "coq.env.end-module" (fun state ->
     let mp = Declaremods.end_module () in
     state, !: mp, []))),
  DocAbove);

  (* XXX When Coq's API allows it, call vernacentries directly *)
  MLCode(Pred("coq.env.begin-module-type",
    In(id, "Name",
    Full(unit_ctx,"Starts a module type *E*")),
  (fun id ~depth _ _ -> on_global_state "coq.env.begin-module-type" (fun state ->
     if Global.sections_are_opened () then
       err Pp.(str"This elpi code cannot be run within a section since it opens a module");
     let id = Id.of_string id in
     let _mp = Declaremods.start_modtype id [] [] in
      state, (), []))),
  DocAbove);

  (* XXX When Coq's API allows it, call vernacentries directly *)
  MLCode(Pred("coq.env.end-module-type",
    Out(modtypath, "ModTyPath",
    Full(unit_ctx, "end the current module type that becomes known as ModPath *E*")),
  (fun _ ~depth _ _ -> on_global_state "coq.env.end-module-type" (fun state ->
     let mp = Declaremods.end_modtype () in
     state, !: mp, []))),
  DocAbove);

  (* XXX When Coq's API allows it, call vernacentries directly *)
  MLCode(Pred("coq.env.include-module",
    In(modpath, "ModPath",
    Full(unit_ctx, "is like the vernacular Include *E*")),
  (fun mp ~depth _ _ -> on_global_state "coq.env.include-module" (fun state ->
     let fpath = match mp with
       | ModPath.MPdot(mp,l) ->
           Libnames.make_path (ModPath.dp mp) (Label.to_id l)
       | _ -> nYI "functors" in
     let tname = Constrexpr.CMident (Libnames.qualid_of_path fpath) in
     let i = CAst.make tname, Declaremods.DefaultInline in
     Declaremods.declare_include [i];
     state, (), []))),
  DocAbove);

  (* XXX When Coq's API allows it, call vernacentries directly *)
  MLCode(Pred("coq.env.include-module-type",
    In(modtypath, "ModTyPath",
    Full(unit_ctx, "is like the vernacular Include *E*")),
  (fun mp ~depth _ _ -> on_global_state "coq.env.include-module-type" (fun state ->
     let fpath = Nametab.path_of_modtype mp in
     let tname = Constrexpr.CMident (Libnames.qualid_of_path fpath) in
     let i = CAst.make tname, Declaremods.DefaultInline in
     Declaremods.declare_include [i];
     state, (), []))),
  DocAbove);

  MLCode(Pred("coq.env.import-module",
    In(modpath, "ModPath",
    Full(unit_ctx, "is like the vernacular Import *E*")),
  (fun mp ~depth _ _ -> on_global_state "coq.env.import-module" (fun state ->
     Declaremods.import_module ~export:false mp;
     state, (), []))),
  DocAbove);

  MLCode(Pred("coq.env.export-module",
    In(modpath, "ModPath",
    Full(unit_ctx, "is like the vernacular Export *E*")),
  (fun mp ~depth _ _ -> on_global_state "coq.env.export-module" (fun state ->
     Declaremods.import_module ~export:true mp;
     state, (), []))),
  DocAbove);

  LPDoc
{|Support for sections is limited, in particular sections and
Coq quotations may interact in surprising ways. For example
  Section Test.
  Variable x : nat.
  Elpi Query lp:{{ coq.say {{ x }} }}.
works since x is a global Coq term while
  Elpi Query lp:{{
    coq.env.begin-section "Test",
    coq.env.add-const "x" _ {{ nat }} _ @local! GRX,
    coq.say {{ x }}
  }}.
may work in a surprising way or may not work at all since
x is resolved before the section is started hence it cannot
denote the same x as before.|};

  MLCode(Pred("coq.env.begin-section",
    In(id, "Name",
    Full(unit_ctx, "starts a section named Name *E*")),
  (fun id ~depth _ _ -> on_global_state "coq.env.begin-section" (fun state ->
     Lib.open_section (Names.Id.of_string id);
     state, (), []))),
  DocAbove);

  MLCode(Pred("coq.env.end-section",
    Full(unit_ctx, "end the current section *E*"),
  (fun ~depth _ _ -> on_global_state_does_rewind_env "coq.env.end-section" (fun state ->
     Lib.close_section ();
     state, (), []))),
  DocAbove);


  LPDoc "-- Universes --------------------------------------------------------";

  MLData univ;

  MLData universe;

  MLCode(Pred("coq.univ.print",
    Read(unit_ctx,  "prints the set of universe constraints"),
  (fun ~depth _ _ state ->
    let sigma = get_sigma state in
    let uc = Evd.evar_universe_context sigma in
    let uc = Termops.pr_evar_universe_context uc in
    Feedback.msg_notice Pp.(str "Universe constraints: " ++ uc);
    ())),
  DocAbove);

  MLCode(Pred("coq.univ.leq",
    In(univ, "U1",
    In(univ, "U2",
    Full(unit_ctx, "constrains U1 <= U2"))),
  (fun u1 u2 ~depth _ _ state ->
    add_universe_constraint state (constraint_leq u1 u2), (),[])),
  DocAbove);

  MLCode(Pred("coq.univ.eq",
    In(univ, "U1",
    In(univ, "U2",
    Full(unit_ctx, "constrains U1 = U2"))),
  (fun u1 u2 ~depth _ _ state ->
    add_universe_constraint state (constraint_eq u1 u2),(), [])),
  DocAbove);

  MLCode(Pred("coq.univ.new",
    In(unspec (list id), "Names",
    Out(univ, "U",
    Full(unit_ctx, "fresh universe *E*"))),
  (fun nl _ ~depth _ _ state ->
     if not (nl = Unspec || nl = Given []) then nYI "named universes";
     let state, u = mk_fresh_univ state in
     state, !: u, [])),
  DocAbove);

  MLCode(Pred("coq.univ.sup",
    In(univ, "U1",
    In(univ, "U2",
    Full(unit_ctx,  "constrains U2 = U1 + 1"))),
  (fun u1 u2 ~depth _ _ state ->
    univ_super state u1 u2, (), [])),
  DocAbove);

  MLCode(Pred("coq.univ.max",
    In(univ, "U1",
    In(univ, "U2",
    Out(univ, "U3",
    Full(unit_ctx,  "constrains U3 = max U1 U2")))),
  (fun u1 u2 _ ~depth _ _ state ->
    let state, u3 = univ_max state u1 u2 in
    state, !: u3, [])),
  DocAbove);

  LPDoc "Very low level, don't use";

  MLCode(Pred("coq.univ.algebraic-max",
    In(univ, "U1",
    In(univ, "U2",
    Out(univ, "U3",
    Full(unit_ctx,  "constrains U3 = Max(U1,U2) *E*")))),
  (fun u1 u2 _ ~depth _ _ state ->
    state, !: (mk_algebraic_max u1 u2), [])),
  DocAbove);

  MLCode(Pred("coq.univ.algebraic-sup",
    In(univ, "U1",
    Out(univ, "U2",
    Full(unit_ctx,  "constrains U2 = Sup(U1) *E*"))),
  (fun u1 _ ~depth _ _ state ->
    state, !: (mk_algebraic_super u1), [])),
  DocAbove);

  LPCode {|
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API for extra logical objects
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|};

  LPDoc "-- Databases (TC, CS, Coercions) ------------------------------------";

  MLData cs_pattern;
  MLData cs_instance;

  MLCode(Pred("coq.CS.declare-instance",
    In(gref, "GR",
    Full(unit_ctx, "declares GR as a canonical structure instance")),
  (fun gr ~depth _ _ -> on_global_state "coq.CS.declare-instance" (fun state ->
     Canonical.declare_canonical_structure gr;
    state, (), []))),
  DocAbove);

  MLCode(Pred("coq.CS.db",
    Out(list cs_instance, "Db",
    Easy "reads all instances"),
  (fun _ ~depth ->
     !: (Recordops.canonical_projections ()))),
  DocAbove);

    MLCode(Pred("coq.CS.canonical-projections",
    In(inductive, "StructureName",
    Out(list (option constant), "Projections",
    Easy "given a record StructureName lists all projections")),
  (fun i _ ~depth ->
    !: (Recordops.lookup_projections i |>
        CList.map (Option.map (fun x -> Constant x))))),
  DocAbove);

  MLData tc_instance;

  MLCode(Pred("coq.TC.declare-instance",
    In(gref, "GR",
    In(int,  "Priority",
    In(flag "global?", "Global",
    Full(unit_ctx, "declare GR as a Global type class instance with Priority")))),
  (fun gr priority global ~depth _ _ -> on_global_state "coq.TC.declare-instance" (fun state ->
     let global = global = Given true in
     let hint_priority = Some priority in
     let qualid =
       Nametab.shortest_qualid_of_global Names.Id.Set.empty gr in
     Classes.existing_instance global qualid
          (Some { Hints.empty_hint_info with Typeclasses.hint_priority });
     state, (), []))),
  DocAbove);

  MLCode(Pred("coq.TC.db",
    Out(list tc_instance, "Db",
    Easy "reads all instances"),
  (fun _ ~depth -> !: (Typeclasses.all_instances ()))),
  DocAbove);

  MLCode(Pred("coq.TC.db-for",
    In(gref, "GR",
    Out(list tc_instance, "Db",
    Read(unit_ctx,"reads all instances of the given class GR"))),
  (fun gr _ ~depth _ _ state ->
    let env, evd = get_global_env_sigma state in
    !: (Typeclasses.instances env evd gr))),
  DocAbove);

  MLCode(Pred("coq.TC.class?",
    In(gref, "GR",
    Easy "checks if GR is a class"),
  (fun gr ~depth ->
     if Typeclasses.is_class gr then () else raise Pred.No_clause)),
  DocAbove);

  MLData class_;
  MLData coercion;

  MLCode(Pred("coq.coercion.declare",
    In(coercion, "C",
    In(flag "global?", "Global",
    Full (unit_ctx,"declares C = (coercion GR _ From To) as a coercion From >-> To. "))),
  (fun (gr, _, source, target) global ~depth _ _ -> on_global_state "coq.coercion.declare" (fun state ->
     let local = not (global = Given true) in
     let poly = false in
     let source = Class.class_of_global source in

     Class.try_add_new_coercion_with_target gr ~local ~poly ~source ~target;
     state, (), []))),
  DocAbove);

  MLCode(Pred("coq.coercion.db",
    Out(list coercion, "L",
    Easy ("reads all declared coercions")),
  (fun _ ~depth ->
    (* TODO: fix API in Coq *)
     let pats = Classops.inheritance_graph () in
     let coercions = pats |> CList.map_filter (function
       | (source,target),[c] ->
           Some(c.Classops.coe_value,
                Given c.Classops.coe_param,
                src_class_of_class @@ fst (Classops.class_info_from_index source),
                fst (Classops.class_info_from_index target))
       | _ -> None) in
     !: coercions)),
  DocAbove);

  MLCode(Pred("coq.coercion.db-for",
    In(class_,"From",
    In(class_,"To",
    Out(list (pair gref int), "L",
    Easy ("reads all declared coercions")))),
  (fun source target _ ~depth ->
    let source,_ = Classops.class_info source in
    let target,_ = Classops.class_info target in
    let path = Classops.lookup_path_between_class (source,target) in
    let coercions = path |> List.map (fun c ->
     c.Classops.coe_value, c.Classops.coe_param) in
   !: coercions)),
  DocAbove);

  LPDoc "-- Coq's notational mechanisms -------------------------------------";

  MLData implicit_kind;

  MLCode(Pred("coq.arguments.implicit",
    In(gref,"GR",
    Out(list (list implicit_kind),"Imps",
    Easy "reads the implicit arguments declarations associated to a global reference. See also the [] and {} flags for the Arguments command.")),
  (fun gref _ ~depth ->
    !: (List.map (fun (_,x) -> List.map implicit_kind_of_status x)
          (Impargs.extract_impargs_data (Impargs.implicits_of_global gref))))),
  DocAbove);

  MLCode(Pred("coq.arguments.set-implicit",
    In(gref,"GR",
    In(list (list (unspec implicit_kind)),"Imps",
    In(flag "global?", "Global",
    Full(unit_ctx,
{|sets the implicit arguments declarations associated to a global reference.
Unspecified means explicit.
See also the [] and {} flags for the Arguments command.|})))),
  (fun gref imps global ~depth _ _ -> on_global_state "coq.arguments.set-implicit" (fun state ->
     let local = not (global = Given true) in
     let imps = imps |> List.(map (map (function
       | Unspec -> Impargs.NotImplicit
       | Given x -> x))) in
     Impargs.set_implicits local gref imps;
     state, (), []))),
  DocAbove);

  MLCode(Pred("coq.arguments.set-default-implicit",
    In(gref,"GR",
    In(flag "global?", "Global",
    Full(unit_ctx,
{|sets the default implicit arguments declarations associated to a global reference.
See also the "default implicits" flag to the Arguments command.|}))),
  (fun gref global ~depth _ _ -> on_global_state "coq.arguments.set-default-implicit" (fun state ->
     let local = not (global = Given true) in
     Impargs.declare_implicits local gref;
     state, (), []))),
  DocAbove);

  MLCode(Pred("coq.arguments.name",
    In(gref,"GR",
    Out(list (option id),"Names",
    Easy "reads the Names of the arguments of a global reference. See also the (f (A := v)) syntax.")),
  (fun gref _ ~depth ->
    let open Name in
    !: (try Arguments_renaming.arguments_names gref
            |> List.map (function Name x -> Some (Id.to_string x) | _ -> None)
        with Not_found -> []))),
  DocAbove);

  MLCode(Pred("coq.arguments.set-name",
    In(gref,"GR",
    In(list (option id),"Names",
    In(flag "global?", "Global",
    Full(unit_ctx,
{|sets the Names of the arguments of a global reference.
See also the :rename flag to the Arguments command.|})))),
  (fun gref names global ~depth _ _ -> on_global_state "coq.arguments.set-name" (fun state ->
     let local = not (global = Given true) in
     let names = names |> List.map (function
       | None -> Names.Name.Anonymous
       | Some x -> Names.(Name.Name (Id.of_string x))) in
     Arguments_renaming.rename_arguments local gref names;
     state, (), []))),
  DocAbove);

  MLCode(Pred("coq.arguments.scope",
    In(gref,"GR",
    Out(list (option id),"Scopes",
    Easy "reads the notation scope of the arguments of a global reference. See also the %scope modifier for the Arguments command")),
  (fun gref _ ~depth -> !: (CNotation.find_arguments_scope gref))),
  DocAbove);

  MLCode(Pred("coq.arguments.set-scope",
    In(gref,"GR",
    In(list (option id),"Scopes",
    In(flag "global?", "Global",
    Full(unit_ctx,
{|sets the notation scope of the arguments of a global reference.
Scope can be a scope name or its delimiter.
See also the %scope modifier for the Arguments command.|})))),
  (fun gref scopes global ~depth _ _ -> on_global_state "coq.arguments.set-scope" (fun state ->
     let local = not (global = Given true) in
     let scopes = scopes |> List.map (Option.map (fun k ->
        try ignore (CNotation.find_scope k); k
        with CErrors.UserError _ -> CNotation.find_delimiters_scope k)) in
     CNotation.declare_arguments_scope local gref scopes;
     state, (), []))),
  DocAbove);

  MLData simplification_strategy;

  MLCode(Pred("coq.arguments.simplification",
    In(gref,"GR",
    Out(option simplification_strategy,"Strategy",
    Easy "reads the behavior of the simplification tactics. Positions are 0 based. See also the ! and / modifiers for the Arguments command")),
  (fun gref _ ~depth ->
     !: (Reductionops.ReductionBehaviour.get gref))),
  DocAbove);

  MLCode(Pred("coq.arguments.set-simplification",
    In(gref,"GR",
    In(simplification_strategy, "Strategy",
    In(flag "global?", "Global",
    Full(unit_ctx,
{|sets the behavior of the simplification tactics.
Positions are 0 based.
See also the ! and / modifiers for the Arguments command.|})))),
  (fun gref strategy global ~depth _ _ -> on_global_state "coq.arguments.set-simplification" (fun state ->
     let local = not (global = Given true) in
     Reductionops.ReductionBehaviour.set ~local gref strategy;
     state, (), []))),
  DocAbove);

  MLCode(Pred("coq.locate-abbreviation",
    In(id, "Name",
    Out(abbreviation, "Abbreviation",
    Easy "locates an abbreviation.  It's a fatal error if Name cannot be located.")),
  (fun s _ ~depth ->
    let qualid = Libnames.qualid_of_string s in
    let sd =
      try Nametab.locate_syndef qualid
      with Not_found -> err Pp.(str "Abbreviation not found: " ++ Libnames.pr_qualid qualid) in
    !:sd)),
  DocAbove);

  MLData abbreviation;

  MLCode(Pred("coq.notation.add-abbreviation",
    In(id,"Name",
    In(int,"Nargs",
    CIn(closed_term,"Body",
    In(flag "global?", "Global",
    In(flag "bool","OnlyParsing",
    In(unspec (B.pair B.string B.string),"Deprecation",
    Out(abbreviation, "Abbreviation",
    Full(global, {|
Declares an abbreviation Name with Nargs arguments.
The term must begin with at least Nargs lambdas.
Deprecation can be (pr "version" "note")|})))))))),
  (fun name nargs term global onlyparsing deprecated _ ~depth {env} _ -> on_global_state "coq.notation.add-abbreviation" (fun state ->
       let sigma = get_sigma state in
       let strip_n_lambas nargs env term =
       let rec aux vars nenv env n t =
         if n = 0 then List.rev vars, nenv, env, t
         else match EConstr.kind sigma t with
         | Constr.Lambda(name,ty,t) ->
             let name, id =
                match name with
               | { Context.binder_name = Names.Name.Name id; _ } -> name, id
               | _ ->
                 let id = Id.of_string_soft (Printf.sprintf "_elpi_ctx_entry_%d_" n) in
                 { name with Context.binder_name = Names.Name.Name id }, id
             in
             let nenv, vars =
               { nenv with Notation_term.ninterp_var_type =
                   Id.Map.add id Notation_term.NtnInternTypeAny
                     nenv.Notation_term.ninterp_var_type },
               (id, (None,[])) :: vars in
             let env = EConstr.push_rel (Context.Rel.Declaration.LocalAssum(name,ty)) env in
             aux vars nenv env (n-1) t
         | _ ->
             API.Utils.type_error
               (Printf.sprintf "coq.notation.add-abbreviation: term with %d more lambdas expected" n)
         in
         let vars = [] in
         let nenv =
           {
              Notation_term.ninterp_var_type = Id.Map.empty;
              ninterp_rec_vars = Id.Map.empty;
           } in
         aux vars nenv env nargs term
     in
     let local = not (global = Given true) in
     let onlyparsing = (onlyparsing = Given true) in
     let deprecated =
       match deprecated with
       | Unspec -> None
       | Given (since,note) ->
           let since = if since <> "" then Some since else None in
           let note = if note <> "" then Some note else None in
           Some { Deprecation.since; note } in
     let name = Id.of_string name in
     let vars, nenv, env, body = strip_n_lambas nargs env term in
     let gbody = detype env sigma body in
     let gbody =
       let rec aux x = match DAst.get x with
         | Glob_term.GEvar _ -> Coq_elpi_utils.mkGHole
         | _ -> Glob_ops.map_glob_constr aux x in
       aux gbody in
     let pat, _ = Notation_ops.notation_constr_of_glob_constr nenv gbody in
     Syntax_def.declare_syntactic_definition ~local ~onlyparsing deprecated name (vars,pat);
     let qname = Libnames.qualid_of_string (Id.to_string name) in
     match Nametab.locate_extended qname with
     | Globnames.TrueGlobal _ -> assert false
     | Globnames.SynDef sd -> state, !: sd, []))),
  DocAbove);

  MLCode(Pred("coq.notation.abbreviation",
    In(abbreviation,"Abbreviation",
    In(B.list (B.poly "term"),"Args",
    Out(B.poly "term","Body",
    Full(unit_ctx, "Unfolds an abbreviation")))),
  (fun sd arglist _ ~depth proof_context _ state ->
    let args, _ = Syntax_def.search_syntactic_definition sd in
    let nargs = List.length args in
    let open Constrexpr in
    let binders, vars = List.split (CList.init nargs (fun i ->
      let name = Coq_elpi_glob_quotation.mk_restricted_name i in
      let lname = CAst.make @@ Name.Name (Id.of_string name) in
      CLocalAssum([lname],Default Glob_term.Explicit, CAst.make @@ CHole(None,Namegen.IntroAnonymous,None)),
      (CAst.make @@ CRef(Libnames.qualid_of_string name,None), None))) in
    let eta = CAst.(make @@ CLambdaN(binders,make @@ CApp((None,make @@ CRef(Libnames.qualid_of_string (KerName.to_string sd),None)),vars))) in
    let env, sigma = get_global_env_sigma state in
    let geta = Constrintern.intern_constr env sigma eta in
    let state, teta = Coq_elpi_glob_quotation.gterm2lp ~depth state geta in
    let t =
      let rec aux ~depth n t =
        if n = 0 then t
        else match Coq_elpi_HOAS.is_lam ~depth t with
        | Some(_,bo) -> E.mkLam (aux ~depth:(depth+1) (n-1) bo)
        | None -> CErrors.anomaly Pp.(str"coq.notation.abbreviation")
      in
        aux ~depth nargs teta in
    state, !: (API.Utils.beta ~depth t arglist), []
  )),
  DocAbove);

  MLCode(Pred("coq.notation.abbreviation-body",
    In(abbreviation,"Abbreviation",
    Out(B.int,"Nargs",
    Out(B.poly "term","Body",
    Full(global, "Retrieves the body of an abbreviation")))),
  (fun sd _ _ ~depth proof_context _ state ->
    let args, _ = Syntax_def.search_syntactic_definition sd in
    let nargs = List.length args in
    let open Constrexpr in
    let binders, vars = List.split (CList.init nargs (fun i ->
      let name = Coq_elpi_glob_quotation.mk_restricted_name i in
      let lname = CAst.make @@ Name.Name (Id.of_string name) in
      CLocalAssum([lname],Default Glob_term.Explicit, CAst.make @@ CHole(None,Namegen.IntroAnonymous,None)),
      (CAst.make @@ CRef(Libnames.qualid_of_string name,None), None))) in
    let eta = CAst.(make @@ CLambdaN(binders,make @@ CApp((None,make @@ CRef(Libnames.qualid_of_string (KerName.to_string sd),None)),vars))) in
    let env, sigma = get_global_env_sigma state in
    let geta = Constrintern.intern_constr env sigma eta in
    let state, teta = Coq_elpi_glob_quotation.gterm2lp ~depth state geta in
    state, !: nargs +! teta, []
  )),
  DocAbove);

  MLData attribute_value;
  MLData attribute;

  LPDoc "-- Coq's pretyper ---------------------------------------------------";

  MLCode(Pred("coq.sigma.print",
    Read(raw_ctx, "Prints Coq's Evarmap and the mapping to/from Elpi's unification variables"),
    (fun ~depth hyps constraints state ->
      let state, _, _, _ = get_current_env_sigma ~depth hyps constraints state in
      Feedback.msg_notice Pp.(
        str (Format.asprintf "%a" API.RawPp.constraints constraints) ++ spc () ++
        str (show_engine state));
      ())),
  DocAbove);

  MLCode(Pred("coq.typecheck",
    CIn(term,  "T",
    CInOut(B.ioargC term, "Ty",
    InOut(B.ioarg B.diagnostic, "Diagnostic",
    Full (proof_context,
{|typchecks a term T returning its type Ty. If Ty is provided, then
the inferred type is unified (see unify-leq) with it.
Universe constraints are put in the constraint store.|})))),
  (fun t ety diag ~depth proof_context _ state ->
     try
       let sigma = get_sigma state in
       let sigma, ty = Typing.type_of proof_context.env sigma t in
       match ety with
       | Data ety ->
           let sigma = Evarconv.unify proof_context.env sigma ~with_ho:true Reduction.CUMUL ty ety in
           let state, assignments = set_current_sigma ~depth state sigma in
           state, !: ety +! B.mkOK, assignments
       | NoData ->
           let flags = Evarconv.default_flags_of TransparentState.full in
           let sigma = Evarconv.solve_unif_constraints_with_heuristics ~flags ~with_ho:true proof_context.env sigma in
           let state, assignments = set_current_sigma ~depth state sigma in
           state, !: ty +! B.mkOK, assignments
     with Pretype_errors.PretypeError (env, sigma, err) ->
       match diag with
       | Data B.OK ->
          (* optimization: don't print the error if caller wants OK *)
          raise No_clause
       | _ ->
          let error = Pp.string_of_ppcmds @@ Himsg.explain_pretype_error env sigma err in
          state, ?: None +! B.mkERROR error, [])),
  DocAbove);

  MLCode(Pred("coq.typecheck-ty",
    CIn(term,  "Ty",
    InOut(B.ioarg universe, "U",
    InOut(B.ioarg B.diagnostic, "Diagnostic",
    Full (proof_context,
{|typchecks a type Ty returning its universe U. If U is provided, then
the inferred universe is unified (see unify-leq) with it.
Universe constraints are put in the constraint store.|})))),
  (fun ty es diag ~depth proof_context _ state ->
     try
       let sigma = get_sigma state in
       let sigma, s = Typing.sort_of proof_context.env sigma ty in
       match es with
       | Data es ->
           let sigma = Evarconv.unify proof_context.env sigma ~with_ho:true Reduction.CUMUL (EConstr.mkSort s) (EConstr.mkSort es) in
           let state, assignments = set_current_sigma ~depth state sigma in
           state, !: es +! B.mkOK, assignments
       | NoData ->
           let flags = Evarconv.default_flags_of TransparentState.full in
           let sigma = Evarconv.solve_unif_constraints_with_heuristics ~flags ~with_ho:true proof_context.env sigma in
           let state, assignments = set_current_sigma ~depth state sigma in
           state, !: s +! B.mkOK, assignments
     with Pretype_errors.PretypeError (env, sigma, err) ->
       match diag with
       | Data B.OK ->
          (* optimization: don't print the error if caller wants OK *)
          raise No_clause
       | _ ->
          let error = Pp.string_of_ppcmds @@ Himsg.explain_pretype_error env sigma err in
          state, ?: None +! B.mkERROR error, [])),
  DocAbove);

  MLCode(Pred("coq.unify-eq",
    CIn(term, "A",
    CIn(term, "B",
    InOut(B.ioarg B.diagnostic, "Diagnostic",
    Full (proof_context, "unifies the two terms")))),
  (fun a b diag ~depth proof_context _ state ->
     let sigma = get_sigma state in
     try
       let sigma = Evarconv.unify proof_context.env sigma ~with_ho:true Reduction.CONV a b in
       let state, assignments = set_current_sigma ~depth state sigma in
       state, !: B.mkOK, assignments
     with Pretype_errors.PretypeError (env, sigma, err) ->
       match diag with
       | Data B.OK ->
          (* optimization: don't print the error if caller wants OK *)
          raise No_clause
       | _ ->
          let error = Pp.string_of_ppcmds @@ Himsg.explain_pretype_error env sigma err in
          state, !: (B.mkERROR error), [])),
  DocAbove);

  MLCode(Pred("coq.unify-leq",
    CIn(term, "A",
    CIn(term, "B",
    InOut(B.ioarg B.diagnostic, "Diagnostic",
    Full (proof_context, "unifies the two terms (with cumulativity, if they are types)")))),
  (fun a b diag ~depth proof_context _ state ->
     let sigma = get_sigma state in
     try
       let sigma = Evarconv.unify proof_context.env sigma ~with_ho:true Reduction.CUMUL a b in
       let state, assignments = set_current_sigma ~depth state sigma in
       state, !: B.mkOK, assignments
     with Pretype_errors.PretypeError (env, sigma, err) ->
       match diag with
       | Data B.OK ->
          (* optimization: don't print the error if caller wants OK *)
          raise No_clause
       | _ ->
          let error = Pp.string_of_ppcmds @@ Himsg.explain_pretype_error env sigma err in
          state, !: (B.mkERROR error), [])),
  DocAbove);


  LPDoc "-- Coq's tactics --------------------------------------------";

  MLCode(Pred("coq.ltac1.call",
    In(B.string, "Tac",
    CIn(!>> list term,  "Args",
    In(raw_goal, "G",
    Out(list raw_goal,"GL",
    Full(proof_context, "Calls Ltac1 tactic named Tac with arguments Args on goal G"))))),
    (fun tac_name tac_args goal _ ~depth proof_context constraints state ->
       let sigma = get_sigma state in
       let tactic =
         let open Ltac_plugin in
         let tac_name =
           let q = Libnames.qualid_of_string tac_name in
           try Tacenv.locate_tactic q
           with Not_found ->
             match Tacenv.locate_extended_all_tactic q with
             | [x] -> x
             | _::_::_ -> err Pp.(str"Ltac1 tactic " ++ str tac_name ++ str" is ambiguous, qualify the name")
             | [] -> err Pp.(str"Ltac1 tactic " ++ str tac_name ++ str" not found") in
         let tacref = Locus.ArgArg (Loc.tag @@ tac_name) in
         let tacexpr = Tacexpr.(TacArg (CAst.make @@ TacCall (CAst.make @@ (tacref, [])))) in
         let tac = Tacinterp.Value.of_closure (Tacinterp.default_ist ()) tacexpr in
         let args = List.map Tacinterp.Value.of_constr tac_args in
         Tacinterp.Value.apply tac args in
       let goal =
         match get_goal_ref ~depth constraints state goal with
         | None -> raise Conv.(TypeErr (TyName"goal",depth,goal))
         | Some k -> k in
       let subgoals, sigma =
         let open Proofview in let open Notations in
         let focused_tac =
           Unsafe.tclSETGOALS [with_empty_state goal] <*> tactic in
         let _, pv = init sigma [] in
         let (), pv, _, _ =
           try
             apply ~name:(Id.of_string "elpi") ~poly:false proof_context.env focused_tac pv
           with e when CErrors.noncritical e -> raise Pred.No_clause
         in
           proofview pv in
       let state, assignments = set_current_sigma ~depth state sigma in
       let state, subgoals, gls2 =
         API.Utils.map_acc (embed_goal ~depth) state subgoals in
       state, !: subgoals, assignments @ gls2
      )),
  DocAbove);

  LPDoc "-- Datatypes conversions --------------------------------------------";

  MLData name;

  MLCode(Pred("coq.name-suffix",
    In(name, "Name",
    In(B.any,  "Suffix",
    Out(name,"NameSuffix",
    Easy "suffixes a Name with a string or an int or another name"))),
  (fun n suffix _ ~depth ->
     match E.look ~depth suffix with
     | E.CData i when API.RawOpaqueData.(is_string i || is_int i || isname i) ->
         let s = Pp.string_of_ppcmds (Name.print n) in
         let suffix =
           if API.RawOpaqueData.is_string i then API.RawOpaqueData.to_string i
           else if API.RawOpaqueData.is_int i then string_of_int (API.RawOpaqueData.to_int i)
           else Pp.string_of_ppcmds (Name.print (nameout i)) in
         let s = s ^ suffix in
         !: (Name.mk_name (Id.of_string s))
     | _ -> err Pp.(str "coq.name-suffix: suffix is not int, nor string nor name"))),
  DocAbove);

  MLCode(Pred("coq.string->name",
    In(B.string, "Hint",
    Out(name,  "Name",
    Easy "creates a name hint")),
  (fun s _ ~depth -> !: (Name.mk_name (Id.of_string s)))),
  DocAbove);

  LPCode {|
pred coq.id->name i:id, o:name.
coq.id->name S N :- coq.string->name S N.
  |};

  MLCode(Pred("coq.name->id",
    In(name, "Name",
    Out(id,  "Id",
    Easy "tuns a pretty printing hint into a string. This API is for internal use, no guarantee on its behavior.")),
  (fun n _ ~depth ->
     match n with
     | Name.Anonymous -> !: "_"
     | Name.Name s -> !: (Id.to_string s))),
  DocAbove);

  MLCode(Pred("coq.gref->id",
    In(gref, "GR",
    Out(id, "Id",
    Read (unit_ctx, "extracts the label (last component of a full kernel name)"))),
  (fun gr _ ~depth _ _ state ->
    let open GlobRef in
    match gr with
    | VarRef v ->
        !: (Id.to_string v)
    | ConstRef c ->
        !: (Id.to_string (Label.to_id (Constant.label c)))
    | IndRef (i,0) ->
        let open Declarations in
        let env, sigma = get_global_env_sigma state in
        let { mind_packets } = Environ.lookup_mind i env in
        !: (Id.to_string (mind_packets.(0).mind_typename))
    | ConstructRef ((i,0),j) ->
        let open Declarations in
        let env, sigma = get_global_env_sigma state in
        let { mind_packets } = Environ.lookup_mind i env in
        !: (Id.to_string (mind_packets.(0).mind_consnames.(j-1)))
    | IndRef _  | ConstructRef _ ->
          nYI "mutual inductive (make-derived...)")),
   DocAbove);

  MLCode(Pred("coq.gref->string",
    In(gref, "GR",
    Out(B.string, "FullPath",
    Read(unit_ctx, "extract the full kernel name"))),
  (fun gr _ ~depth h c state ->
    let path = gr2path state gr in
    !: (String.concat "." path))),
  DocAbove);

  MLCode(Pred("coq.gref->path",
    In(gref, "GR",
    Out(B.list B.string, "FullPath",
    Read(unit_ctx, "extract the full kernel name, each component is a separate list item"))),
  (fun gr _ ~depth h c state -> !: (gr2path state gr))),
  DocAbove);

  MLCode(Pred("coq.term->string",
    CIn(term,"T",
    Out(B.string, "S",
    Full(proof_context, "prints a term T to a string S using Coq's pretty printer"))),
  (fun t _ ~depth proof_context constraints state ->
     let sigma = get_sigma state in
     let s = Pp.string_of_ppcmds (Printer.pr_econstr_env proof_context.env sigma t) in
     state, !: s, [])),
  DocAbove);

  LPDoc "-- Access to Elpi's data --------------------------------------------";

   (* Self modification *)
  MLData clause;
  MLData grafting;
  MLData scope;

  MLCode(Pred("coq.elpi.accumulate",
    In(unspec scope, "Scope",
    In(id, "DbName",
    In(clause, "Clause",
    Full (unit_ctx, {|
Declare that, once the program is over, the given clause has to be added to
the given db (see Elpi Db). Clauses belong to Coq modules: the Scope argument
lets one select which module (default is execution-site).|} )))),
  (fun scope dbname (name,graft,clause) ~depth _ _ state ->
     let loc = API.Ast.Loc.initial "(elpi.add_clause)" in
     let dbname = Coq_elpi_utils.string_split_on_char '.' dbname in
     warn_if_contains_univ_levels ~depth clause;
     let clause = API.Utils.clause_of_term ?name ?graft ~depth loc clause in
     match scope with
     | Unspec | Given ExecutionSite ->
         State.update clauses_for_later state (fun l ->
           (dbname,clause) :: l), (), []
     | Given CurrentModule ->
          let elpi, f = get_accumulate_to_db () in
          f dbname API.(Compile.unit ~elpi:(elpi ()) ~flags:Compile.default_flags clause);
          state, (), []
     )),
  DocAbove);

  LPDoc "-- Utils ------------------------------------------------------------";
  ] @
  B.ocaml_set ~name:"coq.gref.set" gref (module GRSet) @
  B.ocaml_map ~name:"coq.gref.map" gref (module GRMap)

;;
