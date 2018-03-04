open Migrate_parsetree
open OCaml_404.Ast
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

open Ast_convenience_404

(** {2 Convenient stuff} *)

let with_loc f {txt ; loc = _loc} =
  (f txt) [@metaloc _loc]

let def_loc txt =
  { txt; loc = !default_loc }

(** Test if a case is a catchall. *)
let is_catchall case =
  let rec is_catchall_pat p = match p.ppat_desc with
    | Ppat_any | Ppat_var _ -> true
    | Ppat_alias (p, _) -> is_catchall_pat p
    | _ -> false
  in
  case.pc_guard = None && is_catchall_pat case.pc_lhs

(** Add a wildcard case in there is none. Useful for exception handlers. *)
let add_wildcard_case cases =
  let has_wildcard =
    List.exists is_catchall cases
  in
  if not has_wildcard
  then cases @ [Exp.case [%pat? exn] [%expr Lwt.fail exn]] [@metaloc Location.none]
  else cases

(** {3 Internal names} *)

let lwt_prefix = "__ppx_lwt_"

(** {2 Here we go!} *)

let warn_let_lwt_rec loc attrs =
  let attr = attribute_of_warning loc "\"let%lwt rec\" is not a recursive Lwt binding" in
  attr :: attrs

let debug      = ref true
let log        = ref false
let sequence   = ref true
let strict_seq = ref true

let used_no_debug_option = ref false
let used_log_option = ref false
let used_no_log_option = ref false
let used_no_sequence_option = ref false
let used_no_strict_sequence_option = ref false

let no_debug_option () =
  debug := false;
  used_no_debug_option := true

let log_option () =
  log := true;
  used_log_option := true

let no_log_option () =
  log := false;
  used_no_log_option := true

let no_sequence_option () =
  sequence := false;
  used_no_sequence_option := true

let no_strict_sequence_option () =
  strict_seq := false;
  used_no_strict_sequence_option := true

(** let%lwt related functions *)

let gen_name i = lwt_prefix ^ string_of_int i

(** [p = x] ≡ [__ppx_lwt_$i = x] *)
let gen_bindings l =
  let aux i binding =
    { binding with
      pvb_pat = pvar ~loc:binding.pvb_expr.pexp_loc (gen_name i)
    }
  in
  List.mapi aux l

(** [p = x] and e ≡ [Lwt.bind __ppx_lwt_$i (fun p -> e)] *)
let gen_binds e_loc l e =
  let rec aux i bindings =
    match bindings with
    | [] -> e
    | binding :: t ->
      let name = (* __ppx_lwt_$i, at the position of $x$ *)
        evar ~loc:binding.pvb_expr.pexp_loc (gen_name i)
      in
      let fun_ =
        [%expr (fun [%p binding.pvb_pat] -> [%e aux (i+1) t])] [@metaloc e_loc]
      in
      let new_exp =
        if !debug then
          [%expr
            let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
            Lwt.backtrace_bind
              (fun exn -> try Reraise.reraise exn with exn -> exn)
              [%e name]
              [%e fun_]
          ] [@metaloc e_loc]
        else
          [%expr Lwt.bind [%e name] [%e fun_]] [@metaloc e_loc]
      in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in aux 0 l

(** [p = x and p' = x' and ...] ≡
    [p, p', ... = Lwt_main.run (
      Lwt.bind x  (fun __ppx_lwt_$i  ->
      Lwt.bind x' (fun __ppx_lwt_$i' ->
      ...
      Lwt.return (__ppx_lwt_$i, __ppx_lwt_$i', ...))))] *)

let gen_top_binds vbs =
  let gen_exp vbs i =
    match vbs with
    | {pvb_expr; _}::_rest ->
      if !debug then
        [%expr
          let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
          Lwt.backtrace_bind
            (fun exn -> try Reraise.reraise exn with exn -> exn)
            [%e pvb_expr]
            (fun [%p pvar (gen_name i)] -> gen_exp _rest (i + 1))
        ]
      else
        [%expr Lwt.bind [%e pvb_expr] (fun [%p pvar (gen_name i)] -> gen_exp rest (i + 1))]
    | [] ->
      let rec names i =
        if i >= 0 then evar (gen_name i) :: names (i - 1) else []
      in Exp.tuple (names i)
  in
  [Vb.mk (Pat.tuple (vbs |> List.map (fun { pvb_pat; _ } -> pvb_pat)))
     [%expr Lwt_main.run [%e gen_exp vbs 0]]]

let lwt_sequence mapper ~lhs ~rhs =
  let lhs, rhs = mapper.expr mapper lhs, mapper.expr mapper rhs in
  if !debug then
    [%expr
      let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
      Lwt.backtrace_bind
        (fun exn -> try Reraise.reraise exn with exn -> exn)
        [%e lhs]
        (fun () -> [%e rhs])
    ] [@metaloc lhs.pexp_loc]
  else
    [%expr Lwt.bind [%e lhs] (fun () -> [%e rhs])]

(** For expressions only *)
(* We only expand the first level after a %lwt.
   After that, we call the mapper to expand sub-expressions. *)
let lwt_expression mapper exp attributes =
  default_loc := exp.pexp_loc;
  let pexp_attributes = attributes @ exp.pexp_attributes in
  match exp.pexp_desc with

  (* $e$;%lwt $e'$ ≡ [Lwt.bind $e$ (fun $p$ -> $e'$)] *)
  | Pexp_sequence (lhs, rhs) ->
     lwt_sequence mapper ~lhs ~rhs
  (* [let%lwt $p$ = $e$ in $e'$] ≡ [Lwt.bind $e$ (fun $p$ -> $e'$)] *)
  | Pexp_let (Nonrecursive, vbl , e) ->
    let new_exp =
      Exp.let_
        Nonrecursive
        (gen_bindings vbl)
        (gen_binds exp.pexp_loc vbl e)
    in mapper.expr mapper { new_exp with pexp_attributes }

  (* [match%lwt $e$ with $c$] ≡ [Lwt.bind $e$ (function $c$)]
     [match%lwt $e$ with exception $x$ | $c$] ≡
     [Lwt.try_bind (fun () -> $e$) (function $c$) (function $x$)] *)
  | Pexp_match (e, cases) ->
    let exns, cases =
      cases |> List.partition (
        function
        | {pc_lhs = [%pat? exception [%p? _]]; _} -> true
        | _ -> false)
    in
    let exns =
      exns |> List.map (
        function
        | {pc_lhs = [%pat? exception [%p? pat]]; _} as case ->
          { case with pc_lhs = pat }
        | _ -> assert false)
    in
    let exns = add_wildcard_case exns in
    let new_exp =
      match exns with
      | [] -> [%expr Lwt.bind [%e e] [%e Exp.function_ cases]]
      | _  ->  [%expr Lwt.try_bind (fun () -> [%e e])
                                   [%e Exp.function_ cases]
                                   [%e Exp.function_ exns]]
    in
    mapper.expr mapper { new_exp with pexp_attributes }

  (* [assert%lwt $e$] ≡
     [try Lwt.return (assert $e$) with exn -> Lwt.fail exn] *)
  | Pexp_assert e ->
    let new_exp =
      [%expr try Lwt.return (assert [%e e]) with exn -> Lwt.fail exn]
    in mapper.expr mapper { new_exp with pexp_attributes }

  (* [while%lwt $cond$ do $body$ done] ≡
     [let rec __ppx_lwt_loop () =
        if $cond$ then Lwt.bind $body$ __ppx_lwt_loop
        else Lwt.return_unit
      in __ppx_lwt_loop]
  *)
  | Pexp_while (cond, body) ->
    let new_exp =
      [%expr
        let rec __ppx_lwt_loop () =
          if [%e cond] then Lwt.bind [%e body] __ppx_lwt_loop
          else Lwt.return_unit
        in __ppx_lwt_loop ()
      ]
    in mapper.expr mapper { new_exp with pexp_attributes }

  (* [for%lwt $p$ = $start$ (to|downto) $end$ do $body$ done] ≡
     [let __ppx_lwt_bound = $end$ in
     let rec __ppx_lwt_loop $p$ =
       if $p$ COMP __ppx_lwt_bound then Lwt.return_unit
       else Lwt.bind $body$ (fun () -> __ppx_lwt_loop ($p$ OP 1))
     in __ppx_lwt_loop $start$]
  *)
  | Pexp_for ({ppat_desc = Ppat_var p_var; _} as p, start, bound, dir, body) ->
    let comp, op = match dir with
      | Upto ->   evar ">", evar "+"
      | Downto -> evar "<", evar "-"
    in
    let p' = with_loc (fun s -> evar s) p_var in

    let exp_bound = [%expr __ppx_lwt_bound] [@metaloc bound.pexp_loc] in
    let pat_bound = [%pat? __ppx_lwt_bound] [@metaloc bound.pexp_loc] in

    let new_exp =
      [%expr
        let [%p pat_bound] : int = [%e bound] in
        let rec __ppx_lwt_loop [%p p] =
          if [%e comp] [%e p'] [%e exp_bound] then Lwt.return_unit
          else Lwt.bind [%e body] (fun () -> __ppx_lwt_loop ([%e op] [%e p'] 1))
        in __ppx_lwt_loop [%e start]
      ]
    in mapper.expr mapper { new_exp with pexp_attributes }


  (* [try%lwt $e$ with $c$] ≡
     [Lwt.catch (fun () -> $e$) (function $c$)]
  *)
  | Pexp_try (expr, cases) ->
    let cases = add_wildcard_case cases in
    let new_exp =
      if !debug then
        [%expr
          let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
          Lwt.backtrace_catch
            (fun exn -> try Reraise.reraise exn with exn -> exn)
            (fun () -> [%e expr])
            [%e Exp.function_ cases]
        ]
      else
        [%expr Lwt.catch (fun () -> [%e expr]) [%e Exp.function_ cases]]
    in
    mapper.expr mapper { new_exp with pexp_attributes }

  (* [if%lwt $c$ then $e1$ else $e2$] ≡
     [match%lwt $c$ with true -> $e1$ | false -> $e2$]
     [if%lwt $c$ then $e1$] ≡
     [match%lwt $c$ with true -> $e1$ | false -> Lwt.return_unit]
  *)
  | Pexp_ifthenelse (cond, e1, e2) ->
    let e2 = match e2 with None -> [%expr Lwt.return_unit] | Some e -> e in
    let cases =
      [
        Exp.case [%pat? true] e1 ;
        Exp.case [%pat? false] e2 ;
      ]
    in
    let new_exp = [%expr Lwt.bind [%e cond] [%e Exp.function_ cases]] in
    mapper.expr mapper { new_exp with pexp_attributes }

  (* [[%lwt $e$]] ≡ [Lwt.catch (fun () -> $e$) Lwt.fail] *)
  | _ ->
    let exp =
      match exp with
      | { pexp_loc; pexp_desc=Pexp_let (Recursive, _, _); pexp_attributes } ->
        let attr = attribute_of_warning pexp_loc "\"let%lwt rec\" is not a recursive Lwt binding" in
        { exp with pexp_attributes = attr :: pexp_attributes }
      | _ -> exp
    in
    let new_exp =
      if !debug then
        [%expr
          let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
          Lwt.backtrace_catch
            (fun exn -> try Reraise.reraise exn with exn -> exn)
            (fun () -> [%e exp])
            Lwt.fail
        ]
      else
        [%expr Lwt.catch (fun () -> [%e exp]) Lwt.fail]
    in
    let warning =
      attribute_of_warning
        exp.pexp_loc
        ("[%lwt ...] is deprecated\n" ^
         "  See https://github.com/ocsigen/lwt/issues/527")
    in
    let pexp_attributes = warning::pexp_attributes in
    mapper.expr mapper { new_exp with pexp_attributes }

let make_loc {Location.loc_start; _} =
  let (file, line, char) = Location.get_pos_info loc_start in
  [%expr ([%e str file], [%e int line], [%e int char])]

(**
   [Lwt_log.error "message"] ≡
   [let __pa_log_section = Lwt_log.Section.main in
   if Lwt_log.Error >= (Lwt_log.Section.level __pa_log_section)
   then Lwt_log.error ~location:("foo.ml", 1, 0) ~section:__pa_log_section "message"
   else Lwt.return_unit];
   [Lwt_log.error ~section "message"] ≡
   [let __pa_log_section = section in ...].
   Additionally, remove debug-level statements if -no-debug is given. **)
let lwt_log mapper fn args attrs loc =
  let open Longident in
  match fn with
  | {pexp_desc = Pexp_ident {txt = Ldot (Lident "Lwt_log", func); _}; _} ->
    let len = String.length func in
    let fmt = len >= 2 && func.[len - 2] = '_' && func.[len - 1] = 'f'
    and ign = len >= 4 && func.[0] = 'i' && func.[1] = 'g' && func.[2] = 'n' && func.[3] = '_' in
    let level =
      match fmt, ign with
      | false, false -> func
      | true,  false -> String.sub func 0 (len - 2)
      | false, true  -> String.sub func 4 (len - 4)
      | true,  true  -> String.sub func 4 (len - 6)
    in
    let level = (String.capitalize [@ocaml.warning "-3"]) level in
    if level = "Debug" && (not !debug) then
      let new_exp = if ign then [%expr ()] else [%expr Lwt.return_unit] in
      mapper.expr mapper { new_exp with pexp_attributes = attrs }
    else if List.mem level ["Fatal"; "Error"; "Warning"; "Notice"; "Info"; "Debug"] then
      let args = List.map (fun (l,e) -> l, mapper.expr mapper e) args in
      let new_exp =
        let args = (Label.labelled "location", make_loc loc) ::
                   (Label.labelled "section",  [%expr __pa_log_section]) ::
                   List.remove_assoc (Label.labelled "section") args in
        [%expr
          if [%e Exp.construct (def_loc (Ldot (Lident "Lwt_log", level))) None] >=
             Lwt_log.Section.level __pa_log_section then
            [%e Exp.apply (Exp.ident (def_loc (Ldot (Lident "Lwt_log", func)))) args]
          else
            [%e if ign then [%expr ()] else [%expr Lwt.return_unit]]]
      in
      try
        let section = List.assoc (Label.labelled "section") args in
        [%expr let __pa_log_section = [%e section] in [%e new_exp]]
      with Not_found ->
        [%expr let __pa_log_section = Lwt_log.Section.main in [%e new_exp]]
    else default_mapper.expr mapper (Exp.apply ~attrs fn args)
  | _ -> default_mapper.expr mapper (Exp.apply ~attrs fn args)

let warned = ref false

let mapper =
  { default_mapper with

    structure = begin fun mapper structure ->
      if !warned then
        default_mapper.structure mapper structure

      else begin
        warned := true;
        let structure = default_mapper.structure mapper structure in
        let loc = Location.in_file !Location.input_name in

        let warn_if condition message structure =
          if condition then
            (Str.attribute ~loc (attribute_of_warning loc message))::structure
          else
            structure
        in

        structure
        |> warn_if (!used_no_strict_sequence_option)
          ("-no-strict-sequence is a deprecated Lwt PPX option\n" ^
           "  See https://github.com/ocsigen/lwt/issues/495")
        |> warn_if (!used_no_sequence_option)
          ("-no-sequence is a deprecated Lwt PPX option\n" ^
           "  See https://github.com/ocsigen/lwt/issues/495")
        |> warn_if (!used_no_log_option || !used_log_option)
          ("Lwt PPX logging support is deprecated\n" ^
           "  See https://github.com/ocsigen/lwt/issues/520")
        |> warn_if (!used_no_log_option)
          "-no-log is a deprecated Lwt PPX option"
        |> warn_if (!used_log_option)
          "-log is a deprecated Lwt PPX option"
        |> warn_if (!used_no_debug_option)
          ("-no-debug is a deprecated Lwt PPX option\n" ^
           "  See https://github.com/ocsigen/lwt/issues/528")
      end
    end;

    expr = (fun mapper expr ->
      match expr with
      | [%expr [%lwt [%e? exp]]] ->
        lwt_expression mapper exp expr.pexp_attributes

      (* [($e$)[%finally $f$]] ≡
         [Lwt.finalize (fun () -> $e$) (fun () -> $f$)] *)
      | [%expr [%e? exp ] [%finally     [%e? finally]] ]
      | [%expr [%e? exp ] [%lwt.finally [%e? finally]] ] ->
        let new_exp =
          if !debug then
            [%expr
              let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
              Lwt.backtrace_finalize
                (fun exn -> try Reraise.reraise exn with exn -> exn)
                (fun () -> [%e exp])
                (fun () -> [%e finally])
            ]
          else
            [%expr Lwt.finalize (fun () -> [%e exp]) (fun () -> [%e finally])]
        in
        mapper.expr mapper
          { new_exp with
            pexp_attributes = expr.pexp_attributes @ exp.pexp_attributes
          }

      | [%expr [%finally     [%e? _ ]]]
      | [%expr [%lwt.finally [%e? _ ]]] ->
        raise (Location.Error (
          Location.errorf
            ~loc:expr.pexp_loc
            "Lwt's finally should be used only with the syntax: \"(<expr>)[%%finally ...]\"."
        ))

      | [%expr [%e? lhs] >> [%e? rhs]] as e ->
        if !sequence then
          let pat = if !strict_seq then [%pat? ()] else [%pat? _] in
          let lhs, rhs = mapper.expr mapper lhs, mapper.expr mapper rhs in
          let op = match e.Parsetree.pexp_desc with
            | Parsetree.Pexp_apply (op, _) -> op
            | _ -> assert false
          in
          if !debug then
            Ast_helper.Exp.attr
              ([%expr
                let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
                Lwt.backtrace_bind
                  (fun exn -> try Reraise.reraise exn with exn -> exn)
                  [%e lhs]
                  (fun [%p pat] -> [%e rhs])
              ] [@metaloc lhs.pexp_loc])
              (Ast_mapper.attribute_of_warning op.Parsetree.pexp_loc
                "The operator >> is deprecated")
          else
            Ast_helper.Exp.attr
              [%expr (Lwt.bind [%e lhs] (fun [%p pat] -> [%e rhs]))]
              (Ast_mapper.attribute_of_warning op.Parsetree.pexp_loc
                "The operator >> is deprecated")
        else
          default_mapper.expr mapper expr
      | { pexp_desc = Pexp_apply (fn, args); pexp_attributes; pexp_loc } when !log ->
        default_loc := pexp_loc;
        lwt_log mapper fn args pexp_attributes pexp_loc
      | _ ->
        default_mapper.expr mapper expr);
    structure_item = (fun mapper stri ->
      default_loc := stri.pstr_loc;
      match stri with
      | [%stri let%lwt [%p? var] = [%e? exp]] ->
        [%stri let [%p var] = Lwt_main.run [%e mapper.expr mapper exp]]

      | {pstr_desc = Pstr_extension (({txt = "lwt"; _}, PStr [
        {pstr_desc = Pstr_value (Recursive, _); _}]) as content, attrs); pstr_loc} ->
        {stri with pstr_desc =
          Pstr_extension (content, warn_let_lwt_rec pstr_loc attrs)}

      | {pstr_desc = Pstr_extension (({txt = "lwt"; _}, PStr [
        {pstr_desc = Pstr_value (Nonrecursive, vbs); _}]), _); _} ->
        mapper.structure_item mapper (Str.value Nonrecursive (gen_top_binds vbs))
      | x -> default_mapper.structure_item mapper x);
}


let args =
  Arg.([
    "-no-debug",
      Unit no_debug_option,
      " disable debug mode (deprecated)";

    "-log",
      Unit log_option,
      " enable logging (deprecated)";

    "-no-log",
      Unit no_log_option,
      " disable logging (deprecated)";

    "-no-sequence",
      Unit no_sequence_option,
      " disable sequence operator (deprecated)";

    "-no-strict-sequence",
      Unit no_strict_sequence_option,
      " allow non-unit sequence operations (deprecated)";
  ])

let () =
  Driver.register ~name:"ppx_lwt" ~args Versions.ocaml_404
    (fun _config _cookies -> mapper)
