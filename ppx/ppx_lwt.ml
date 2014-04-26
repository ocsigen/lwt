open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

(** {2 Convenient stuff} *)

let str ?(loc = !default_loc) txt =
  {Location. loc ; txt }

let lid ?loc s = str ?loc @@ Longident.Lident s

let map_loc f { txt ; loc } =
  { txt = f txt ; loc }

let str2lid = map_loc (fun x -> Longident.Lident x)

let ident ?loc ?attrs x =
  Exp.ident ?loc ?attrs @@ str ?loc x

let unit ?loc ?attrs () = (* () in exprs *)
  Exp.construct ?loc ?attrs (str ?loc @@ Longident.Lident "()") None
let unit_pat ?loc ?attrs () = (* () in patterns *)
  Pat.construct ?loc ?attrs (str ?loc @@ Longident.Lident "()") None
let unit_fun ?loc ?attrs e = (* fun () -> ... *)
  (Exp.fun_ "" None (unit_pat ()) e)


(** {2 Function names} *)

module Lwt = struct
  open Longident
  let lwt x = Ldot (Lident "Lwt", x)
  let bind = lwt "bind"
  let fail = lwt "fail"
  let return = lwt "return"
end

(** {3 Internal names} *)

let lwt_prefix = "__pa_lwt_"
let lwt_loop_name = lwt_prefix ^ "loop"
let lwt_bound_name = lwt_prefix ^ "bound"

(** {2 Here we go!} *)

(** let%lwt related functions *)

let gen_name i =
  lwt_prefix ^ string_of_int i

(** [p = x] ≡ [__pa_lwt_$i = x] *)
let gen_bindings l =
  let aux i binding =
    { binding with
      pvb_pat = Pat.var @@ str ~loc:binding.pvb_expr.pexp_loc @@ gen_name i }
  in
  List.mapi aux l

(** [p = x] and e ≡ [Lwt.bind __pa_lwt_$i (fun p -> e)] *)
let gen_binds e_loc l e =
  let rec aux i bindings =
    match bindings with
    | [] -> e
    | binding :: t ->
       let attrs = binding.pvb_attributes in
       let p = binding.pvb_pat in
       let name = (* __pa_lwt_$i, at the position of $x$ *)
	 ident ~loc:binding.pvb_expr.pexp_loc @@ Longident.Lident (gen_name i)
       in
       Exp.apply ~loc:e_loc ~attrs
	 (ident ~loc:e_loc Lwt.bind)
	 [ "", name ;
	   "", Exp.fun_ ~loc:binding.pvb_loc ~attrs "" None p @@ aux (i+1) t ;
	 ]
  in aux 0 l

(** For expressions only *)
(* We only expand the first level after a %lwt.
   After that, we call the mapper to expand sub-expressions. *)
let lwt_expression mapper ({ pexp_attributes = attrs } as exp) =
  default_loc := exp.pexp_loc ;
  match exp.pexp_desc with

  (** [let%lwt $p$ = $e$ in $e'$] ≡ [Lwt.bind $e$ (fun $p$ -> $e'$)] *)
  | Pexp_let (Nonrecursive, vbl , e) ->
     let new_exp =
       Exp.let_ ~attrs
	 Nonrecursive
	 (gen_bindings vbl)
	 (gen_binds exp.pexp_loc vbl e)
     in mapper.expr mapper new_exp

  (** [match%lwt $e$ with $c$] ≡ [Lwt.bind $e$ (function $c$)] *)
  | Pexp_match (e , cases) ->
     let new_exp =
       Exp.apply ~attrs
	 (ident Lwt.bind)
	 [ "", e ; "", Exp.function_ cases ]

     in mapper.expr mapper new_exp

  (** [assert%lwt $e$] ≡
      [try Lwt.return (assert $e$) with exn -> Lwt.fail exn] *)
  | Pexp_assert e ->
     let fail_case =
       Exp.case
	 (Pat.var @@ str "exn")
	 (Exp.apply
	    (ident Lwt.fail)
	    [ "", Exp.ident @@ lid "exn" ])
     in
     let new_exp =
       Exp.try_ ~attrs
	 (Exp.apply
	    (ident Lwt.return)
	    [ "", Exp.assert_ e ])
	 [fail_case]

     in mapper.expr mapper new_exp

  (** [while%lwt $cond$ do $body$ done] ≡
      [let rec __pa_lwt_loop () =
         if $cond$ then Lwt.bind $body$ __pa_lwt_loop
         else Lwt.return ()
       in __pa_lwt_loop]
   *)
  | Pexp_while (cond, body) ->
     let else_ = Exp.apply (ident Lwt.return) [ "", unit () ] in
     let then_ =
       Exp.apply
	 (ident Lwt.bind)
	 [ "", body ; "", Exp.ident @@ lid lwt_loop_name ]
     in
     let new_exp =
       Exp.let_ Recursive
	 [Vb.mk
	    (Pat.var @@ str lwt_loop_name)
	    (unit_fun @@ Exp.ifthenelse cond then_ (Some else_)) ]
	 (Exp.ident @@ lid lwt_loop_name)

     in mapper.expr mapper new_exp

  (** [for%lwt $p$ = $start$ (to|downto) $end$ do $body$ done] ≡
      [let __pa_lwt_bound = $end$ in
       let rec __pa_lwt_loop $p$ =
         if $p$ COMP __pa_lwt_bound then Lwt.return ()
         else Lwt.bind $body$ (fun () -> __pa_lwt_loop ($p$ OP 1))
       in __pa_lwt_loop $start$]
   *)
  | Pexp_for (({ ppat_desc = Ppat_var p_var} as p), start, bound, dir, body) ->
     let (comp, op) = match dir with
       | Upto -> ( lid ">", lid "+" )
       | Downto -> ( lid "<" , lid "-" )
     in
     let p_exp = Exp.ident @@ str2lid p_var in
     let bound_loc = bound.pexp_loc in
     let binop op x y = Exp.apply (Exp.ident op) [ "", x ; "", y] in
     let then_ = Exp.apply (ident Lwt.return) [ "", unit () ] in
     let loop_call =
       Exp.apply
	 (Exp.ident @@ lid lwt_loop_name)
	 [ "", binop op p_exp (Exp.constant @@ Const_int 1)]
     in
     let else_ =
       Exp.apply (ident Lwt.bind) [ "", body ; "", unit_fun loop_call ]
     in
     let cond = binop comp p_exp
       (Exp.ident ~loc:bound_loc @@ lid ~loc:bound_loc lwt_bound_name) in
     let new_exp =
       Exp.let_ Nonrecursive
	 [Vb.mk
	    (Pat.var ~loc:bound_loc @@ str ~loc:bound_loc lwt_bound_name)
	    (Exp.constraint_ bound @@ Typ.constr (lid "int") [])
	    (* For type error reporting on the bound. *)
	 ] @@
	 Exp.let_ Recursive
	   [Vb.mk
	      (Pat.var @@ str lwt_loop_name)
	      (Exp.fun_ "" None p @@ Exp.ifthenelse cond then_ (Some else_)) ]
	   (Exp.apply (Exp.ident @@ lid lwt_loop_name) [ "", start ])

     in mapper.expr mapper new_exp

  | _ -> exp



let lwt_mapper _ =
  { default_mapper with
    expr = fun mapper expr ->
	   match expr with
	   | { pexp_desc =
		 Pexp_extension (
		     { txt = "lwt" },
		     PStr [{ pstr_desc = Pstr_eval (exp, _attrs )}]
	     )} ->
	      lwt_expression mapper exp

	   | x -> default_mapper.expr mapper x;
  }

let () = run_main lwt_mapper
