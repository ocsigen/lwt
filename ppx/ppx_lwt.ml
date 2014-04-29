open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

open Ast_convenience

(** {2 Convenient stuff} *)

let with_loc f { txt ; loc } =
  (f txt)[@metaloc loc]


(** {3 Internal names} *)

let lwt_prefix = "__pa_lwt_"

(** {2 Here we go!} *)

(** let%lwt related functions *)

let gen_name i = lwt_prefix ^ string_of_int i

(** [p = x] ≡ [__pa_lwt_$i = x] *)
let gen_bindings l =
  let aux i binding =
    { binding with
      pvb_pat = (pstr @@ gen_name i)[@metaloc binding.pvb_expr.pexp_loc]
    }
  in
  List.mapi aux l

(** [p = x] and e ≡ [Lwt.bind __pa_lwt_$i (fun p -> e)] *)
let gen_binds e_loc l e =
  let rec aux i bindings =
    match bindings with
    | [] -> e
    | binding :: t ->
       let name = (* __pa_lwt_$i, at the position of $x$ *)
	 (str @@ gen_name i)[@metaloc binding.pvb_expr.pexp_loc]
       in
       let fun_ =
	 [%expr
	     (fun [%p binding.pvb_pat] -> [%e aux (i+1) t])
	 ] [@metaloc binding.pvb_loc]
       in
       let new_exp =
	 [%expr
	     Lwt.bind [%e name] [%e fun_]
	 ] [@metaloc e_loc]
       in
       { new_exp with pexp_attributes = binding.pvb_attributes }
  in aux 0 l

(** For expressions only *)
(* We only expand the first level after a %lwt.
   After that, we call the mapper to expand sub-expressions. *)
let lwt_expression mapper ({ pexp_attributes } as exp) =
  default_loc := exp.pexp_loc ;
  match exp.pexp_desc with

  (** [let%lwt $p$ = $e$ in $e'$] ≡ [Lwt.bind $e$ (fun $p$ -> $e'$)] *)
  | Pexp_let (Nonrecursive, vbl , e) ->
     let new_exp =
       Exp.let_
	 Nonrecursive
	 (gen_bindings vbl)
	 (gen_binds exp.pexp_loc vbl e)
     in mapper.expr mapper { new_exp with pexp_attributes }

  (** [match%lwt $e$ with $c$] ≡ [Lwt.bind $e$ (function $c$)] *)
  | Pexp_match (e , cases) ->
     let new_exp =
       [%expr
	   Lwt.bind [%e e] [%e Exp.function_ cases]
       ]
     in mapper.expr mapper { new_exp with pexp_attributes }

  (** [assert%lwt $e$] ≡
      [try Lwt.return (assert $e$) with exn -> Lwt.fail exn] *)
  | Pexp_assert e ->
     let new_exp =
       [%expr
	   try Lwt.return (assert [%e e]) with exn -> Lwt.fail exn
       ]
     in mapper.expr mapper { new_exp with pexp_attributes }

  (** [while%lwt $cond$ do $body$ done] ≡
      [let rec __pa_lwt_loop () =
         if $cond$ then Lwt.bind $body$ __pa_lwt_loop
         else Lwt.return ()
       in __pa_lwt_loop]
   *)
  | Pexp_while (cond, body) ->
     let new_exp =
       [%expr
	let rec __pa_lwt_loop () =
	  if [%e cond] then Lwt.bind [%e body] __pa_lwt_loop
	  else Lwt.return ()
	in __pa_lwt_loop
       ]
     in mapper.expr mapper { new_exp with pexp_attributes }

  (** [for%lwt $p$ = $start$ (to|downto) $end$ do $body$ done] ≡
      [let __pa_lwt_bound = $end$ in
       let rec __pa_lwt_loop $p$ =
         if $p$ COMP __pa_lwt_bound then Lwt.return ()
         else Lwt.bind $body$ (fun () -> __pa_lwt_loop ($p$ OP 1))
       in __pa_lwt_loop $start$]
   *)
  | Pexp_for (({ ppat_desc = Ppat_var p_var} as p), start, bound, dir, body) ->
     let comp, op = match dir with
       | Upto ->   str ">", str "+"
       | Downto -> str "<", str "-"
     in
     let p' = with_loc str p_var in

     let exp_bound = [%expr __pa_lwt_bound ] [@metaloc bound.pexp_loc] in
     let pat_bound = [%pat? __pa_lwt_bound ] [@metaloc bound.pexp_loc] in

     let new_exp =
       [%expr
	let [%p pat_bound] : int = [%e bound] in
	let rec __pa_lwt_loop [%p p] =
          if [%e comp] [%e p'] [%e exp_bound] then Lwt.return ()
          else Lwt.bind [%e body] (fun () -> __pa_lwt_loop ([%e op] [%e p'] 1))
	in __pa_lwt_loop [%e start]
       ]
     in mapper.expr mapper { new_exp with pexp_attributes }

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
