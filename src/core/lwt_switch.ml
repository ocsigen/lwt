(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

exception Off

[@@@alert "-trespassing"]

module Lwt_dls = Lwt_dls

[@@@alert "+trespassing"]

type on_switch = {mutable hooks : (unit -> unit Lwt.t) list}
type state = St_on of on_switch | St_off

(* A switch belongs to the domain that created it, like a mutex: its hooks
   close that domain's resources. *)
type t = {mutable state : state; owner : Lwt_dls.token}

let create () = {state = St_on {hooks = []}; owner = Lwt_dls.self_token ()}
let is_on switch = match switch.state with St_on _ -> true | St_off -> false

let check = function
  | Some {state = St_off; _} -> raise Off
  | Some {state = St_on _; _} | None -> ()

let add_hook switch hook =
  match switch with
  | Some ({state = St_on os; _} as sw) ->
      Lwt_dls.check_owner "Lwt_switch.add_hook" sw.owner;
      os.hooks <- hook :: os.hooks
  | Some {state = St_off; _} -> raise Off
  | None -> ()

let add_hook_or_exec switch hook =
  match switch with
  | Some sw -> (
      (* Checked even when the switch is off, so that another domain is refused
         whatever the state of the switch. *)
      Lwt_dls.check_owner "Lwt_switch.add_hook_or_exec" sw.owner;
      match sw.state with
      | St_on os ->
          os.hooks <- hook :: os.hooks;
          Lwt.return_unit
      | St_off -> hook ())
  | None -> Lwt.return_unit

let turn_off switch =
  Lwt_dls.check_owner "Lwt_switch.turn_off" switch.owner;
  match switch.state with
  | St_on {hooks} ->
      switch.state <- St_off;
      Lwt.join (List.map (fun hook -> Lwt.apply hook ()) hooks)
  | St_off -> Lwt.return_unit

let with_switch fn =
  let switch = create () in
  Lwt.finalize (fun () -> fn switch) (fun () -> turn_off switch)
