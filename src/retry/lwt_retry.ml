(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Lwt.Syntax

let default_sleep_duration n' =
  let base_sleep_time = 2.0 in
  let n = Int.to_float n' in
  n *. base_sleep_time *. Float.pow 2.0 n

type ('retry, 'fatal) error =
  [ `Retry of 'retry
  | `Fatal of 'fatal
  ]

let pp_opaque fmt _ = Format.fprintf fmt "<opaque>"

let pp_error ?(retry = pp_opaque) ?(fatal = pp_opaque) fmt err =
  match err with
  | `Retry r -> Format.fprintf fmt "`Retry %a" retry r
  | `Fatal f -> Format.fprintf fmt "`Fatal %a" fatal f

let equal_error ~retry ~fatal a b =
  match a, b with
  | `Retry a', `Retry b' -> retry a' b'
  | `Fatal a', `Fatal b' -> fatal a' b'
  | _ -> false

type ('ok, 'retry, 'fatal) attempt = ('ok, ('retry, 'fatal) error * int) result

let on_error
    (f : unit -> ('ok, ('retry, 'fatal) error) result Lwt.t)
  : ('ok, 'retry, 'fatal) attempt Lwt_stream.t
  =
  let i = ref 0 in
  let stop = ref false in
  Lwt_stream.from begin fun () ->
    incr i;
    if !stop then
      Lwt.return None
    else
      let+ result = f () in
      match result with
      | Error (`Retry _ as retry) -> Some (Error (retry, !i))
      | Error (`Fatal _ as fatal) -> stop := true; Some (Error (fatal, !i))
      | Ok _ as ok -> stop := true; Some ok
  end

let with_sleep ?(duration=default_sleep_duration) (attempts : _ attempt Lwt_stream.t) : _ attempt Lwt_stream.t =
  attempts
  |> Lwt_stream.map_s begin function
    | Ok _ as ok -> Lwt.return ok
    | Error (_, n) as err ->
      let* () = Lwt_unix.sleep @@ duration n in
      Lwt.return err
  end

let n_times n attempts =
  if n < 0 then invalid_arg "Lwt_retry.n_times: n must be non-negative";
  (* The first attempt is a try, and re-tries start counting from n + 1 *)
  let retries = n + 1 in
  let+ attempts = Lwt_stream.nget retries attempts in
  match List.rev attempts with
  | last :: _ -> last
  | _ -> failwith "Lwt_retry.n_times: impossible"
