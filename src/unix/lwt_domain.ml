open Lwt.Infix

module C = Domainslib.Chan
module T = Domainslib.Task

(* Maximum number of domains: *)
let max_domains : int ref = ref 0

(* Size of the waiting queue: *)
let max_domains_queued = ref 128

let get_max_number_of_domains_queued _ =
  !max_domains_queued

let set_max_number_of_domains_queued n =
  if n < 0 then invalid_arg "Lwt_domain.set_max_number_of_domains_queued";
  max_domains_queued := n
(* Number of live domains *)
let domains_count = ref 0

let get_bounds () = !max_domains

(* Initial pool with only the parent domain *)
let pool = ref (T.setup_pool ~num_domains:0)

let initialized = ref false

(* Destroys old pool and creates a new pool with `num` domains *)
let set_bounds num =
  max_domains := num;
  T.teardown_pool !pool;
  pool := T.setup_pool ~num_domains:!max_domains;
  domains_count := !max_domains

let init max _errlog =
  initialized := true;
  set_bounds max

let simple_init () =
  if not !initialized then begin
    initialized := true;
    set_bounds 4
  end

let init_result = Result.Error (Failure "Lwt_domain.detach")

let detach f args =
  simple_init ();
  let result = ref init_result in
  let task () =
    try
      result := Result.Ok (f args)
    with exn ->
      result := Result.Error exn
  in
  let waiter, wakener = Lwt.wait () in
  let id =
    Lwt_unix.make_notification ~once:true
      (fun () -> Lwt.wakeup_result wakener !result)
  in
  let _ = T.async !pool (fun _ -> task ();
  Lwt_unix.send_notification id) in
  waiter

let nbdomains () = !domains_count

(* +-----------------------------------------------------------------+
   | Running Lwt threads in the main domain                          |
   +-----------------------------------------------------------------+ *)

(* Jobs to be run in the main domain *)
let jobs = C.make_unbounded ()
let job_done = C.make_bounded 0
let job_notification =
  Lwt_unix.make_notification
    (fun () ->
      let thunk = C.recv jobs in
      ignore (thunk ()))

let run_in_main f =
  let res = ref init_result in
  let job () =
    Lwt.try_bind f
      (fun ret -> Lwt.return (Result.Ok ret))
      (fun exn -> Lwt.return (Result.Error exn)) >>= fun result ->
    res := result;
    C.send job_done 1;
    Lwt.return_unit
  in
  C.send jobs job;
  Lwt_unix.send_notification job_notification;
  (* blocks calling domain until the job is executed *)
  ignore @@ C.recv job_done;
  match !res with
  | Result.Ok ret -> ret
  | Result.Error exn -> raise exn