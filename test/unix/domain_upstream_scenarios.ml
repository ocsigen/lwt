(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* The five multidomain scenarios of Lwt 6.0.0-beta00, whose multidomain mode
   was removed in beta01
   (test/multidomain/{basic,movingpromises,domainworkers,unixpipe,preempting}).
   They share Lwt values between domains, which is now refused. Each section
   checks that the original version is refused, and that a version keeping
   each value on its own domain runs. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let contains needle m =
  let n = String.length needle in
  let rec go i =
    i + n <= String.length m && (String.sub m i n = needle || go (i + 1))
  in
  go 0

let on_other_domain f = Domain.join (Domain.spawn f)

(* Refused because the value belongs to another domain, whichever mechanism
   says so: the core's own check on promises, the containers', or
   Lwt_unix's for what goes through its notification descriptor. *)
let refused f =
  match f () with
  | _ -> false
  | exception Lwt.Foreign_promise -> true
  | exception Invalid_argument m -> contains "belongs to another domain" m
  | exception Failure m ->
      contains "only available on the domain that initialized Lwt_unix" m
  | exception _ -> false

let () =
  (* 1. basic: two domains resolving each other's promises. *)
  let p, w = Lwt.wait () in
  check "basic: resolving our promise from another domain is refused"
    (on_other_domain (fun () -> refused (fun () -> Lwt.wakeup w 1)));
  check "basic: binding our promise from another domain is refused"
    (on_other_domain (fun () -> refused (fun () -> Lwt.bind p (fun _ -> p))));
  check "basic: and it still works on its own domain"
    (match Lwt.wakeup w 1; Lwt.state p with Lwt.Return 1 -> true | _ -> false);
  (* 2. movingpromises: a resolver handed to a worker on another domain. *)
  let task, resolver = Lwt.task () in
  check "movingpromises: a resolver does not travel"
    (on_other_domain (fun () -> refused (fun () -> Lwt.wakeup_later resolver 1)));
  check "movingpromises: cancelling from elsewhere is refused too"
    (on_other_domain (fun () -> refused (fun () -> Lwt.cancel task)));
  check "movingpromises: and the task is resolved from its own domain"
    (match
       Lwt.wakeup_later resolver 1;
       Lwt.state task
     with
    | Lwt.Return 1 -> true
    | _ -> false);
  (* 3. domainworkers: a task stream written here, read by a worker there. *)
  let recv_task, send_task = Lwt_stream.create () in
  send_task (Some "work");
  check "domainworkers: a worker on another domain cannot read our stream"
    (on_other_domain (fun () -> refused (fun () -> Lwt_stream.get recv_task)));
  (* The domain-local version: a worker on its own stream, on its own loop.
     Distributing work across domains needs a data channel, not Lwt values. *)
  check "domainworkers: a worker on its own stream works"
    (on_other_domain (fun () ->
       let recv, send = Lwt_stream.create () in
       List.iter (fun s -> send (Some s)) ["a"; "bb"; "ccc"];
       send None;
       match
         Lwt_main.run
           (Lwt_stream.fold (fun s acc -> acc + String.length s) recv 0)
       with
       | 6 -> true
       | _ -> false
       | exception _ -> false));
  (* 4. unixpipe: one pipe, its two ends given to two domains. *)
  let r, wfd = Lwt_unix.pipe () in
  check "unixpipe: an end of our pipe is not usable elsewhere"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_unix.write wfd (Bytes.of_string "x") 0 1)));
  check "unixpipe: nor is the reading end"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_unix.read r (Bytes.create 1) 0 1)));
  (* Each domain owning its own pipe is what domain_two_loops.ml exercises with
     real traffic; here it is enough that ours still works. *)
  check "unixpipe: ours still works here"
    (match
       Lwt_main.run
         (Lwt.bind
            (Lwt_unix.write wfd (Bytes.of_string "x") 0 1)
            (fun _ -> Lwt_unix.read r (Bytes.create 1) 0 1))
     with
    | 1 -> true
    | _ -> false
    | exception _ -> false);
  (try Lwt_main.run (Lwt_unix.close r) with _ -> ());
  (try Lwt_main.run (Lwt_unix.close wfd) with _ -> ());
  (* 5. preempting: detaching blocking work from a spawned domain. *)
  check "preempting: detaching from another domain is refused"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_main.run (Lwt_preemptive.detach (fun () -> 1) ()))));
  check "preempting: detaching from the main domain works"
    (match Lwt_main.run (Lwt_preemptive.detach String.length "hello") with
    | 5 -> true
    | _ -> false
    | exception _ -> false);
  if !failures > 0 then exit 1;
  print_endline "6.0.0-beta00 multidomain scenarios: ok"
