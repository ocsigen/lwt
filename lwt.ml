
type 'a clist =
    CNil
  | CSingle of 'a
  | CApp of 'a clist * 'a clist

let ccons v l =
  match l with
    CNil -> CSingle v
  | _    -> CApp (CSingle v, l)

let capp l l' =
  match l, l' with
    CNil, _ -> l'
  | _, CNil -> l
  | _       -> CApp (l, l')

let rec citer_rec f l rem =
  match l, rem with
    CNil, [] ->
      ()
  | CNil, l :: rem ->
      citer_rec f l rem
  | CSingle v, [] ->
      f v
  | CSingle v, l :: rem ->
      f v;
      citer_rec f l rem
  | CApp (l, l'), _ ->
      citer_rec f l (l' :: rem)

let citer f l = citer_rec f l []

(* Either a thread ['a t] has terminated, either successfully [Return of 'a] or
 *  unsuccessfully [Fail of exn], or it is sleeping, or it behaves the same
 *  as some representant.
 *)
type 'a state =
    Return of 'a
  | Fail of exn
  | Sleep
  | Repr of 'a t  (* implements union-find *)

(* A suspended thread is described by ['a t]
 * It could have several [waiters], which are thunk functions
 *)
and 'a t =
  { mutable state : 'a state;
    mutable waiters : ('a t -> unit) clist }

(* [make st] returns a thread of state [st] and no waiters *)
let make st = { state = st; waiters = CNil }

(* add a thunk [f] to the waiting list of thread [t]
   [f] will be called with an argument that is either [Fail ...] or
   [Return ...]. *)
let add_waiter t f = t.waiters <- ccons f t.waiters

let rec repr t =
  match t.state with
    Repr t' -> let t'' = repr t' in if t'' != t' then t.state <- Repr t''; t''
  | _       -> t

(* restart a sleeping thread [t], run all its waiters
 * and running all the waiters, and make the terminating state [st]
 * [caller] is a string that describes the caller
 *)
let restart t st caller =
  assert (st <> Sleep);
  let t = repr t in
  if t.state <> Sleep then invalid_arg caller;
  t.state <- st;
  let waiters = t.waiters in
  t.waiters <- CNil;
  citer (fun f -> f t) waiters

(*
 * pre-condition: [(repr t).state] is Sleep (i.e., not terminated)
 * [connect t t'] connects the two processes when t' finishes up
 * connecting means: running all the waiters for [t']
 * and assigning the state of [t'] to [t]
 *)
let rec connect t t' =
  let t = repr t in
  let t' = repr t' in
  if t.state <> Sleep then invalid_arg "connect";
  if t == t' then
    ()
  else if t'.state = Sleep then begin
    t'.state <- Repr t;
    t.waiters <- capp t'.waiters t.waiters;
    t'.waiters <- CNil
  end else begin
    t.state <- t'.state;
    let waiters = t.waiters in
    t.waiters <- CNil;
    citer (fun f -> f t) waiters
  end

(* similar to [connect t t']; does nothing instead of raising exception when
 * [t] is not asleep
 *)
let rec try_connect t t' =
  let t = repr t in
  let t' = repr t' in
  if t == t' || t.state <> Sleep then
    ()
  else if t'.state = Sleep then
    add_waiter t' (fun t' -> try_connect t t')
  else begin
    t.state <- t'.state;
    let waiters = t.waiters in
    t.waiters <- CNil;
    citer (fun f -> f t) waiters
  end

(* apply function, reifying explicit exceptions into the thread type
 * apply: ('a -(exn)-> 'b t) -> ('a -(n)-> 'b t)
 * semantically a natural transformation TE -> T, where T is the thread
 * monad, which is layered over exception monad E.
 *)
let apply f x = try f x with e -> make (Fail e)

(****)

let return v = make (Return v)
let fail e = make (Fail e)

let wait () = make Sleep
let wakeup t v = restart t (Return v) "wakeup"
let wakeup_exn t e = restart t (Fail e) "wakeup_exn"

let rec bind x f =
  match (repr x).state with
    Return v ->
      (* we don't use apply here so that tail recursion is not broken *)
      f v
  | Fail e ->
      fail e
  | Sleep ->
      let res = wait () in
      add_waiter x (fun x -> connect res (bind x (apply f)));
      res
  | Repr _ ->
      assert false
let (>>=) = bind

let rec catch_rec x f =
  match (repr x).state with
    Return v ->
      x
  | Fail e ->
      f e
  | Sleep ->
      let res = wait () in
      add_waiter x (fun x -> connect res (catch_rec x (apply f)));
      res
  | Repr _ ->
      assert false

let catch x f = catch_rec (apply x ()) f

let rec try_bind_rec x f g =
  match (repr x).state with
    Return v ->
      f v
  | Fail e ->
      apply g e
  | Sleep ->
      let res = wait () in
      add_waiter x (fun x -> connect res (try_bind_rec x (apply f) g));
      res
  | Repr _ ->
      assert false

let try_bind x f = try_bind_rec (apply x ()) f

let poll x =
  match (repr x).state with
    Fail e   -> raise e
  | Return v -> Some v
  | Sleep    -> None
  | Repr _   -> assert false

let rec ignore_result x =
  match (repr x).state with
    Return v ->
      ()
  | Fail e ->
      raise e
  | Sleep ->
      add_waiter x (fun x -> ignore_result x)
  | Repr _ ->
      assert false

let rec nth_ready l n =
  match l with
    [] ->
      assert false
  | x :: rem ->
      let x = repr x in
      if x.state = Sleep then
        nth_ready rem n
      else if n > 0 then
        nth_ready rem (n - 1)
      else
        x

let choose l =
  let ready = ref 0 in
  List.iter (fun x -> if (repr x).state <> Sleep then incr ready) l;
  if !ready > 0 then
    nth_ready l (Random.int !ready)
  else
    let res = wait () in
    (* XXX We may leak memory here, if we repeatedly select the same event *)
    List.iter (fun x -> try_connect res x) l;
    res

let finalize f g =
  try_bind f
    (fun x -> g () >>= fun () -> return x)
    (fun e -> g () >>= fun () -> fail e)
