
# Module `Lwt_condition`

Conditions

Condition variables to synchronize between threads.

```ocaml
type 'a t
```
Condition variable type. The type parameter denotes the type of value propagated from notifier to waiter.

```ocaml
val create : unit -> 'a t
```
`create ()` creates a new condition variable.

```ocaml
val wait : ?mutex:Lwt_mutex.t -> 'a t -> 'a Lwt.t
```
`wait mutex condvar` will cause the current thread to block, awaiting notification for a condition variable, `condvar`. If provided, the `mutex` must have been previously locked (within the scope of `Lwt_mutex.with_lock`, for example) and is temporarily unlocked until the condition is notified. Upon notification, `mutex` is re-locked before `wait` returns and the thread's activity is resumed. When the awaited condition is notified, the value parameter passed to `signal` is returned.

```ocaml
val signal : 'a t -> 'a -> unit
```
`signal condvar value` notifies that a condition is ready. A single waiting thread will be awoken and will receive the notification value which will be returned from `wait`. Note that condition notification is not "sticky", i.e. if there is no waiter when `signal` is called, the notification will be missed and the value discarded.

```ocaml
val broadcast : 'a t -> 'a -> unit
```
`broadcast condvar value` notifies all waiting threads. Each will be awoken in turn and will receive the same notification value.

```ocaml
val broadcast_exn : 'a t -> exn -> unit
```
`broadcast_exn condvar exn` fails all waiting threads with exception `exn`.

since 2\.6.0