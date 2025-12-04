(** This is the runtime-events library for lwt. It contains lwt-specific
    definition of events (so that programs can be written to consume the events
    lwt produces) and the function to emit them (so that lwt can emit them). This
    library is not intended for emitting events by hand. *)

(** Counter event indicatiing the number of paused promise, i.e., the number of
    calls to [Lwt.pause] which have not yet been resolved. *)
type Runtime_events.User.tag += Paused_count
val paused_count : int Runtime_events.User.t
val emit_paused_count : int -> unit

(** Span event indicatiing that [Lwt_main.run] has been called and hasn't
    returned yet, i.e., when the Lwt scheduler is running. *)
type Runtime_events.User.tag += Scheduler_call
val sch_call : Runtime_events.Type.span Runtime_events.User.t
val emit_sch_call_begin : unit -> unit
val emit_sch_call_end : unit -> unit

(** Punctual event indicatiing that the scheduler is performing one loop. *)
type Runtime_events.User.tag += Scheduler_lap
val sch_lap : unit Runtime_events.User.t
val emit_sch_lap : unit -> unit

(** Counter event indicatiing the number of unix I/O jobs, i.e., the number of
    calls to [Lwt_unix.*] which have not yet been resolved. *)
type Runtime_events.User.tag += Unix_job_count
val unix_job_count : int Runtime_events.User.t
val emit_job_count : int -> unit

module Trace : sig
  type t = { kind: Runtime_events.Type.span; context: string option; filename: string; line: int; }
  val t : t Runtime_events.Type.t

  (** Span event indicatiing that a promise is taking time to resolve. The
      event indicates the location of the bind for the promise. These events are
      automatically emitted by code produced by the lwt_ppx syntax extension. *)
  type Runtime_events.User.tag += T
  val span : t Runtime_events.User.t
  val emit : t -> unit
end
