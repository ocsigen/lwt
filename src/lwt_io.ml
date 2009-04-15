(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_io
 * Copyright (C) 2009 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

open Lwt

(* Minimum size for buffers: *)
let min_buffer_size = 16

let check_buffer_size fun_name buffer_size =
  if buffer_size < min_buffer_size then
    Printf.ksprintf invalid_arg "Lwt_io.%s: too small buffer size (%d)" fun_name buffer_size
  else if buffer_size > Sys.max_string_length then
    Printf.ksprintf invalid_arg "Lwt_io.%s: too big buffer size (%d)" fun_name buffer_size
  else
    ()

let default_buffer_size = ref 4096
let get_default_buffer_size _ = !default_buffer_size
let set_default_buffer_size size =
  check_buffer_size "set_default_buffer_size" size;
  default_buffer_size := size

let close_fd fd =
  try
    Lwt_unix.close fd;
    return ()
  with
      exn -> fail exn

(* +-------+
   | Types |
   +-------+ *)

type input
type output

type 'a mode = Input | Output

let input : input mode = Input
let output : output mode = Output

(* A channel state *)
type 'mode state =
  | Busy_primitive
      (* A primitive is running on the channel *)

  | Busy_atomic of 'mode channel
      (* An atomic operations is being performed on the channel. The
         argument is the temporary atomic wrapper. *)

  | Idle
      (* The channel is unused *)

  | Closed
      (* The channel has been closed *)

  | Invalid
      (* The channel is a temporary channel created for an atomic
         operation which has terminated. *)

(* A wrapper, which ensures that io operations are atomic: *)
and 'mode channel = {
  mutable state : 'mode state;

  channel : 'mode _channel;
  (* The real channel *)

  mutable queued : unit Lwt.t Queue.t;
  (* Queued operations *)
}

and 'mode _channel = {
  buffer : string;
  length : int;

  mutable ptr : int;
  (* Current position *)

  mutable max : int;
  (* Position of the end of data int the buffer. It is equal to
     [length] for output channels. *)

  abort : int Lwt.t;
  (* Thread which is wakeup with an exception when the channel is
     closed. *)

  auto_flush : bool;
  (* Is auto-flushing enabled ? *)

  mutable auto_flushing : bool;
  (* Wether the auto-flusher is currently running or not *)

  main : 'mode channel;
  (* The main wrapper *)

  perform_io : string -> int -> int -> int Lwt.t;
  (* Either a read or a write function *)

  close : unit Lwt.t Lazy.t;
  (* Close function *)

  mode : 'mode mode;
  (* The channel mode *)

  seek : int64 -> Unix.seek_command -> int64 Lwt.t;
  (* Random access *)

  mutable offset : int64;
  (* Number of bytes really read/written *)
}

type ic = input channel
type oc = output channel

let mode wrapper = wrapper.channel.mode

external cast : 'a channel -> unit channel = "%identity"

let cast_ic wrapper =
  if wrapper.channel.mode = Input then
    (Obj.magic (wrapper : 'a channel) : input channel)
  else
    invalid_arg "Lwt_io.cast_ic"

let cast_oc wrapper =
  if wrapper.channel.mode = Output then
    (Obj.magic (wrapper : 'a channel) : output channel)
  else
    invalid_arg "Lwt_io.cast_oc"

(* +----------------------------------+
   | Creations, closing, locking, ... |
   +----------------------------------+ *)

let get_pos wrapper =
  let ch = wrapper.channel in
  match ch.mode with
    | Input ->
        Int64.sub ch.offset (Int64.of_int (ch.max - ch.ptr))
    | Output ->
        Int64.add ch.offset (Int64.of_int ch.ptr)

let name ch = match ch.mode with
  | Input -> "input"
  | Output -> "output"

let closed_channel ch = Sys_error(Printf.sprintf "closed %s channel" (name ch))
let invalid_channel ch = Failure(Printf.sprintf "temporary atomic %s channel no more valid" (name ch))

(* Flush/refill the buffer. No race condition could happen because
   this function is always called atomically: *)
let perform_io ch = match ch.main.state with
  | Busy_primitive | Busy_atomic _ ->
      let ptr, len = match ch.mode with
        | Input ->
            (* Size of data in the buffer *)
            let size = ch.max - ch.ptr in
            (* If there are still data in the buffer, keep them: *)
            if size > 0 then String.unsafe_blit ch.buffer ch.ptr ch.buffer 0 size;
            (* Update positions: *)
            ch.ptr <- 0;
            ch.max <- size;
            (size, ch.length - size)
        | Output ->
            (0, ch.ptr) in
      (perform
         n <-- choose [ch.abort; ch.perform_io ch.buffer ptr len];
         (* Never trust user functions... *)
         if n < 0 || n > len then
           fail (Failure (Printf.sprintf "Lwt_io: invalid result of the [%s] function(request=%d,result=%d)"
                            (match ch.mode with Input -> "read" | Output -> "write") len n))
         else begin
           (* Update the global offset: *)
           ch.offset <- Int64.add ch.offset (Int64.of_int n);
           (* Update buffer positions: *)
           begin match ch.mode with
             | Input ->
                 ch.max <- ch.max + n
             | Output ->
                 (* Shift remaining data: *)
                 let len = len - n in
                 String.unsafe_blit ch.buffer ptr ch.buffer 0 len;
                 ch.ptr <- len
           end;
           return n
         end)

  | Closed ->
      fail (closed_channel ch)

  | Invalid ->
      fail (invalid_channel ch)

  | Idle ->
      assert false

let refill = perform_io
let flush_partial = perform_io

let rec flush_total oc =
  if oc.ptr > 0 then
    (perform
       flush_partial oc;
       flush_total oc)
  else
    return ()

let safe_flush_total oc =
  catch
    (fun _ -> flush_total oc)
    (fun _ -> return ())

let rec auto_flush oc =
  let wrapper = oc.main in
  Lwt_unix.yield () >>= fun _ -> match wrapper.state with
    | Busy_primitive | Busy_atomic _ ->
        (* The channel is used, wait again *)
        auto_flush oc

    | Idle ->
        if Queue.is_empty oc.main.queued then begin
          oc.auto_flushing <- false;
          wrapper.state <- Busy_primitive;
          safe_flush_total oc >>= fun _ ->
            if wrapper.state = Busy_primitive then
              wrapper.state <- Idle;
            if not (Queue.is_empty wrapper.queued) then
              wakeup (Queue.pop wrapper.queued) ();
            return ()
        end else
          (* Some operations are queued, wait again: *)
          auto_flush oc

    | Closed ->
        fail (closed_channel oc)

    | Invalid ->
        fail (invalid_channel oc)

(* A ``locked'' channel is a channel in the state [Busy_primitive] or
   [Busy_atomic] *)

let unlock wrapper = match wrapper.state with
  | Busy_primitive | Busy_atomic _ ->
      wrapper.state <- Idle;
      if not (Queue.is_empty wrapper.queued) then
        wakeup (Queue.pop wrapper.queued) ();
      (* Launches the auto-flusher: *)
      let ch = wrapper.channel in
      if (ch.mode = Output &&
          ch.auto_flush &&
          (* Launch the auto-flusher only for the main wrapper: *)
          ch.main == wrapper &&
          (* Do not launch two auto-flusher: *)
          not ch.auto_flushing &&
          (* Do not launch the auto-flusher if operations are queued: *)
          Queue.is_empty wrapper.queued) then begin
        ch.auto_flushing <- true;
        ignore (auto_flush ch)
      end

  | Closed | Invalid ->
      (* Do not change channel state if the channel has been closed *)
      if not (Queue.is_empty wrapper.queued) then
        wakeup (Queue.pop wrapper.queued) ()

  | Idle ->
      (* We must never unlock an unlocked channel *)
      assert false

(* Wrap primitives into atomic io operations: *)
let primitive f wrapper = match wrapper.state with
  | Idle ->
      wrapper.state <- Busy_primitive;
      finalize (fun _ -> f wrapper.channel)
        (fun _ ->
           unlock wrapper;
           return ())

  | Busy_primitive | Busy_atomic _ ->
      let w = wait () in
      Queue.push w wrapper.queued;
      (perform
         w;
         match wrapper.state with
           | Closed ->
               (* The channel has been closed while we were waiting *)
               unlock wrapper;
               fail (closed_channel wrapper.channel)

           | Idle ->
               wrapper.state <- Busy_primitive;
               finalize (fun _ -> f wrapper.channel)
                 (fun _ ->
                    unlock wrapper;
                    return ())

           | Invalid ->
               fail (invalid_channel wrapper.channel)

           | Busy_primitive | Busy_atomic _ ->
               assert false)

  | Closed ->
      fail (closed_channel wrapper.channel)

  | Invalid ->
      fail (invalid_channel wrapper.channel)

(* Wrap a sequence of io operations into an atomic operation: *)
let atomic f wrapper = match wrapper.state with
  | Idle ->
      let tmp_wrapper = { state = Idle;
                          channel = wrapper.channel;
                          queued = Queue.create () } in
      wrapper.state <- Busy_atomic tmp_wrapper;
      finalize (fun _ -> f tmp_wrapper)
          (fun _ ->
           (* The temporary wrapper is no more valid: *)
           tmp_wrapper.state <- Invalid;
           unlock wrapper;
           return ())

  | Busy_primitive | Busy_atomic _ ->
      let w = wait () in
      Queue.push w wrapper.queued;
      (perform
         w;
         match wrapper.state with
           | Closed ->
               (* The channel has been closed while we were waiting *)
               unlock wrapper;
               fail (closed_channel wrapper.channel)

           | Idle ->
               let tmp_wrapper = { state = Idle;
                                   channel = wrapper.channel;
                                   queued = Queue.create () } in
               wrapper.state <- Busy_atomic tmp_wrapper;
               finalize (fun _ -> f tmp_wrapper)
                 (fun _ ->
                    tmp_wrapper.state <- Invalid;
                    unlock wrapper;
                    return ())

           | Invalid ->
               fail (invalid_channel wrapper.channel)

           | Busy_primitive | Busy_atomic _ ->
               assert false)

  | Closed ->
      fail (closed_channel wrapper.channel)

  | Invalid ->
      fail (invalid_channel wrapper.channel)

let rec abort wrapper = match wrapper.state with
  | Busy_atomic tmp_wrapper ->
      (* Close the depest opened wrapper: *)
      abort tmp_wrapper
  | Closed ->
      (* Double close, just returns the same thing as before *)
      Lazy.force wrapper.channel.close
  | Invalid ->
      fail (invalid_channel wrapper.channel)
  | Idle | Busy_primitive ->
      wrapper.state <- Closed;
      (* Abort any current real reading/writing operation on the
         channel: *)
      wakeup_exn wrapper.channel.abort (closed_channel wrapper.channel);
      Lazy.force wrapper.channel.close

let close wrapper = match wrapper.channel.mode with
  | Input ->
      (* Just close it now: *)
      abort wrapper
  | Output ->
      (* Performs all pending action, flush the buffer, then close
         it: *)
      catch
        (fun _ ->
           primitive
             (fun ch ->
                perform
                  if ch.mode = Output then
                    safe_flush_total wrapper.channel
                  else
                    return ();
                  abort wrapper)
             wrapper)
        (fun _ ->
           abort wrapper)

(* Avoid confusion with the [close] argument of [make]: *)
let alias_to_close = close

let no_seek pos cmd =
  fail (Failure "Lwt_io.seek: seek not supported on this channel")

let make ?(auto_flush=true) ?buffer_size ?(close=return) ?(seek=no_seek) ~mode perform_io =
  let buffer =
    String.create (match buffer_size with
                     | None ->
                         !default_buffer_size
                     | Some size ->
                         check_buffer_size "Lwt_io.make" size;
                         size)
  in
  let rec ch = {
    buffer = buffer;
    length = String.length buffer;
    ptr = 0;
    max = (match mode with
             | Input -> 0
             | Output -> String.length buffer);
    perform_io = perform_io;
    close = Lazy.lazy_from_fun (fun _ -> try close () with e -> fail e);
    abort = wait ();
    main = wrapper;
    auto_flush = auto_flush;
    auto_flushing = false;
    mode = mode;
    seek = (fun pos cmd -> try seek pos cmd with e -> fail e);
    offset = 0L;
  } and wrapper = {
    state = Idle;
    channel = ch;
    queued = Queue.create ();
  } in
  Lwt_gc.finalise_or_exit alias_to_close wrapper;
  wrapper

let of_fd ?buffer_size ~mode fd =
  make
    ?buffer_size
    ~close:(fun _ -> close_fd fd)
    ~seek:(fun pos cmd -> return (Unix.LargeFile.lseek (Lwt_unix.unix_file_descr fd) pos cmd))
    ~mode
    (match mode with
       | Input -> Lwt_unix.read fd
       | Output -> Lwt_unix.write fd)

let of_unix_fd ?buffer_size ~mode fd =
  of_fd ?buffer_size ~mode (Lwt_unix.of_unix_file_descr fd)

(* +------------+
   | Byte-order |
   +------------+ *)

module Byte_order =
struct
  module type S = sig
    val pos16_0 : int
    val pos16_1 : int
    val pos32_0 : int
    val pos32_1 : int
    val pos32_2 : int
    val pos32_3 : int
    val pos64_0 : int
    val pos64_1 : int
    val pos64_2 : int
    val pos64_3 : int
    val pos64_4 : int
    val pos64_5 : int
    val pos64_6 : int
    val pos64_7 : int
  end

  module LE =
  struct
    let pos16_0 = 0
    let pos16_1 = 1
    let pos32_0 = 0
    let pos32_1 = 1
    let pos32_2 = 2
    let pos32_3 = 3
    let pos64_0 = 0
    let pos64_1 = 1
    let pos64_2 = 2
    let pos64_3 = 3
    let pos64_4 = 4
    let pos64_5 = 5
    let pos64_6 = 6
    let pos64_7 = 7
  end

  module BE =
  struct
    let pos16_0 = 1
    let pos16_1 = 0
    let pos32_0 = 3
    let pos32_1 = 2
    let pos32_2 = 1
    let pos32_3 = 0
    let pos64_0 = 7
    let pos64_1 = 6
    let pos64_2 = 5
    let pos64_3 = 4
    let pos64_4 = 3
    let pos64_5 = 2
    let pos64_6 = 1
    let pos64_7 = 0
  end
end

module Primitives =
struct

  (* This module contains all primitives operations. The operates
     without protection regarding locking, they are wrapped after into
     safe operations. *)

  (* +--------+
     | Inputs |
     +--------+ *)

  let rec get_char ic =
    let ptr = ic.ptr in
    if ptr = ic.max then
      refill ic >>= function
        | 0 -> fail End_of_file
        | _ -> get_char ic
    else begin
      ic.ptr <- ptr + 1;
      return (String.unsafe_get ic.buffer ptr)
    end

  let get_byte ic = get_char ic >>= fun ch -> return (Char.code ch)

  let peek_char ic =
    catch
      (fun _ ->
         get_char ic >>= fun ch -> return (Some ch))
      (function
         | End_of_file ->
             return None
         | exn ->
             fail exn)

  let unsafe_get ic str ofs len =
    let avail = ic.max - ic.ptr in
    if avail > 0 then begin
      let len = min len avail in
      String.unsafe_blit ic.buffer ic.ptr str ofs len;
      ic.ptr <- ic.ptr + len;
      return len
    end else begin
      refill ic >>= fun n ->
        let len = min len n in
        String.unsafe_blit ic.buffer 0 str ofs len;
        ic.ptr <- len;
        ic.max <- n;
        return len
    end

  let get ic str ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length str then
      fail (Invalid_argument (Printf.sprintf
                                "Lwt_io.get(ofs=%d,len=%d,str_len=%d)"
                                ofs len (String.length str)))
    else begin
      if len = 0 then
        return 0
      else
        unsafe_get ic str ofs len
    end

  let rec unsafe_get_exactly ic str ofs len =
    unsafe_get ic str ofs len >>= function
      | 0 ->
          fail End_of_file
      | n ->
          let len = len - n in
          if len = 0 then
            return ()
          else
            unsafe_get_exactly ic str (ofs + n) len

  let get_exactly ic str ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length str then
      fail (Invalid_argument (Printf.sprintf
                                "Lwt_io.get_exactly(ofs=%d,len=%d,str_len=%d)"
                                ofs len (String.length str)))
    else begin
      if len = 0 then
        return ()
      else
        unsafe_get_exactly ic str ofs len
    end

  let get_string ic len =
    let str = String.create len in
    unsafe_get_exactly ic str 0 len >>= fun _ -> return str

  let rec add_line_rec ic cr_read buf =
    peek_char ic >>= function
      | None ->
          if cr_read then Buffer.add_char buf '\r';
          return true
      | Some '\n' ->
          return false
      | Some '\r' ->
          if cr_read then Buffer.add_char buf '\r';
          add_line_rec ic true buf
      | Some ch ->
          if cr_read then Buffer.add_char buf '\r';
          Buffer.add_char buf ch;
          add_line_rec ic false buf

  let add_line ic buf =
    add_line_rec ic false buf

  let peek_line ic =
    let buf = Buffer.create 128 in
    add_line ic buf >>= function
      | true ->
          return None
      | false ->
          return (Some (Buffer.contents buf))

  let get_line ic =
    let buf = Buffer.create 128 in
    add_line ic buf >>= function
      | true ->
          fail End_of_file
      | false ->
          return (Buffer.contents buf)

  let get_value ic =
    let header = String.create 20 in
    (perform
       unsafe_get_exactly ic header 0 20;
       let bsize = Marshal.data_size header 0 in
       let buffer = String.create (20 + bsize) in
       let _ = String.unsafe_blit header 0 buffer 0 20 in
       unsafe_get_exactly ic buffer 20 bsize;
       return (Marshal.from_string buffer 0))

  (* +---------+
     | Outputs |
     +---------+ *)

  let force_flush = flush_total

  let rec put_char oc ch =
    let ptr = oc.ptr in
    if ptr < oc.length then begin
      oc.ptr <- ptr + 1;
      String.unsafe_set oc.buffer ptr ch;
      return ()
    end else
      flush_partial oc >>= fun _ -> put_char oc ch

  let put_byte oc x = put_char oc (Char.unsafe_chr x)

  let rec unsafe_put oc str ofs len =
    let avail = oc.length - oc.ptr in
    if avail >= len then begin
      String.unsafe_blit str ofs oc.buffer oc.ptr len;
      oc.ptr <- oc.ptr + len;
      return 0
    end else begin
      String.unsafe_blit str ofs oc.buffer oc.ptr avail;
      oc.ptr <- oc.length;
      flush_partial oc >>= fun _ ->
        let len = len - avail in
        if oc.ptr = 0 then begin
          if len = 0 then
            return 0
          else
            (* Everything has been written, try to write more: *)
            unsafe_put oc str (ofs + avail) len
        end else
          (* Not everything has been written, just what is remaining: *)
          return len
    end

  let put oc str ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length str then
      fail (Invalid_argument (Printf.sprintf
                                "Lwt_io.put(ofs=%d,len=%d,str_len=%d)"
                                ofs len (String.length str)))
    else begin
      if len = 0 then
        return 0
      else
        unsafe_put oc str ofs len >>= fun remaining -> return (len - remaining)
    end

  let rec unsafe_put_exactly oc str ofs len =
    unsafe_put oc str ofs len >>= function
      | 0 ->
          return ()
      | n ->
          unsafe_put_exactly oc str (ofs + len - n) n

  let put_exactly oc str ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length str then
      fail (Invalid_argument (Printf.sprintf
                                "Lwt_io.put_exactly(ofs=%d,len=%d,str_len=%d)"
                                ofs len (String.length str)))
    else begin
      if len = 0 then
        return ()
      else
        unsafe_put_exactly oc str ofs len
    end

  let put_string oc str =
    unsafe_put_exactly oc str 0 (String.length str)

  let put_line oc str =
    put_string oc str >>= fun _ -> put_char oc '\n'

  let put_value oc ?(flags=[]) x =
    put_string oc (Marshal.to_string x flags)

  (* +------------------+
     | Low-level access |
     +------------------+ *)

  let rec get_block_unsafe ic size f =
    if ic.max - ic.ptr < size then
      refill ic >>= function
        | 0 ->
            fail End_of_file
        | _ ->
            get_block_unsafe ic size f
    else begin
      let ptr = ic.ptr in
      ic.ptr <- ptr + size;
      f ic.buffer ptr
    end

  let rec put_block_unsafe oc size f =
    if oc.max - oc.ptr < size then
      flush_partial oc >>= fun _ -> put_block_unsafe oc size f
    else begin
      let ptr = oc.ptr in
      oc.ptr <- ptr + size;
      f oc.buffer ptr
    end

  let block ch size f =
    if size < 0 || size > min_buffer_size then
      fail (Invalid_argument(Printf.sprintf "Lwt_io.block(size=%d)" size))
    else
      if ch.max - ch.ptr >= size then begin
        let ptr = ch.ptr in
        ch.ptr <- ptr + size;
        f ch.buffer ptr
      end else
        match ch.mode with
          | Input ->
              get_block_unsafe ch size f
          | Output ->
              put_block_unsafe ch size f

  let direct_access ch f =
    (perform
       (len, x) <-- (if ch.ptr < ch.max then
                       f ch.buffer ch.ptr (ch.max - ch.ptr)
                     else
                       (perform
                          perform_io ch;
                          f ch.buffer ch.ptr (ch.max - ch.ptr)));
       if len < 0 || len > ch.max - ch.ptr then
         fail (Failure(Printf.sprintf "Lwt_io.direct_access: invalid result of [f]: result = %d, ptr = %d, max = %d"
                         len ch.ptr ch.max))
       else begin
         ch.ptr <- ch.ptr + len;
         return x
       end)

  module Make_number_io(Byte_order : Byte_order.S) =
  struct
    open Byte_order

    (* +-----------------+
       | Reading numbers |
       +-----------------+ *)

    let get buffer ptr = Char.code (String.unsafe_get buffer ptr)

    let get_int ic =
      get_block_unsafe ic 4
        (fun buffer ptr ->
           let v0 = get buffer (ptr + pos32_0)
           and v1 = get buffer (ptr + pos32_1)
           and v2 = get buffer (ptr + pos32_2)
           and v3 = get buffer (ptr + pos32_3) in
           let v = v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24) in
           if v3 land 0x80 = 0 then
             return v
           else
             return (v - (1 lsl 32)))

    let get_int16 ic =
      get_block_unsafe ic 2
        (fun buffer ptr ->
           let v0 = get buffer (ptr + pos16_0)
           and v1 = get buffer (ptr + pos16_1) in
           let v = v0 lor (v1 lsl 8) in
           if v1 land 0x80 = 0 then
             return v
           else
             return (v - (1 lsl 16)))

    let get_int32 ic =
      get_block_unsafe ic 4
        (fun buffer ptr ->
           let v0 = get buffer (ptr + pos32_0)
           and v1 = get buffer (ptr + pos32_1)
           and v2 = get buffer (ptr + pos32_2)
           and v3 = get buffer (ptr + pos32_3) in
           return (Int32.logor
                     (Int32.logor
                        (Int32.of_int v0)
                        (Int32.shift_left (Int32.of_int v1) 8))
                     (Int32.logor
                        (Int32.shift_left (Int32.of_int v2) 16)
                        (Int32.shift_left (Int32.of_int v3) 24))))

    let get_int64 ic =
      get_block_unsafe ic 8
        (fun buffer ptr ->
           let v0 = get buffer (ptr + pos64_0)
           and v1 = get buffer (ptr + pos64_1)
           and v2 = get buffer (ptr + pos64_2)
           and v3 = get buffer (ptr + pos64_3)
           and v4 = get buffer (ptr + pos64_4)
           and v5 = get buffer (ptr + pos64_5)
           and v6 = get buffer (ptr + pos64_6)
           and v7 = get buffer (ptr + pos64_7) in
           return (Int64.logor
                     (Int64.logor
                        (Int64.logor
                           (Int64.of_int v0)
                           (Int64.shift_left (Int64.of_int v1) 8))
                        (Int64.logor
                           (Int64.shift_left (Int64.of_int v2) 16)
                           (Int64.shift_left (Int64.of_int v3) 24)))
                     (Int64.logor
                        (Int64.logor
                           (Int64.shift_left (Int64.of_int v4) 32)
                           (Int64.shift_left (Int64.of_int v5) 40))
                        (Int64.logor
                           (Int64.shift_left (Int64.of_int v6) 48)
                           (Int64.shift_left (Int64.of_int v7) 56)))))

    let get_float32 ic = get_int32 ic >>= fun x -> return (Int32.float_of_bits x)
    let get_float64 ic = get_int64 ic >>= fun x -> return (Int64.float_of_bits x)

    (* +-----------------+
       | Writing numbers |
       +-----------------+ *)

    let set buffer ptr x = String.unsafe_set buffer ptr (Char.unsafe_chr x)

    let put_int oc v =
      put_block_unsafe oc 4
        (fun buffer ptr ->
           set buffer (ptr + pos32_0) v;
           set buffer (ptr + pos32_1) (v lsr 8);
           set buffer (ptr + pos32_2) (v lsr 16);
           set buffer (ptr + pos32_3) (v asr 24);
           return ())

    let put_int16 oc v =
      put_block_unsafe oc 2
        (fun buffer ptr ->
           set buffer (ptr + pos16_0) v;
           set buffer (ptr + pos16_1) (v lsr 8);
           return ())

    let put_int32 oc v =
      put_block_unsafe oc 4
        (fun buffer ptr ->
           set buffer (ptr + pos32_0) (Int32.to_int v);
           set buffer (ptr + pos32_1) (Int32.to_int (Int32.shift_right v 8));
           set buffer (ptr + pos32_2) (Int32.to_int (Int32.shift_right v 16));
           set buffer (ptr + pos32_3) (Int32.to_int (Int32.shift_right v 24));
           return ())

    let put_int64 oc v =
      put_block_unsafe oc 8
        (fun buffer ptr ->
           set buffer (ptr + pos64_0) (Int64.to_int v);
           set buffer (ptr + pos64_1) (Int64.to_int (Int64.shift_right v 8));
           set buffer (ptr + pos64_2) (Int64.to_int (Int64.shift_right v 16));
           set buffer (ptr + pos64_3) (Int64.to_int (Int64.shift_right v 24));
           set buffer (ptr + pos64_4) (Int64.to_int (Int64.shift_right v 32));
           set buffer (ptr + pos64_5) (Int64.to_int (Int64.shift_right v 40));
           set buffer (ptr + pos64_6) (Int64.to_int (Int64.shift_right v 48));
           set buffer (ptr + pos64_7) (Int64.to_int (Int64.shift_right v 56));
           return ())

    let put_float32 oc v = put_int32 oc (Int32.bits_of_float v)
    let put_float64 oc v = put_int64 oc (Int64.bits_of_float v)
  end

  (* +---------------+
     | Random access |
     +---------------+ *)

  let seek ch pos =
    (perform
       offset <-- ch.seek pos Unix.SEEK_SET;
       if offset <> pos then
         fail (Sys_error "Lwt_io.set_pos: seek failed")
       else
         return ())

  let set_pos ch pos = match ch.mode with
    | Output ->
        (perform
           flush_total ch;
           seek ch pos;
           let _ = ch.offset <- pos in
           return ())
    | Input ->
        let current = Int64.sub ch.offset (Int64.of_int (ch.max - ch.ptr)) in
        if pos >= current && pos <= ch.offset then begin
          ch.ptr <- ch.max - (Int64.to_int (Int64.sub ch.offset pos));
          return ()
        end else
          (perform
             seek ch pos;
             let _ =
               ch.offset <- pos;
               ch.ptr <- 0;
               ch.max <- 0
             in
             return ())

  let length ch =
    (perform
       len <-- ch.seek 0L Unix.SEEK_END;
       seek ch ch.offset;
       return len)
end

(* +----------------------+
   | Primitive operations |
   +----------------------+ *)

let get_char ic = primitive Primitives.get_char ic
let get_byte ic = primitive Primitives.get_byte ic
let peek_char ic = primitive Primitives.peek_char ic
let get_string ic len = primitive (fun ic -> Primitives.get_string ic len) ic
let get_line ic = primitive Primitives.get_line ic
let peek_line ic = primitive Primitives.peek_line ic
let add_line ic buf = primitive (fun ic -> Primitives.add_line ic buf) ic
let get ic str ofs len = primitive (fun ic -> Primitives.get ic str ofs len) ic
let get_exactly ic str ofs len = primitive (fun ic -> Primitives.get_exactly ic str ofs len) ic
let get_value ic = primitive Primitives.get_value ic

let force_flush oc = primitive Primitives.force_flush oc
let flush oc =
  if oc.channel.auto_flush then
    return ()
  else
    force_flush oc

let put_char oc ch = primitive (fun oc -> Primitives.put_char oc ch) oc
let put_byte oc x = primitive (fun oc -> Primitives.put_byte oc x) oc
let put_string oc str = primitive (fun oc -> Primitives.put_string oc str) oc
let put_line oc str = primitive (fun oc -> Primitives.put_line oc str) oc
let put oc str ofs len = primitive (fun oc -> Primitives.put oc str ofs len) oc
let put_exactly oc str ofs len = primitive (fun oc -> Primitives.put_exactly oc str ofs len) oc
let put_value oc ?flags x = primitive (fun oc -> Primitives.put_value oc ?flags x) oc

let block ch size f = primitive (fun ch -> Primitives.block ch size f) ch
let direct_access ch f = primitive (fun ch -> Primitives.direct_access ch f) ch

let set_pos ch pos = primitive (fun ch -> Primitives.set_pos ch pos) ch
let length ch = primitive Primitives.length ch

module type Number_io = sig
  val get_int : ic -> int Lwt.t
  val get_int16 : ic -> int Lwt.t
  val get_int32 : ic -> int32 Lwt.t
  val get_int64 : ic -> int64 Lwt.t
  val get_float32 : ic -> float Lwt.t
  val get_float64 : ic -> float Lwt.t
  val put_int : oc -> int -> unit Lwt.t
  val put_int16 : oc -> int -> unit Lwt.t
  val put_int32 : oc -> int32 -> unit Lwt.t
  val put_int64 : oc -> int64 -> unit Lwt.t
  val put_float32 : oc -> float -> unit Lwt.t
  val put_float64 : oc -> float -> unit Lwt.t
end

module Make_number_io(Byte_order : Byte_order.S) =
struct
  module Primitives = Primitives.Make_number_io(Byte_order)

  let get_int ic = primitive Primitives.get_int ic
  let get_int16 ic = primitive Primitives.get_int16 ic
  let get_int32 ic = primitive Primitives.get_int32 ic
  let get_int64 ic = primitive Primitives.get_int64 ic
  let get_float32 ic = primitive Primitives.get_float32 ic
  let get_float64 ic = primitive Primitives.get_float64 ic

  let put_int oc x = primitive (fun oc -> Primitives.put_int oc x) oc
  let put_int16 oc x = primitive (fun oc -> Primitives.put_int16 oc x) oc
  let put_int32 oc x = primitive (fun oc -> Primitives.put_int32 oc x) oc
  let put_int64 oc x = primitive (fun oc -> Primitives.put_int64 oc x) oc
  let put_float32 oc x = primitive (fun oc -> Primitives.put_float32 oc x) oc
  let put_float64 oc x = primitive (fun oc -> Primitives.put_float64 oc x) oc
end

module LE = Make_number_io(Byte_order.LE)
module BE = Make_number_io(Byte_order.BE)

(* +-------+
   | Other |
   +-------+ *)

let zero =
  make
    ~mode:input
    ~buffer_size:min_buffer_size
    (fun str ofs len -> String.fill str ofs len '\x00'; return len)

let null =
  make
    ~mode:output
    ~buffer_size:min_buffer_size
    (fun str ofs len -> return len)

let of_fd_no_close ~mode fd =
  make
    ~seek:(fun pos cmd -> return (Unix.LargeFile.lseek (Lwt_unix.unix_file_descr (Lazy.force fd)) pos cmd))
    ~mode (match mode with
             | Input ->
                 (fun buf ofs len -> Lwt_unix.read (Lazy.force fd) buf ofs len)
             | Output ->
                 (fun buf ofs len -> Lwt_unix.write (Lazy.force fd) buf ofs len))

(* Do not close standard ios on close, otherwise uncaught exceptions
   will not be printed *)
let stdin = of_fd_no_close input Lwt_unix.stdin
let stdout = of_fd_no_close output Lwt_unix.stdout
let stderr = of_fd_no_close output Lwt_unix.stderr

let pipe ?buffer_size _ =
  let fd_r, fd_w = Lwt_unix.pipe () in
  (of_fd ?buffer_size ~mode:input fd_r, of_fd ?buffer_size ~mode:output fd_w)

let open_file ?buffer_size ?flags ?perm ~mode filename =
  let flags = match flags, mode with
    | Some l, _ ->
        l
    | None, Input ->
        [Unix.O_RDONLY; Unix.O_NONBLOCK]
    | None, Output ->
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK]
  and perm = match perm, mode with
    | Some p, _ ->
        p
    | None, Input ->
        0
    | None, Output ->
        0o666
  in
  catch
    (fun _ -> return (of_unix_fd ?buffer_size ~mode (Unix.openfile filename flags perm)))
    fail

let with_file ?buffer_size ?flags ?perm ~mode filename f =
  (perform
     ic <-- open_file ?buffer_size ?flags ?perm ~mode filename;
     finalize
       (fun _ -> f ic)
       (fun _ -> close ic))

let file_length filename = with_file ~mode:input filename length

let open_connection ?buffer_size sockaddr =
  let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  let ref_count = ref 2 in
  let close mode =
    Lwt_unix.shutdown fd mode;
    decr ref_count;
    if !ref_count = 0 then
      close_fd fd
    else
      return ()
  in
  Lwt.catch
    (fun _ ->
       perform
         Lwt_unix.connect fd sockaddr;
         let _ = try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> () in
         return (make ?buffer_size
                   ~close:(fun _ -> close Unix.SHUTDOWN_RECEIVE)
                   ~mode:input (Lwt_unix.read fd),
                 make ?buffer_size
                   ~close:(fun _ -> close Unix.SHUTDOWN_SEND)
                   ~mode:output (Lwt_unix.write fd)))
    (fun exn ->
       perform
         close_fd fd;
         fail exn)

let with_connection ?buffer_size sockaddr f =
  (perform
     (ic, oc) <-- open_connection sockaddr;
     finalize
       (fun _ -> f (ic, oc))
       (fun _ ->
          perform
            close ic;
            close oc))
