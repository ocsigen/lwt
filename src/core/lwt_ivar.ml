(*****************************************************************************)
(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_ivar
 ******************************************************************************
 * Copyright (c) 2016, Richard M Neswold, Jr
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY METAWEB TECHNOLOGIES ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL METAWEB TECHNOLOGIES BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *****************************************************************************)

exeception Already_set

type 'a t = { mutable contents : 'a option;
              readers : 'a Lwt.u Lwt_sequence.t }

let create () = { contents = None;
                  readers = Lwt_sequence.create () }

let put ivar v =
  match ivar.contents with
  | None ->
     begin
       ivar.contents <- Some v;
       Lwt_sequence.iter_l (fun w -> Lwt.wakeup_later w v) ivar.readers;
       Lwt.return_unit
     end
  | Some _ ->
     Lwt.fail Already_determined

let get ivar =
  match ivar.contents with
  | Some v ->
     Lwt.return v
  | None ->
     Lwt.add_task_r ivar.readers
