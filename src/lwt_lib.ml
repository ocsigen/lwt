(* Ocsigen
 * http://www.ocsigen.org
 * lwt_lib.ml Copyright (C) 2007 Pierre Clairambault
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Weak
open Unix
open Lwt

let switch_time = 30.

exception Not_in_table
(* We use a specific Not_in_table exception, because since we're caching
 * threads, we can't for the moment behave differently whether a request
 * is not found in the table or not found as a host.*)


module WeakHashtbl = Make(
  struct
    type t = string*(Unix.host_entry Lwt.t)*float
    let equal = (fun (a,b,c) -> fun (a',b',c') -> a=a')
    let hash = fun (a,b,c) -> Hashtbl.hash a
  end
)

open WeakHashtbl

let keeper : (((string*(Unix.host_entry Lwt.t)*float) list) *
                ((string*(Unix.host_entry Lwt.t)*float) list)) ref = ref ([],[])
let cache = create 0
let dummy_addr : Unix.host_entry =
  {h_name="dummy";
   h_aliases=[||];
   h_addrtype=Unix.PF_INET;
   h_addr_list = [||]}

let cache_find d = try
                        match (find cache (d,return dummy_addr,0.)) with (_,h,t) -> (h,t)
                   with
                       |Not_found -> raise Not_in_table
                       |e -> raise e

let gethostbyname d =
  Lwt.catch
    (fun _ ->
       let (h,t) = cache_find d
       and t' = Unix.time () in
       match (t'>t+.60.) with
         | true ->
             (remove cache) (d,h,t);
             Lwt.fail Not_in_table
         | false -> h)
    (function
       | Not_in_table ->
           let t = Unix.time() and
               h = Lwt_preemptive.detach Unix.gethostbyname d in
           let entry =  (d,h,t) in
             add cache entry;
             (match !keeper with (a,b) -> keeper:= (entry::a,b));
             h
       | e -> fail e)
(* Begin getaddrinfo caching *)


module WeakAddrInfo = Make(
  struct
    type t = string*string*(Unix.getaddrinfo_option list)*((Unix.addr_info list) Lwt.t)*float
    let equal = (fun (h,s,o,i,t) -> fun (h',s',o',i',t') -> (h,s,o)=(h',s',o'))
    let hash = fun (h,s,o,i,t) -> Hashtbl.hash (h,s,o)
  end
)

let keeper6 : (((string*string*(Unix.getaddrinfo_option list)*((Unix.addr_info list) Lwt.t)*float) list) *
             ((string*string*(Unix.getaddrinfo_option list)*((Unix.addr_info list) Lwt.t)*float) list)) ref = ref
             ([],[])

let switch_thread : unit Lwt.t=
  let rec switch_worker () =
    Lwt_unix.sleep switch_time >>= fun () ->
    (match !keeper with (a,b) -> keeper:=([],a));
    (match !keeper6 with (a,b) -> keeper6:=([],a));
    switch_worker ()
  in
  switch_worker()

let cache6 = WeakAddrInfo.create 0

let cache_find6 d s o = try
                                match (WeakAddrInfo.find cache6 (d,s,o,return [],0.)) with (_,_,_,i,t) -> (i,t)
                        with
                                |Not_found -> raise Not_in_table
                                |e -> raise e


let getaddrinfo d s o =
  Lwt.catch
    (fun _ ->
       let (i,t) = cache_find6 d s o
       and t' = Unix.time() in
       match (t'>t+.60.) with
         | true ->
             WeakAddrInfo.remove cache6 (d,s,o,i,t);
             Lwt.fail Not_in_table
         | false -> i)
    (function
         | Not_in_table ->
             let t = Unix.time () and
                 i = Lwt_preemptive.detach (Unix.getaddrinfo d s) o in
             let entry = (d,s,o,i,t) in
               WeakAddrInfo.add cache6 entry;
               (match !keeper6 with (a,b) -> keeper6 := (entry::a,b));
               i
         | e -> fail e)


let getnameinfo s l =
  (*VVV à implémenter !!! *)
  Lwt_preemptive.detach (Unix.getnameinfo s) l
