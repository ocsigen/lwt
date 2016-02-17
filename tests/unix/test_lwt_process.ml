
open Lwt
open Lwt_io
open Test

let suite = suite "lwt_process" [
  test "lazy_undefined"
    (fun () ->
      Lwt_process.with_process_in
        ~timeout:1. ("sleep", [| "sleep"; "2" |]) 
          (fun p ->
            Lwt.catch
              (fun () -> Lwt_io.read p#stdout)
              (fun _ -> Lwt.return ""))
        >>= fun _ -> Lwt.return_true)
]
