open Test
open Lwt_direct
open Lwt.Syntax

let main_tests = suite "main" [
  test "basic await" begin fun () ->
    let fut = spawn @@ fun () ->
      Lwt_unix.sleep 1e-6 |> await;
      42
    in
    let+ res = fut in
    res = 42
  end;

  test "await multiple values" begin fun () ->
    let fut1 = let+ () = Lwt_unix.sleep 1e-6 in 1 in
    let fut2 = let+ () = Lwt_unix.sleep 2e-6 in 2 in
    let fut3 = let+ () = Lwt_unix.sleep 3e-6 in 3 in

    spawn @@ fun () ->
      let x1 = fut1 |> await in
      let x2 = fut2 |> await in
      let x3 = fut3 |> await in
      x1 = 1 && x2 = 2 && x3 = 3
  end;

  test "list.iter await" begin fun () ->
    let items = List.init 101 (fun i -> Lwt.return i) in
    spawn @@ fun () ->
      let sum = ref 0 in
      List.iter (fun fut -> sum := !sum + await fut) items;
      !sum = 5050
  end;

  test "lwt_list.iter_p spawn" begin fun () ->
    let items = List.init 101 (fun i -> i) in
    let+ items = Lwt_list.map_p
      (fun i -> spawn (fun () ->
        for _ = 0 to i mod 5 do yield () done;
        i
      ))
      items
    in
    List.fold_left (+) 0 items = 5050
  end;

  test "spawn in background" begin fun () ->
    let stream, push = Lwt_stream.create_bounded 2 in
    spawn_in_the_background (fun () ->
      for i = 1 to 10 do
        push#push i |> await
      done;
      push#close);
    spawn @@ fun () ->
      let continue = ref true in
      let seen = ref [] in

      while !continue do
        match Lwt_stream.get stream |> await with
        | None -> continue := false
        | Some x -> seen := x :: !seen
      done;
      List.rev !seen = [1;2;3;4;5;6;7;8;9;10]
  end;

  test "list.iter await with yield" begin fun () ->
    let items = List.init 101 (fun i -> Lwt.return i) in
    spawn @@ fun () ->
      let sum = ref 0 in
      List.iter (fun fut -> yield(); sum := !sum + await fut) items;
      !sum = 5050
  end;

  test "awaiting on failing promise" begin fun () ->
    let fut: unit Lwt.t = let* () = Lwt.pause () in let* () = Lwt_unix.sleep 0.0001 in Lwt.fail Exit in
    spawn @@ fun () ->
      try await fut; false
      with Exit -> true
  end;

  test "spawn can fail" begin fun () ->
    spawn @@ fun () ->
      let sub: unit Lwt.t = spawn @@ fun () ->
        Lwt_unix.sleep 0.00001 |> await;
        raise Exit
      in
      try await sub; false
      with Exit -> true
  end;

  test "concurrent fib" begin fun () ->
    let rec badfib n =
      if n <= 2 then Lwt.return 1
      else
        spawn begin fun () ->
          let f1 = badfib (n-1) in
          let f2 = badfib (n-2) in
          await f1 + await f2
        end
    in
    spawn @@ fun () ->
      let fib12 = badfib 12 in
      let fib12 = await fib12 in
      fib12 = 144
  end
]

let storage_tests = suite "storage" [
  test "get set" begin fun () ->
    let k1 = Storage.new_key () in
    let k2 = Storage.new_key () in
    spawn @@ fun () ->
      assert (Storage.get k1 = None);
      assert (Storage.get k2 = None);
      Storage.set k1 42;
      assert (Storage.get k1 = Some 42);
      assert (Storage.get k2 = None);
      Storage.set k2 true;
      assert (Storage.get k1 = Some 42);
      assert (Storage.get k2 = Some true);
      Storage.remove k1;
      assert (Storage.get k1 = None);
      assert (Storage.get k2 = Some true);
      true
  end;

  test "storage across await" begin fun () ->
    let k = Storage.new_key () in

    (* spawn another promise that touches storage *)
    let run_promise_async () =
      Lwt.async @@ fun () ->
        Lwt.with_value k (Some "something else") @@ fun () ->
          assert (Lwt.get k = Some "something else");
          Lwt.return_unit
    in

    let run_promise () : unit Lwt.t =
      Lwt.with_value k (Some "another one") @@ fun () ->
        assert (Lwt.get k = Some "another one");
        Lwt.return_unit
    in

    let one_task () =
      run_promise_async();
      assert (Storage.get k = None);
      Storage.set k "v1";
      assert (Storage.get k = Some "v1");
      run_promise () |> await;
      assert (Storage.get k = Some "v1");
      Storage.remove k;
      assert (Storage.get k = None);
      yield();
      assert (Storage.get k = None);
      run_promise () |> await;
      assert (Storage.get k = None);
      run_promise_async();
      yield();
      assert (Storage.get k = None);
      Storage.set k "v2";
      assert (Storage.get k = Some "v2");
      run_promise_async();
      yield();
      run_promise () |> await;
      assert (Storage.get k = Some "v2");
    in

    (* spawn multiple such tasks *)
    let tasks = [ spawn one_task; spawn one_task; spawn one_task ] in

    spawn @@ fun () ->
      List.iter await tasks;
      true
  end;
]

let io_tests = suite "io" [
  test "read io" begin fun () ->
    let str = "some\ninteresting\ntext string here!\n" in
    let ic = Lwt_io.of_bytes ~mode:Input (Lwt_bytes.of_string str) in
    spawn @@ fun () ->
      let lines = ref [] in
      while
        try
          yield ();
          let line = Lwt_io.read_line ic |> await in
          lines := line :: !lines;
          true
        with End_of_file -> false
      do ()
      done;
      List.rev !lines = ["some"; "interesting"; "text string here!"]
  end;

  test "pipe" begin fun () ->
    let ic, oc = Lwt_io.pipe () in
    spawn_in_the_background (fun () ->
      for i = 1 to 100 do
        Lwt_io.write_line oc (string_of_int i) |> await;
        Lwt_io.flush oc |> await
      done;
      Lwt_io.close oc |> await;
    );

    spawn @@ fun () ->
      let sum = ref 0 in
      let continue = ref true in
      while !continue do
        match Lwt_io.read_line ic |> await |> String.trim |> int_of_string with
        | exception End_of_file -> continue := false
        | i -> sum := !sum + i
      done;
      Lwt_io.close ic |> await;
      !sum = 5050
  end
]

let suites = [
  main_tests;
  storage_tests;
  io_tests;
]
