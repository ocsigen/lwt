let ringopt = (Sys.argv.(1), int_of_string Sys.argv.(2))
let () = Stallerlib.detect ~ringopt ()
