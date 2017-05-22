let () = 
  Ppx_lwt_int.register ();
  Migrate_parsetree.Driver.run_as_ppx_rewriter ()
