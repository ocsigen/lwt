.PHONY : test
test : clean
	ocamlfind opt -package lwt.ppx -c user.ml
	ocamlfind opt -package lwt -linkpkg user.cmx
	./a.out
	@# The PPX option needs to be passed to lwt.ppx.deprecated-ppx-method
	@# instead of lwt.ppx. This is a bug awaiting a fix in
	@#   https://github.com/ocsigen/lwt/issues/417
	# Expecting failure with "Unbound value >>":
	! ocamlfind opt -package lwt.ppx \
	    -ppxopt lwt.ppx.deprecated-ppx-method,-no-sequence -c user.ml

.PHONY : clean
clean :
	rm -f *.cm* *.o a.out
