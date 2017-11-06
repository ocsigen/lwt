.PHONY : test
test : clean
	ocamlfind opt -package lwt_ppx -c user.ml
	ocamlfind opt -package lwt -linkpkg user.cmx
	./a.out
	# Expecting failure with "Unbound value >>":
	! ocamlfind opt -package lwt_ppx \
	    -ppxopt lwt_ppx,-no-sequence -c user.ml

.PHONY : clean
clean :
	rm -f *.cm* *.o a.out
