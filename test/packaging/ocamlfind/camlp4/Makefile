.PHONY : test
test : clean
	ocamlfind opt -syntax camlp4o -package lwt,lwt_camlp4 -c user.ml
	ocamlfind opt -package lwt -linkpkg user.cmx
	./a.out

.PHONY : clean
clean :
	rm -f *.cm* *.o a.out
