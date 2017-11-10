.PHONY : test
test : clean
	@# Deliberately link with lwt.unix incorrectly, as Lwt is supporting this
	@# usage until 4.0.0.
	ocamlfind opt -package lwt -c user.ml
	ocamlfind opt -package lwt.unix -linkpkg -thread user.cmx
	./a.out

.PHONY : clean
clean :
	rm -f *.cm* *.o a.out
