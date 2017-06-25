.PHONY : test
test : clean
	@# Deliberately link with lwt.preemptive incorrectly, as Lwt is supporting
	@# this usage until 4.0.0.
	ocamlfind opt -package lwt -c user.ml
	ocamlfind opt -package lwt.preemptive -thread -linkpkg user.cmx
	./a.out

.PHONY : clean
clean :
	rm -f *.cm* *.o a.out
