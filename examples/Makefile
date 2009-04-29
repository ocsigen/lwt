all: relay start_editor ex_gtk

OCAMLC = ocamlfind ocamlc

relay: relay.ml
	$(OCAMLC) -o relay -linkpkg -package lwt relay.ml -cclib -s

start_editor : editor.ml
	$(OCAMLC) -o start_editor -linkpkg -package lwt editor.ml -cclib -s

ex_gtk: ex_gtk.ml
	$(OCAMLC) -o ex_gtk -linkpkg -package lwt.glib ex_gtk.ml -cclib -s

clean:
	-rm -f *.cmi *.cmo *~ relay start_editor ex_gtk
