all: relay start_editor show_keys ex_gtk

OCAMLC = ocamlfind ocamlc

relay: relay.ml
	$(OCAMLC) -o relay -linkpkg -package lwt relay.ml -cclib -s

start_editor : editor.ml
	$(OCAMLC) -o start_editor -linkpkg -package lwt editor.ml -cclib -s

show_keys: show_keys.ml
	$(OCAMLC) -o show_keys -linkpkg -syntax camlp4o -package lwt,lwt.syntax show_keys.ml

ex_gtk: ex_gtk.ml
	$(OCAMLC) -o ex_gtk -linkpkg -package lwt.glib ex_gtk.ml

clean:
	-rm -f *.cmi *.cmo *~ relay start_editor ex_gtk show_keys
