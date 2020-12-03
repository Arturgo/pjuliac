all:
	rm _build -r || true
	mkdir _build
	cp *.mly _build/
	cp *.mll _build/
	cp *.ml _build/
	cp *.mli _build/

	cd _build; \
	menhir parser.mly --explain; \
	ocamllex lexer.mll; \
	ocamlc -c ast.ml; \
	ocamlopt -c ast.ml; \
	ocamlc -c parser.mli; \
	ocamlc -c parser.ml; \
	ocamlopt -c parser.ml; \
	ocamlc -c lexer.ml; \
	ocamlopt -c lexer.ml; \
	ocamlc -c typeur.ml; \
	ocamlopt -c typeur.ml; \
	ocamlc -c x86_64.mli; \
	ocamlc -c x86_64.ml; \
	ocamlopt -c x86_64.ml; \
	ocamlc -c codegen.ml; \
	ocamlopt -c codegen.ml; \
	ocamlc -c julia.ml; \
	ocamlopt -c julia.ml; \
	ocamlopt -o pjuliac ast.cmx lexer.cmx parser.cmx typeur.cmx x86_64.cmx codegen.cmx julia.cmx; \
	cp pjuliac ../pjuliac; \
	cp pjuliac ../Tests/pjuliac
