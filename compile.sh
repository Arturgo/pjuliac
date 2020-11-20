#!/bin/bash

rm _build -r
mkdir _build
cp *.mly _build/
cp *.mll _build/
cp *.ml _build/

cd _build

menhir parser.mly --explain
ocamllex lexer.mll

ocamlc -c ast.ml
ocamlopt -c ast.ml

ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlopt -c parser.ml

ocamlc -c lexer.ml
ocamlopt -c lexer.ml

ocamlc -c julia.ml
ocamlopt -c julia.ml

ocamlc -c typeur.ml
ocamlopt -c typeur.ml

ocamlopt -o pjuliac ast.cmx lexer.cmx parser.cmx julia.cmx typeur.cmx

cp pjuliac ../pjuliac
