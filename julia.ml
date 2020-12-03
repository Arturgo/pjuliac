open Format
open Codegen
open X86_64
open Typeur
open Arg

let code_str str =
   let p = code_fichier (parse_str str) in
   print_program Format.std_formatter p
 
 
 
let parse_only = ref false
let type_only = ref false
let filename = ref ""

let specs = [("--parse-only", Arg.Set parse_only, "Se contenter de parser le .jl");
("--type-only", Arg.Set type_only, "Se contenter de parser et typer le .jl")]

let msg = "pjuliac est un compilateur de petit-julia. Options disponibles : "

let () = Arg.parse specs (fun f -> filename := f) msg

let f = open_in !filename
let buf = Lexing.from_channel f
let ast = try 
   Codegen.parse buf
with 
| _ -> (Format.printf "File \"%s\", line %d, characters %d-%d:\n syntax error" !filename buf.lex_curr_p.pos_lnum
(buf.lex_start_p.pos_cnum - buf.lex_start_p.pos_bol)
(buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol); exit 1)

let () = if !parse_only then exit 0

let ast = try
   calculerTypage ast
with
| Failure(s) -> (Format.printf "%s\n" s; exit 1)

let () = if !type_only then exit 0

let asm_out = (Filename.remove_extension !filename) ^ ".s"
let () = print_program (Format.formatter_of_out_channel (open_out asm_out)) (code_fichier ast)
