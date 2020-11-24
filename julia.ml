open Parser
open Format
open Lexing
open Codegen
open X86_64

let parse buf = 
   let ast = Parser.fichier Lexer.token buf in
   ast;;


let parse_str str = 
   let buf = Lexing.from_string str in
   parse buf

let code_str str =
   let p = code_fichier (parse_str str) in
   print_program Format.std_formatter p
   

(*let filename = Sys.argv.(2)

let f = open_in filename
let buf = Lexing.from_channel f
let ast = try 
   parse buf
with 
| _ -> Format.printf "File \"%s\", line %d, characters %d-%d:\n syntax error" filename buf.lex_curr_p.pos_lnum
(buf.lex_start_p.pos_cnum - buf.lex_start_p.pos_bol)
(buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol); exit 1*)

let () = code_str "print_bool(true && true); print_bool(true && false); print_bool(false && false); print_int(40 + 50 - 10);";;
