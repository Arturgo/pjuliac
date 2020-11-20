open Parser
open Format
open Lexing

let parse buf = 
   let ast = Parser.fichier Lexer.token buf in
   ast;;


let parse_str str = 
   let buf = Lexing.from_string str in
   parse buf

let filename = Sys.argv.(2)

let f = open_in filename
let buf = Lexing.from_channel f
let ast = try 
   parse buf
with 
| _ -> Format.printf "File \"%s\", line %d, characters %d-%d:\n syntax error" filename buf.lex_curr_p.pos_lnum
(buf.lex_start_p.pos_cnum - buf.lex_start_p.pos_bol)
(buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol); exit 1
