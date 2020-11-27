open Format
open Codegen
open X86_64

let code_str str =
   let p = code_fichier (parse_str str) in
   print_program Format.std_formatter p

let filename = Sys.argv.(1)

let f = open_in filename
let buf = Lexing.from_channel f
let ast = try 
   parse buf
with 
| _ -> (Format.printf "File \"%s\", line %d, characters %d-%d:\n syntax error" filename buf.lex_curr_p.pos_lnum
(buf.lex_start_p.pos_cnum - buf.lex_start_p.pos_bol)
(buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol); exit 1)

let asm_out = (Filename.remove_extension filename) ^ ".s"
let () = print_program (Format.formatter_of_out_channel (open_out asm_out)) (code_fichier ast)
