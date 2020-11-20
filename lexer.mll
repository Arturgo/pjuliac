{
   open Lexing
   open Parser
      
   let kwd_tbl = [
      "else",ELSE; 
      "elseif",ELSEIF; 
      "end",END; 
      "false",BOOLEEN(false);
      "for",FOR;
      "function",FUNCTION;
      "if",IF;
      "mutable",MUTABLE;
      "return",RETURN;
      "struct",STRUCT;
      "true",BOOLEEN(true);
      "while",WHILE
   ]

   let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s
   
   (* TODO : supprimer les effets de bords sur semi *)
   let semi = ref false
}

let chiffre = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | chiffre)*
let entier = chiffre+
let car = ([' '-'~'] # ['\\' '"']) | "\\\\" | "\\\"" | "\\n" | "\\t"
let chaine = '"' car* '"'

rule token = parse
| eof { if !semi then (semi := false; SEMI) else EOF }
| [' ' '\t']+ { token lexbuf }

| '\n' { new_line lexbuf; if !semi then (semi := false; SEMI) else token lexbuf }

(* TODO : gérer les commentaires entre le else et le if *)
(*| "else if" { raise (Failure "else if -> elseif") }*)

| entier as _entier { semi := true; ENTIER(_entier) }
| chaine as _chaine { semi := true; CHAINE(_chaine) }
| ident as _ident '(' { semi := false; IDENT_PARG(_ident) }
| entier as _entier '(' { semi := false; ENTIER_PARG(_entier) }
| (entier as _entier) (ident as _ident) { semi := true; ENTIER_IDENT(_entier, _ident) }
| ')' (ident as _ident) { semi := true; PARD_IDENT(_ident) }

| ident as _ident { 
   (if _ident = "true" || _ident = "false" || _ident = "return" || _ident = "end" || (not (List.mem_assoc _ident kwd_tbl)) then semi := true else semi := false ); 
   id_or_kwd _ident 
}

| '!' { semi := false; NON }
| '+' { semi := false; PLUS }
| '-' { semi := false; MOINS }
| '*' { semi := false; FOIS }
| '%' { semi := false; MOD }
| "==" { semi := false; EGAL }
| "!=" { semi := false; DIFF }
| '<'  { semi := false; INF }
| "<=" { semi := false; INFEGAL }
| '>'  { semi := false; SUP }
| ">=" { semi := false; SUPEGAL }
| '^'  { semi := false; PUIS }
| "&&" { semi := false; ET }
| "||" { semi := false; OU }

| '(' { semi := false; PARG }
| ')' { semi := true; PARD }

| ';' { semi := false; SEMI }
| ',' { semi := false; VIRGULE }
| '.' { semi := false; POINT }
| '=' { semi := false; EG_SIGNE }
| "::" { semi := false; TYPAGE }
| ':' { semi := false; TO }
| '#' { comment lexbuf }

and comment = parse
| '\n' { new_line lexbuf; if !semi then (semi := false; SEMI) else token lexbuf }
| _ { comment lexbuf }
