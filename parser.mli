
(* The type of tokens. *)

type token = 
  | WHILE
  | VIRGULE
  | TYPAGE
  | TO
  | SUPEGAL
  | SUP
  | STRUCT
  | SEMI
  | RETURN
  | PUIS
  | POINT
  | PLUS
  | PARG
  | PARD_IDENT of (string)
  | PARD
  | OU
  | NON
  | MUTABLE
  | MOINS
  | MOD
  | INFEGAL
  | INF
  | IF
  | IDENT_PARG of (string)
  | IDENT of (string)
  | FUNCTION
  | FOR
  | FOIS
  | ET
  | EOF
  | ENTIER_PARG of (string)
  | ENTIER_IDENT of (string * string)
  | ENTIER of (string)
  | END
  | ELSEIF
  | ELSE
  | EG_SIGNE
  | EGAL
  | DIFF
  | CHAINE of (string)
  | BOOLEEN of (bool)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val fichier: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.fichier)
