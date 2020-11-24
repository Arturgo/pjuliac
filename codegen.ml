open Ast
open X86_64

(*
Types : 0 : nothing, 1 : int, 2 : bool
Registes : r15 : tas, 
   r14 : argument des fonctions variadiques
   rax : valeur de retour
   <= r12 : réservé pour les utilitaires *)

(* Utilitaires *)
let get_int addr reg =
   movq addr !%r8
   (* TODO : Check du type *)
   ++ movq (ind r8 ~ofs:(8)) reg

let get_bool addr reg =
   movq addr !%r8
   (* TODO : Check du type *)
   ++ movq (ind r8 ~ofs:(8)) reg
   
let set_int code =
   movq !%r15 !%rax
   ++ movq (imm 1) (ind r15 ~ofs:(0))
   ++ movq code (ind r15 ~ofs:(8))
   ++ addq (imm 16) !%r15

let set_bool code =
   movq !%r15 !%rax
   ++ movq (imm 2) (ind r15 ~ofs:(0))
   ++ movq code (ind r15 ~ofs:(8))
   ++ addq (imm 16) !%r15

let rec code_expr = function
| ExprCst(cst) -> (
   match cst with
   | CInt(v) -> set_int (imm64 v)
   | CBool(b) -> if b then set_bool (imm 1) else set_bool (imm 0)
)
| ExprCall(name, args) -> 
   List.fold_left (++) nop (List.map 
      (fun expr -> (code_expr expr) ++ (pushq !%rax))
   args)
   ++ movq (imm (List.length args)) !%r14
   ++ call name
   ++ addq (imm (8 * List.length args)) !%rsp
| ExprListe(liste) -> 
   List.fold_left (++) nop (List.map code_expr liste)
| _ -> nop

let library () =
   (* Print functions *)
   
   (* TODO : Gérer l'alignement des printf *)
   label "print_int"
   ++ movq (ilab "int_format") !%rdi
   ++ get_int (ind rsp ~ofs:(8)) !%rsi
   ++ xorq !%rax !%rax
   ++ call "printf"
   ++ ret
   
   ++ label "print_bool"
   ++ movq (ilab "string_format") !%rdi
   ++ get_bool (ind rsp ~ofs:(8)) !%rbx
   ++ movq (ilab "false") !%rsi
   ++ testq !%rbx !%rbx
   ++ jz "__print_bool_false"
   ++ movq (ilab "true") !%rsi
   ++ label "__print_bool_false"
   ++ xorq !%rax !%rax
   ++ call "printf"
   ++ ret
   
   (* Operators *)
   ++ label "__plus"
   ++ get_int (ind rsp ~ofs:(8)) !%rbx
   ++ get_int (ind rsp ~ofs:(16)) !%rcx
   ++ addq !%rbx !%rcx
   ++ set_int !%rcx
   ++ ret
   
   ++ label "__moins"
   ++ get_int (ind rsp ~ofs:(8)) !%rbx
   ++ get_int (ind rsp ~ofs:(16)) !%rcx
   ++ subq !%rbx !%rcx
   ++ set_int !%rcx
   ++ ret
   
   ++ label "__fois"
   ++ get_int (ind rsp ~ofs:(8)) !%rbx
   ++ get_int (ind rsp ~ofs:(16)) !%rcx
   ++ imulq !%rbx !%rcx
   ++ set_int !%rcx
   ++ ret
   
   ++ label "div"
   ++ get_int (ind rsp ~ofs:(8)) !%r9
   ++ get_int (ind rsp ~ofs:(16)) !%rax
   ++ movq (imm 0) !%rdx
   ++ idivq !%r9
   ++ movq !%rax !%rbx
   ++ set_int !%rbx
   ++ ret
   
   ++ label "mod"
   ++ get_int (ind rsp ~ofs:(8)) !%r9
   ++ get_int (ind rsp ~ofs:(16)) !%rax
   ++ movq (imm 0) !%rdx
   ++ idivq !%r9
   ++ set_int !%rdx
   ++ ret

   ++ label "__et"
   ++ get_bool (ind rsp ~ofs:(8)) !%rbx
   ++ get_bool (ind rsp ~ofs:(16)) !%rcx
   ++ andq !%rbx !%rcx
   ++ set_bool !%rcx
   ++ ret
   
   ++ label "__ou"
   ++ get_bool (ind rsp ~ofs:(8)) !%rbx
   ++ get_bool (ind rsp ~ofs:(16)) !%rcx
   ++ orq !%rbx !%rcx
   ++ set_bool !%rcx
   ++ ret

let code_fichier f =
   let rec loop_exprs = function
   | [] -> nop
   | x :: r -> (match x with
      | DeclExpr(expr) -> code_expr expr
      | _ -> nop
   ) ++ loop_exprs r
   in
   
   let rec loop_decls = function
   | [] -> nop
   | x :: r -> (match x with
      | DeclFonction(nom, args, t, body) -> 
      (* TODO : gérer la portée, les variables locales, les types, ... *)
         label nom
         ++ code_expr body
         ++ ret
      | _ -> nop
   ) ++ loop_decls r
   in
   
   { text= 
   globl "main"
   ++ label "main"
   ++ leaq (lab "tas") r15
   ++ loop_exprs f
   ++ movq (imm 0) !%rax
   ++ ret
   ++ library ()
   ++ loop_decls f;
   data= label "int_format" ++ string "%d"
   ++ label "string_format" ++ string "%s"
   ++ label "true" ++ string "true"
   ++ label "false" ++ string "false"
   ++ label "tas" ++ dquad []
   }

