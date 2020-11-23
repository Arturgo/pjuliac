open Ast
open X86_64

let get_int addr reg =
   movq addr !%rsi
   (* TODO : Check du type *)
   ++ movq (ind rsi ~ofs:(8)) reg
   
let set_int code =
   movq !%r15 !%rax
   ++ movq (imm 1) (ind r15 ~ofs:(0))
   ++ movq code (ind r15 ~ofs:(8))
   ++ addq (imm 16) !%r15

let library () =
   label "print"
   (* TODO : Gérer l'alignement *)
   ++ movq (ilab "int_format") !%rdi
   ++ get_int (ind rsp ~ofs:(8)) !%rsi
   ++ xorq !%rax !%rax
   ++ call "printf"
   ++ ret
   
   ++ label "__plus"
   ++ get_int (ind rsp ~ofs:(8)) !%rbx
   ++ get_int (ind rsp ~ofs:(16)) !%rcx
   ++ addq !%rcx !%rbx
   ++ set_int !%rbx
   ++ ret

(* Types : 0 : nothing, 1 : int *)

let rec code_expr = function
| ExprCst(cst) -> (
   match cst with
   | CInt(v) -> set_int (imm64 v)
)
| ExprCall(name, args) -> 
   List.fold_left (++) nop (List.map 
      (fun expr -> (code_expr expr) ++ (pushq !%rax))
   args)
   ++ call name
   ++ addq (imm (8 * List.length args)) !%rsp
| ExprListe(liste) -> 
   List.fold_left (++) nop (List.map code_expr liste)
| _ -> nop

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
   ++ label "tas" ++ dquad []
   }

