open Ast
open X86_64

(* TODO : rendre les ints en pointeurs avec type *)

let library () =
   label "print"
   (* TODO : Gérer l'alignement *)
   ++ movq (ilab "int_format") !%rdi
   ++ movq (ind rsp ~ofs:(8)) !%rsi
   ++ xorq !%rax !%rax
   ++ call "printf"
   ++ ret
   
   ++ label "__plus"
   ++ movq (ind rsp ~ofs:(8)) !%rax
   ++ addq (ind rsp ~ofs:(16)) !%rax
   ++ ret

let rec code_expr = function
| ExprCst(cst) -> (
   match cst with
   | CInt(v) -> movq (imm64 v) !%rax
)
| ExprCall(name, args) -> 
    List.fold_left (++) nop (List.map 
      (fun expr -> (code_expr expr) ++ (pushq !%rax))
   args)
   ++ call name
   ++ addq (imm (8 * List.length args)) !%rsp

let code_fichier f =
   let rec loop = function
   | [] -> nop
   | x :: r -> (match x with
      | DeclExpr(expr) -> code_expr expr
   ) ++ loop r
   in
   
   { text= globl "main"
   ++ label "main"
   ++ loop f
   ++ movq (imm 0) !%rax
   ++ ret
   ++ library ();
   data= label "int_format" ++ string "%d"
   }

