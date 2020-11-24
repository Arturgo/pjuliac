open Ast
open X86_64
open Parser
open Lexing

(* TODO : urgent, corrgier le segfault lorsque le tas est trop gros *)

module Smap = Map.Make(String)

let parse buf = 
   let ast = Parser.fichier Lexer.token buf in
   ast;;

let parse_str str = 
   let buf = Lexing.from_string str in
   parse buf

let standard_library = "
function __non(x)
   if x
      return false
   else
      return true
   end
end

function __egal(x, y)
   return !(x - y)
end

function __diff(x, y)
   return !(x == y)
end

function print(x)
   if typeof(x) == 1
      __print_int(x)
   elseif typeof(x) == 2
      __print_bool(x)
   end
end
"

(*
Types : 0 : nothing, 1 : int, 2 : bool, 3 : string
Registes : r15 : tas, 
   r14 : argument des fonctions variadiques
   rax : valeur de retour
   <= r12 : réservé pour les utilitaires *)

(* TODO : Générer les copies et les == pour chaque structure *)

let label_id = ref 0

let new_label () =
   label_id := 1 + !label_id;
   "__lbl_" ^ (string_of_int !label_id)

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

let rec code_expr local_vars = function
| ExprCst(cst) -> (
   match cst with
   | CInt(v) -> set_int (imm64 v)
   | CBool(b) -> if b then set_bool (imm 1) else set_bool (imm 0)
)
| ExprAssignement(LvalueVar(name), expr) ->
   Smap.find name local_vars
| ExprCall(name, args) -> 
   List.fold_left (++) nop (List.map 
      (fun expr -> (code_expr local_vars expr) ++ (pushq !%rax))
   args)
   ++ movq (imm (List.length args)) !%r14
   ++ call name
   ++ addq (imm (8 * List.length args)) !%rsp
| ExprListe(liste) -> 
   List.fold_left (++) nop (List.map (fun expr -> code_expr local_vars expr) liste)
| ExprIfElse(select, true_bloc, false_bloc) -> (
   let label_false = new_label () in
   let label_true = new_label() in
   
   code_expr local_vars select
   ++ get_bool !%rax !%rbx
   ++ testq !%rbx !%rbx
   ++ jz label_false
   ++ code_expr local_vars true_bloc
   ++ jmp label_true
   ++ label label_false
   ++ code_expr local_vars false_bloc
   ++ label label_true
)
| ExprWhile(condition, bloc) -> (
   let label_condition = new_label () in
   let label_debut = new_label () in
   
   jmp label_condition
   ++ label label_debut
   ++ code_expr local_vars bloc
   ++ label label_condition
   ++ code_expr local_vars condition
   ++ get_bool !%rax !%rbx
   ++ testq !%rbx !%rbx
   ++ jnz label_debut
)
| ExprReturn(expr_option) ->
   (match expr_option with
      | Some expr -> (code_expr local_vars expr)
      | None -> nop
   )
   ++ movq !%rbp !%rsp
   ++ popq rbp
   ++ ret
| _ -> nop

let library () =
   (* Typeof function *)
   label "typeof"
   ++ movq (ind rsp ~ofs:(8)) !%r8
   ++ movq (ind r8 ~ofs:(0)) !%rbx
   ++ set_int !%rbx
   ++ ret

   (* Print functions *)
   
   (* TODO : Gérer l'alignement des printf *)
   ++ label "__print_int"
   ++ movq (ilab "int_format") !%rdi
   ++ get_int (ind rsp ~ofs:(8)) !%rsi
   ++ xorq !%rax !%rax
   ++ call "printf"
   ++ ret
   
   ++ label "__print_bool"
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
      | DeclExpr(expr) -> code_expr (Smap.empty) expr
      | _ -> nop
   ) ++ loop_exprs r
   in
   
   let rec loop_decls = function
   | [] -> nop
   | x :: r -> (match x with
      | DeclFonction(nom, args, t, body) -> (
         let local_vars = ref Smap.empty in
         let var_id = ref 8 in
         
         List.iter (fun arg -> 
            (var_id := !var_id + 8;
            local_vars := Smap.add (fst arg) (movq (ind rbp ~ofs:(!var_id)) !%rax) !local_vars)
         ) (List.rev args);
         
         label nom
         ++ pushq !%rbp
         ++ movq !%rsp !%rbp
         
         ++ code_expr !local_vars body
         
         ++ movq !%rbp !%rsp
         ++ popq rbp
         ++ ret
      )
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
   ++ loop_decls f
   ++ loop_decls (parse_str standard_library)
   ; data= label "int_format" ++ string "%d"
   ++ label "string_format" ++ string "%s"
   ++ label "true" ++ string "true"
   ++ label "false" ++ string "false"
   ++ label "tas" ++ dquad []
   }

