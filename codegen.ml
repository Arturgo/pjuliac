open Ast
open X86_64
open Parser
open Lexing
open Format

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

function __copy(x)
   return __copy_singleton(x)
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
   r14 : variables globales
   r13 : argument des fonctions variadiques
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
   ++ movq (ind r8 ~ofs:(8)) reg

let get_bool addr reg =
   movq addr !%r8
   ++ movq (ind r8 ~ofs:(8)) reg
   
let set_int code =
   subq (imm 16) !%r15
   ++ movq (imm 1) (ind r15 ~ofs:(0))
   ++ movq code (ind r15 ~ofs:(8))
   ++ movq !%r15 !%rax

let set_bool code =
   subq (imm 16) !%r15
   ++ movq (imm 2) (ind r15 ~ofs:(0))
   ++ movq code (ind r15 ~ofs:(8))
   ++ movq !%r15 !%rax

let globals = ref Smap.empty

type pos_t = [ `Q ] X86_64.operand
type var_manager =
| Global
| Local of (pos_t Smap.t) * (pos_t Smap.t)

let rec code_expr vars = function
| ExprCst(cst) -> (
   match cst with
   | CInt(v) -> set_int (imm64 v)
   | CBool(b) -> if b then set_bool (imm 1) else set_bool (imm 0)
)
| ExprAssignement(LvalueVar(name), value) ->
begin
   let position = (match !vars with
   | Global -> 
      if (Smap.mem name !globals) then
         Smap.find name !globals
      else (
         globals := Smap.add name (ind r14 ~ofs:(-8 * (1 + Smap.cardinal !globals))) !globals;
         Smap.find name !globals
      )
   | Local(args, locals) ->
      if (Smap.mem name args)  then
         Smap.find name args
      else if (Smap.mem name locals) then
         Smap.find name locals
      else if value = None && Smap.mem name !globals then
         Smap.find name !globals
      else (
         let nouv_pos = (ind rbp ~ofs:(-8 * (1 + Smap.cardinal locals))) in
         vars := (Local(args, Smap.add name nouv_pos locals));
         nouv_pos
      )
   ) in
   
   match value with
   | None -> movq position !%rax
   | Some expr -> (
      code_expr vars expr
      ++ pushq !%rax
      ++ call "__copy"
      ++ addq (imm 8) !%rsp
      ++ movq !%rax position
   )
end
| ExprCall(name, args) -> 
   List.fold_left (++) nop (List.map 
      (fun expr -> (code_expr vars expr) ++ (pushq !%rax))
   args)
   ++ movq (imm (List.length args)) !%r13
   ++ call name
   ++ addq (imm (8 * List.length args)) !%rsp
| ExprListe(liste) -> 
   let rec loop = function
   | [] -> nop
   | x :: r -> let code = code_expr vars x in code ++ loop r
   in loop liste
| ExprIfElse(select, true_bloc, false_bloc) -> (
   let label_false = new_label () in
   let label_true = new_label() in
   
   code_expr vars select
   ++ get_bool !%rax !%rbx
   ++ testq !%rbx !%rbx
   ++ jz label_false
   ++ code_expr vars true_bloc
   ++ jmp label_true
   ++ label label_false
   ++ code_expr vars false_bloc
   ++ label label_true
)
| ExprWhile(condition, bloc) -> (
   let label_condition = new_label () in
   let label_debut = new_label () in
   
   jmp label_condition
   ++ label label_debut
   ++ code_expr vars bloc
   ++ label label_condition
   ++ code_expr vars condition
   ++ get_bool !%rax !%rbx
   ++ testq !%rbx !%rbx
   ++ jnz label_debut
)
| ExprReturn(expr_option) ->
   (match expr_option with
      | Some expr -> (code_expr vars expr)
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
   
   (* Copy functions *)
   
   ++ label "__copy_singleton"
   ++ movq (ind rsp ~ofs:(8)) !%r8
   ++ movq (ind r8 ~ofs:(0)) !%rbx
   ++ movq (ind r8 ~ofs:(8)) !%rcx
   ++ subq (imm 16) !%r15
   ++ movq !%rbx (ind r15 ~ofs:(0))
   ++ movq !%rcx (ind r15 ~ofs:(8))
   ++ movq !%r15 !%rax
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
   | x :: r -> let code = (match x with
      | DeclExpr(expr) -> code_expr (ref Global) expr
      | _ -> nop
   ) in code ++ loop_exprs r
   in
   
   let rec loop_decls = function
   | [] -> nop
   | x :: r -> (match x with
      | DeclFonction(nom, args, t, body) -> (
         let args_p = ref Smap.empty in
         let var_id = ref 8 in
         
         List.iter (fun arg -> 
            (var_id := !var_id + 8;
            args_p := Smap.add (fst arg) (ind rbp ~ofs:(!var_id)) !args_p)
         ) (List.rev args);
         
         
         let local_vars = ref (Local(!args_p, Smap.empty)) in
         let code_fonction = code_expr local_vars body in
         
         let nb_vars = (match !local_vars with
         | Global -> 0
         | Local(args, vars) -> Smap.cardinal vars
         ) in
         
         label nom
         ++ pushq !%rbp
         ++ movq !%rsp !%rbp
         
         ++ subq (imm (8 * nb_vars)) !%rsp
         ++ code_fonction
         
         ++ movq !%rbp !%rsp
         ++ popq rbp
         ++ ret
      )
      | _ -> nop
   ) ++ loop_decls r
   in
   
   let code_exprs = loop_exprs f in
   
   let code_decls = loop_decls f
   ++ loop_decls (parse_str standard_library) in   
   
   { text= 
   globl "main"
   ++ label "main"
   ++ pushq !%rbp
   ++ movq !%rsp !%rbp
   
   ++ movq !%rsp !%r15
   (* Maximum heap size *)
   ++ subq (imm 1048576) !%rsp
   (* Début des variables globales *)
   ++ movq !%rsp !%r14
   ++ subq (imm (8 * (Smap.cardinal !globals))) !%rsp
   ++ code_exprs
   ++ movq (imm 0) !%rax
   
   ++ movq !%rbp !%rsp
   ++ popq rbp
   ++ ret
   
   ++ library ()
   ++ code_decls
   
   ; data= label "int_format" ++ string "%d"
   ++ label "string_format" ++ string "%s"
   ++ label "true" ++ string "true"
   ++ label "false" ++ string "false"
   }

