open Ast
open X86_64
open Parser
open Lexing
open Scanf
open Format
open Filename

let list_init n f =
   let rec loop i =
      if i = n then []
      else (f i) :: (loop (i + 1))
   in loop 0

let rec list_count e = function 
| [] -> 0
| x :: r -> (if x = e then 1 else 0) + (list_count e r)

let parse buf = 
   let ast = Parser.fichier Lexer.token buf in
   ast;;

let parse_str str = 
   let buf = Lexing.from_string str in
   parse buf

module Smap = Map.Make(String)

let globals = ref Smap.empty
let () = globals := Smap.add "nothing" (ind r14 ~ofs:(-8)) !globals

type fct = {signature: string list; retour: string; corps: [ `text ] X86_64.asm}

let dispatchers = ref Smap.empty

let iCst = ref 0
let csts = ref nop

type pos_t = [ `Q ] X86_64.operand
type var_manager =
| Global
| Local of (pos_t Smap.t) * (pos_t Smap.t)

let types = ref Smap.empty
let getters = ref ""

let () = types := Smap.add "Nothing" 0 !types
let () = types := Smap.add "Int64" 1 !types
let () = types := Smap.add "Bool" 2 !types
let () = types := Smap.add "String" 3 !types


let standard_library = "
# elements non types

function __non__uncheck(x)
   if x
      false
   else
      true
   end
end

function __egal__uncheck(x, y)
   __non__uncheck(x - y)
end

# elements types

function __undefined__uncheck() 
	__print_string(\"erreur: une variable est utilisee sans etre initialisee\\n\")
	__exit()
end

function __arg(x :: Int64)
   return __deref(__access(x))
end

function __non(x :: Bool)
   __non__uncheck(x)
end

function __egal(x, y)
   __egal__uncheck(x, y)
end

function __diff(x, y)
   !(x == y)
end

function __sup(x :: Int64, y :: Int64)
   y < x
end

function __infegal(x :: Int64, y :: Int64)
   !(x > y)
end

function __supegal(x :: Int64, y :: Int64)
   !(x < y)
end

function __umoins(x :: Int64)
   0 - x
end

function __puis(x :: Int64, y :: Int64)
   prod = 1
   for i = 1 : y
      prod = x * prod
   end
   return prod
end

function div(x :: Int64, y :: Int64)
   if y == 0
      __print_string(\"erreur: division par zero\\n\")
      __exit()
   end
   
   if x < 0
      return -__div_pos(-x, y)
   else
      return __div_pos(x, y)
   end
end

function mod(x :: Int64, y :: Int64)
   if y == 0
      __print_string(\"erreur: division par zero\\n\")
      __exit()
   end
   
   if x < 0
      return -__mod_pos(-x, y)
   else
      return __mod_pos(x, y)
   end
end

function __print(x :: String)
   __print_string(x)
end

function __print(x :: Int64)
   __print_int(x)
end

function __print(x :: Bool)
   if x
      __print_string(\"true\")
   else
      __print_string(\"false\")
   end
end

function print()
   for i = 0 : __nb_args - 1
      __print(__arg(__args_p + 8 * i))
   end
end

function println()
   for i = 0 : __nb_args - 1
      __print(__arg(__args_p + 8 * i))
   end
   __print_string(\"\\n\")
end
"

(*
Types : 0 : nothing, 1 : int, 2 : bool, 3 : string
Registes : 
   r14 : variables globales
   r13 : argument des fonctions variadiques
   rax : valeur de retour
   <= r12 : réservé pour les utilitaires *)

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
   
let set_nothing () =
   movq (imm 8) !%rdi
   ++ call "malloc"
   ++ movq (imm 0) (ind rax ~ofs:(0))
   
let set_int code =
   movq (imm 16) !%rdi
   ++ pushq code
   ++ call "malloc"
   ++ popq rdx
   ++ movq (imm 1) (ind rax ~ofs:(0))
   ++ movq !%rdx (ind rax ~ofs:(8))

let set_bool code =
   movq (imm 16) !%rdi
   ++ pushq code
   ++ call "malloc"
   ++ popq rdx
   ++ movq (imm 2) (ind rax ~ofs:(0))
   ++ movq !%rdx (ind rax ~ofs:(8))

let rec code_expr vars = function
| ExprCst(cst) -> (let label_name = "__cst_" ^ string_of_int !iCst in
   iCst := !iCst + 1;
   csts := !csts ++
   label label_name
   ++ (match cst with
   | CInt(v) -> (
      dquad [Int64.of_int 1; v]
   )
   | CBool(b) -> (
      dquad [Int64.of_int 2; if b then Int64.of_int 1 else Int64.of_int 0]
   )
   | CString(s) -> (
      dquad [Int64.of_int 3; Int64.of_int (String.length s)]
      ++ string (Scanf.unescaped s)
   ));
   movq (ilab label_name) !%rax
)
| ExprAssignement(LvalueVar(name), value) ->
begin
	if name = "__nb_args" then
		set_int (ind rbp ~ofs:(-16))
	else if name = "__args_p" then
		set_int (ind rbp ~ofs:(-8))
	else (
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
		
		(match value with
		| None -> movq position !%rax
			++ testq !%rax !%rax
			++ jz "__fun___undefined__uncheck"
		| Some expr -> (
		   code_expr vars expr
		   ++ movq !%rax position
			)
   	)
   )
end
| ExprAssignement(LvalueAttr(expr, attr), value) ->
   code_expr vars expr
   ++ pushq !%rax
   ++ movq (imm 1) !%r13
   ++ call ("__fun___get_" ^ attr)
   ++ addq (imm 8) !%rsp
   ++ movq (ind rax ~ofs:(8)) !%rbx
   ++ (match value with
   | None -> nop
   | Some e -> (pushq !%rbx ++ code_expr vars e ++ popq rbx 
   ++ movq !%rax (ind rbx ~ofs:(0)))
   )
   ++ movq (ind rbx ~ofs:(0)) !%rax
| ExprCall(name, args) -> 
   (* Exceptions pour les évaluations paresseuses *)
   if name = "__et" then
      let label_sortie = new_label() in
      let [expA; expB] = args in
      
      code_expr vars expA
      ++ get_bool !%rax !%rbx
      ++ testq !%rbx !%rbx
      ++ jz label_sortie
      
      ++ code_expr vars expB
      ++ get_bool !%rax !%rbx
      ++ testq !%rbx !%rbx
      ++ label label_sortie
   else if name = "__ou" then
      let label_sortie = new_label() in
      let [expA; expB] = args in
      
      code_expr vars expA
      ++ get_bool !%rax !%rbx
      ++ testq !%rbx !%rbx
      ++ jnz label_sortie
      
      ++ code_expr vars expB
      ++ get_bool !%rax !%rbx
      ++ testq !%rbx !%rbx
      ++ label label_sortie
   else
      List.fold_left (++) nop (List.map 
         (fun expr -> (code_expr vars expr) ++ (pushq !%rax))
      (List.rev args))
      ++ movq (imm (List.length args)) !%r13
      ++ call ("__fun_" ^ name)
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
| ExprFor(var, deb, fin, bloc) ->
   (* TODO : faire les bonnes scopes des variables *)
   let i = ExprAssignement(LvalueVar(var), None) in
   code_expr vars (ExprListe([
      ExprAssignement(LvalueVar(var), Some deb);
      ExprAssignement(LvalueVar("__" ^ var ^ "_fin"), Some fin);
      ExprWhile(ExprCall("__infegal", [
      i;
      ExprAssignement(LvalueVar("__" ^ var ^ "_fin"), None)
   ]), ExprListe([bloc;
      ExprAssignement(LvalueVar(var), Some (ExprCall("__plus", [i; ExprCst(CInt (Int64.of_int 1))])))
   ]))]))
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
   label "__fun_typeof"
   ++ movq (ind rsp ~ofs:(8)) !%r8
   ++ movq (ind r8 ~ofs:(0)) !%rbx
   ++ set_int !%rbx
   ++ ret
   
   (* Dereferencing pointers *)

   ++ label "__fun___deref"
   ++ movq (ind rsp ~ofs:(8)) !%rax
   ++ movq (ind rax ~ofs:(8)) !%rax
   ++ ret
   
   ++ label "__fun___ref"
   ++ movq (ind rsp ~ofs:(8)) !%rbx
   ++ set_int !%rbx
   ++ ret
   
   ++ label "__fun___access"
   ++ movq (ind rsp ~ofs:(8)) !%rbx
   ++ get_int !%rbx !%rbx
   ++ movq (ind rbx ~ofs:(0)) !%rbx
   ++ set_int !%rbx
   ++ ret
   
   ++ label "__fun___modify"
   ++ movq (ind rsp ~ofs:(8)) !%rbx
   ++ get_int !%rbx !%rbx
   ++ movq (ind rsp ~ofs:(16)) !%rcx
   ++ get_int !%rcx !%rcx
   ++ movq !%rcx (ind rbx ~ofs:(0))
   ++ ret
   
   ++ label "__fun___exit"
   ++ movq (imm 1) !%rax
   ++ movq (imm 1) !%rbx
   ++ syscall

   (* Print functions *)
   
   (* TODO : Gérer l'alignement des printf *)
   ++ label "__fun___print_int"
   ++ movq (ilab "int_format") !%rdi
   ++ get_int (ind rsp ~ofs:(8)) !%rsi
   ++ xorq !%rax !%rax
   ++ call "printf"
   ++ ret
   
   ++ label "__fun___print_string"
   ++ movq (ilab "string_format") !%rdi
   ++ movq (ind rsp ~ofs:(8)) !%r8
   ++ leaq (ind r8 ~ofs:(16)) rsi
   ++ xorq !%rax !%rax
   ++ call "printf"
   ++ ret
   
   (* Operators *)   
   ++ label "__fun___plus"
   ++ get_int (ind rsp ~ofs:(8)) !%rbx
   ++ get_int (ind rsp ~ofs:(16)) !%rcx
   ++ addq !%rbx !%rcx
   ++ set_int !%rcx
   ++ ret
   
   ++ label "__fun___moins"
   ++ get_int (ind rsp ~ofs:(8)) !%rcx
   ++ get_int (ind rsp ~ofs:(16)) !%rbx
   ++ subq !%rbx !%rcx
   ++ set_int !%rcx
   ++ ret
   
   ++ label "__fun___fois"
   ++ get_int (ind rsp ~ofs:(8)) !%rcx
   ++ get_int (ind rsp ~ofs:(16)) !%rbx
   ++ imulq !%rbx !%rcx
   ++ set_int !%rcx
   ++ ret
   
   ++ label "__fun___div_pos"
   ++ get_int (ind rsp ~ofs:(8)) !%rax
   ++ get_int (ind rsp ~ofs:(16)) !%r9
   ++ movq (imm 0) !%rdx
   ++ idivq !%r9
   ++ movq !%rax !%rbx
   ++ set_int !%rbx
   ++ ret
   
   ++ label "__fun___mod_pos"
   ++ get_int (ind rsp ~ofs:(8)) !%rax
   ++ get_int (ind rsp ~ofs:(16)) !%r9
   ++ movq (imm 0) !%rdx
   ++ idivq !%r9
   ++ set_int !%rdx
   ++ ret

   ++ label "__fun___inf"
   ++ get_int (ind rsp ~ofs:(8)) !%rcx
   ++ get_int (ind rsp ~ofs:(16)) !%rbx
   ++ cmpq !%rbx !%rcx
   ++ jl "__inf_true"
   ++ set_bool (imm 0)
   ++ ret
   ++ label "__inf_true"
   ++ set_bool (imm 1)
   ++ ret
   
   ++ label "__fun___malloc"
   ++ get_int (ind rsp ~ofs:(8)) !%rdi
   ++ call "malloc"
   ++ set_int !%rax
   ++ ret

let code_fct args body =
   let args_p = ref Smap.empty in
   let var_id = ref 8 in
   
   List.iter (fun arg -> 
      (var_id := !var_id + 8;
      args_p := Smap.add (fst arg) (ind rbp ~ofs:(!var_id)) !args_p)
   ) (args);
   
   let defaults = 
      Smap.add "__nb_args" (ind rbp ~ofs:(-16))
      (Smap.singleton "__args_p" (ind rbp ~ofs:(-8)))
   in
   
   let local_vars = ref (Local(!args_p, defaults)) in
   let code_fonction = code_expr local_vars body in
   
   let nb_vars = (match !local_vars with
   | Global -> 0
   | Local(args, vars) -> Smap.cardinal vars
   ) in
   
   pushq !%rbp
   ++ movq !%rsp !%rbp
   
 	++ (List.fold_left (++) nop (list_init
 		nb_vars (fun x -> pushq (imm 0))
 	))
   
   (* Place __nb_args et __args_p *)
   ++ movq !%r13 (ind rbp ~ofs:(-16))
   ++ leaq (ind rbp ~ofs:(16)) rbx
   ++ movq !%rbx (ind rbp ~ofs:(-8))
   
   ++ code_fonction
   
   ++ movq !%rbp !%rsp
   ++ popq rbp
   ++ ret

let string_arg i =
   "__arg(__args_p + " ^ (string_of_int (8 * i)) ^ ")"

let check_sig s =
   let rec loop i = function
   | [] -> []
   | x :: r -> 
      if x = "Any" then 
         loop (i + 1) r 
      else 
         ("__egal__uncheck(typeof(" ^ (string_arg i) ^ "), " ^ x ^ ")") :: loop (i + 1) r
   in
   
   let conditions = loop 0 s in
   
   if 0 = List.length conditions then
      "__egal__uncheck(__nb_args, " ^ (string_of_int (List.length s)) ^ ")"
   else 
      "__egal__uncheck(__nb_args, " ^ (string_of_int (List.length s)) ^  ") && (" ^
      (String.concat " && " conditions)
      ^ ")"

let rec	 is_specialised f1 f2 = match f1, f2 with
| [], [] -> true
| x1 :: r1, x2 :: r2 -> (x1 = "Any" || x1 = x2) && is_specialised r1 r2
| _ -> false

let jl_dispatch l =
   let tab = Array.of_list l in
   
   let julia = ref "" in
   
   for i = 0 to (Array.length tab) - 1 do
   	let specialisation = ref "" in
   	let curFct = tab.(i) in
   	
   	for j = 0 to i - 1 do
   		let precFct = tab.(j) in
   		
   		if is_specialised precFct.signature curFct.signature then
   			specialisation := !specialisation ^ "if __egal__uncheck(__func_id, " ^ (string_of_int (j + 1)) ^ ") __func_id = 0; end\n"
   	done;
      
      julia := !julia ^ "if (" ^ (check_sig curFct.signature) ^ ") 
      " ^
      	!specialisation
      ^
      "
      if __egal__uncheck(__func_id, 0) 
      	__func_id = " ^ (string_of_int (i + 1)) ^ "
     	else
     		__print_string(\"erreur: appel ambigu a une fonction\\n\")
     		__exit()
     	end
      end\n"
   done;
   
   !julia

let code_dispatch name l = 
   (* Exceptions *)
   if Filename.check_suffix name "__uncheck" || name = "__fun___arg" || name = "__fun_print" || name = "__fun_println" then (
      (*print_string name;*)
      label name
      ++ (List.hd l).corps
   ) else (
      let l = List.sort (fun f1 f2 ->
         let nb1 = list_count "Any" f1.signature in
         let nb2 = list_count "Any" f2.signature in
         -compare nb1 nb2
      ) l
      in
      let corps = "
      function dispatch()
         __func_id = 0
      " ^
         (jl_dispatch l)
        ^
         (List.fold_left (^) "" (List.mapi (fun i f ->
            "if __egal__uncheck(__func_id, " ^ (string_of_int (1 + i)) ^ ") __ret = __func_" ^ name ^ (string_of_int (1 + i)) ^ "(" ^
            String.concat "," ( list_init (List.length f.signature) string_arg)
            ^ ")
            " ^ (if f.retour <> "Any" then "if __egal__uncheck(typeof(__ret), " ^ f.retour ^ ")
            		return __ret
           		else
           			__print_string(\"erreur: mauvaise valeur de retour pour " ^ name ^ "\\n\")
           			__exit()
           		end
           		"
           	else "return __ret")
           	^ "
            end\n"
         ) l))
        ^ "
        if __egal__uncheck(__func_id, 0)
            __print_string(\"erreur: parametres incorrects pour " ^ name ^ "\\n\")
            __exit()
        end
      end
      " in

      let [DeclFonction(_, _, _, code)] = parse_str corps in
      
      label name
      ++ (code_fct [] code)
      
      ++ (List.fold_left (++) nop (List.mapi (fun i f -> 
         label ("__fun___func_" ^ name ^ string_of_int (1 + i))
         ++ f.corps
      ) l))
   )

let rec loop_exprs = function
   | [] -> nop
   | x :: r -> let code = (match x with
      | DeclExpr(expr) -> code_expr (ref Global) expr
      | _ -> nop
   ) in 
   code ++ loop_exprs r   

let rec loop_fcts = function
   | [] -> ()
   | x :: r -> (match x with
      | DeclFonction(nom, args, t, body) -> (
         let corps = code_fct args body in
         
         dispatchers :=
            Smap.add ("__fun_" ^ nom) (
               {signature = (List.map snd args); retour = (match t with
		            | None -> "Any"
		            | Some e -> e
               ); corps = corps}
               :: (try
                  Smap.find ("__fun_" ^ nom) !dispatchers
               with Not_found -> []) 
            ) !dispatchers;
         ()
      )
      | _ -> ()
   ); 
   loop_fcts r   

let rec loop_structs = function
   | [] -> ()
   | x :: r -> (match x with
      | DeclStructure(nom, mut, vars) -> (
         let type_id = Smap.cardinal !types in
         types := Smap.add nom type_id !types;
         
         getters := !getters ^ 
         (* Constructeur *)
         "function " ^ nom ^ "(" ^ (String.concat "," (List.map (fun x -> 
           (fst x) ^ " :: " ^ (snd x)
         ) vars)) ^ ")
            ptr = __malloc(" ^ (string_of_int (List.length vars)) ^ ")
            __modify(ptr, " ^ (string_of_int type_id) ^ ")
            " ^
            (String.concat "\n" (List.mapi (fun id x -> 
               "__modify(ptr + " ^ (string_of_int (8 + 8 * id)) ^ ", __ref(" ^ (fst x) ^ "))"
            ) vars))
            ^ "
            return __deref(ptr)
         end\n"
         ^ (String.concat "\n" (List.mapi (fun id x ->
            "function __get_" ^ (fst x) ^ "(s :: " ^ nom ^ ")
               return __ref(s) + " ^ (string_of_int (8 + 8 * id))^ "
            end\n"
         ) vars))
      )
      | _ -> ()
   );
   loop_structs r 

let code_fichier f =
   loop_structs f;
   
   let code_exprs = loop_exprs (parse_str 
      (String.concat "\n" (
         Smap.fold (fun nom id l -> ((nom ^ " = " ^ (string_of_int id) ^ "\n") :: l)) !types []
      ))
   )
   ++ loop_exprs f in
   
   loop_fcts f;
   loop_fcts (parse_str !getters);
   loop_fcts (parse_str standard_library);
   
   let functions = (Smap.fold (fun name l code -> (code_dispatch name l) ++ code) !dispatchers nop) in
   
   { text= 
   globl "main"
   ++ label "main"
   ++ pushq !%rbp
   ++ movq !%rsp !%rbp
   
   (* Début des variables globales *)
   ++ movq !%rsp !%r14
   
 	++ (List.fold_left (++) nop (list_init
 		(Smap.cardinal !globals) (fun x -> pushq (imm 0))
 	))
   
   (* Création du nothing *)
   ++ set_nothing ()
   ++ movq !%rax (ind r14 ~ofs:(-8))
   
   ++ code_exprs
   
   ++ movq !%rbp !%rsp
   ++ popq rbp
   
   ++ movq (imm 0) !%rax
   ++ ret
   
   ++ library ()
   ++ functions
   
   ; data= label "int_format" ++ string "%lld"
   ++ label "string_format" ++ string "%s"
   ++ !csts
   }

