open Ast
open X86_64
open Parser
open Lexing
open Scanf
open Format
open Filename

(* "list_init" est équivalent à List.init, n'existe pas dans ma version de Ocaml *)

let list_init n f =
   let rec loop i =
      if i = n then []
      else (f i) :: (loop (i + 1))
   in loop 0

(* "list_count" compte le nombre d'éléments égaux à e dans une liste *)

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

(* Contient les addresses des variables globales. *)
let globals = ref Smap.empty
let () = globals := Smap.add "nothing" (ind r14 ~ofs:(-8)) !globals

(* Type d'une fonction : contient la signature, le type de retour, et son code assembleur. *)
type fct = {signature: string list; retour: string; corps: [ `text ] X86_64.asm}

let dispatchers = ref Smap.empty

let iCst = ref 0
let csts = ref nop

type pos_t = [ `Q ] X86_64.operand
type var_manager =
| Global
| Local of (pos_t Smap.t) * (pos_t Smap.t)

(* On associe à chaque type/structure un identifiant unique, dans la map "types" *)

let types = ref Smap.empty
let () = types := Smap.add "Nothing" 0 !types
let () = types := Smap.add "Int64" 1 !types
let () = types := Smap.add "Bool" 2 !types
let () = types := Smap.add "String" 3 !types

(* "getters" contient le code julia généré pour les getters, ie les fonctions pour accéder aux champs des structures *)

let getters = ref ""

(* "get_int" a pour argument une adresse "addr" sous la forme d'une opérande asm et met le booléen contenu dans l'objet pointé dans le registre "reg" *)

let get_int addr reg =
   movq addr !%r8
   ++ movq (ind r8 ~ofs:(8)) reg

(* "get_bool" a pour argument une adresse "addr" sous la forme d'une opérande asm et met l'entier contenu dans l'objet pointé dans le registre "reg" *)

let get_bool addr reg =
   movq addr !%r8
   ++ movq (ind r8 ~ofs:(8)) reg

(* "set_nothing" crée un objet de type nothing, et met son pointeur dans "rax" *)

let set_nothing () =
   movq (imm 8) !%rdi
   ++ call "malloc"
   ++ movq (imm 0) (ind rax ~ofs:(0))
   
(* "set_int" crée un objet entier contenant l'opérande asm "code", et met son pointeur dans "rax" *)

let set_int code =
   movq (imm 16) !%rdi
   ++ pushq code
   ++ call "malloc"
   ++ popq rdx
   ++ movq (imm 1) (ind rax ~ofs:(0))
   ++ movq !%rdx (ind rax ~ofs:(8))

(* "set_bool" crée un objet booléen contenant l'opérande asm "code", et met son pointeur dans "rax" *)

let set_bool code =
   movq (imm 16) !%rdi
   ++ pushq code
   ++ call "malloc"
   ++ popq rdx
   ++ movq (imm 2) (ind rax ~ofs:(0))
   ++ movq !%rdx (ind rax ~ofs:(8))

(* "library" contient les fonctions assembleurs de base.
Dans le code généré, l'étiquette d'une fonction est préfixée de "__fun_" *)

let library () =
   (* "typeof" renvoie un entier : le type de l'objet passé en paramètre *)
   label "__fun_typeof"
   ++ movq (ind rsp ~ofs:(8)) !%r8
   ++ movq (ind r8 ~ofs:(0)) !%rbx
   ++ set_int !%rbx
   ++ ret
   
   (* "__deref" prend un pointeur et renvoie l'objet contenu à cette adresse. *)

   ++ label "__fun___deref"
   ++ movq (ind rsp ~ofs:(8)) !%rax
   ++ movq (ind rax ~ofs:(8)) !%rax
   ++ ret
   
   (* "__ref" renvoie un entier : l'adresse de l'objet passé en paramètre *)
   
   ++ label "__fun___ref"
   ++ movq (ind rsp ~ofs:(8)) !%rbx
   ++ set_int !%rbx
   ++ ret
   
   (* "__access" permet de lire à n'importe quelle position de la RAM. *)
   
   ++ label "__fun___access"
   ++ movq (ind rsp ~ofs:(8)) !%rbx
   ++ get_int !%rbx !%rbx
   ++ movq (ind rbx ~ofs:(0)) !%rbx
   ++ set_int !%rbx
   ++ ret
   
   (* "__modify" permet de modifier n'importe quelle position RAM. *)
   
   ++ label "__fun___modify"
   ++ movq (ind rsp ~ofs:(8)) !%rbx
   ++ get_int !%rbx !%rbx
   ++ movq (ind rsp ~ofs:(16)) !%rcx
   ++ get_int !%rcx !%rcx
   ++ movq !%rcx (ind rbx ~ofs:(0))
   ++ ret
   
   (* "__exit" sort du programme avec le code d'erreur 1 *)
   
   ++ label "__fun___exit"
   ++ movq (imm 1) !%rax
   ++ movq (imm 1) !%rbx
   ++ syscall

	(* TODO : Gérer l'alignement des printf *)
   (* "__print_int" affiche l'entier passé en paramètre *)
   
   ++ label "__fun___print_int"
   ++ movq (ilab "int_format") !%rdi
   ++ get_int (ind rsp ~ofs:(8)) !%rsi
   ++ xorq !%rax !%rax
   ++ call "printf"
   ++ ret
   
   (* "__print_string" affiche la chaîne de caractères passée en paramètre *)
   
   ++ label "__fun___print_string"
   ++ movq (ilab "string_format") !%rdi
   ++ movq (ind rsp ~ofs:(8)) !%r8
   ++ leaq (ind r8 ~ofs:(16)) rsi
   ++ xorq !%rax !%rax
   ++ call "printf"
   ++ ret
   
   (* "__plus_int" renvoie la somme des deux entiers passés en paramètres *)
     
   ++ label "__fun___plus_int"
   ++ get_int (ind rsp ~ofs:(8)) !%rbx
   ++ get_int (ind rsp ~ofs:(16)) !%rcx
   ++ addq !%rbx !%rcx
   ++ set_int !%rcx
   ++ ret
   
   (* "__moins_int" renvoie la différence des deux entiers passés en paramètres *)
   
   ++ label "__fun___moins_int"
   ++ get_int (ind rsp ~ofs:(8)) !%rcx
   ++ get_int (ind rsp ~ofs:(16)) !%rbx
   ++ subq !%rbx !%rcx
   ++ set_int !%rcx
   ++ ret
   
   (* "__fois_int" renvoie le produit des deux entiers passés en paramètres *)
   
   ++ label "__fun___fois_int"
   ++ get_int (ind rsp ~ofs:(8)) !%rcx
   ++ get_int (ind rsp ~ofs:(16)) !%rbx
   ++ imulq !%rbx !%rcx
   ++ set_int !%rcx
   ++ ret
   
   (* "__div_int" renvoie le quotient des deux entiers passés en paramètres *)
   
   ++ label "__fun___div_pos"
   ++ get_int (ind rsp ~ofs:(8)) !%rax
   ++ get_int (ind rsp ~ofs:(16)) !%r9
   ++ movq (imm 0) !%rdx
   ++ idivq !%r9
   ++ movq !%rax !%rbx
   ++ set_int !%rbx
   ++ ret
   
   (* "__mod_int" renvoie le reste dans la division euclidienne des deux entiers passés en paramètres *)
   
   ++ label "__fun___mod_pos"
   ++ get_int (ind rsp ~ofs:(8)) !%rax
   ++ get_int (ind rsp ~ofs:(16)) !%r9
   ++ movq (imm 0) !%rdx
   ++ idivq !%r9
   ++ set_int !%rdx
   ++ ret

	(* "__inf_int" renvoie un booléen qui vaut "true" si le premier argument est plus petit que le deuxième *)

   ++ label "__fun___inf_int"
   ++ get_int (ind rsp ~ofs:(8)) !%rcx
   ++ get_int (ind rsp ~ofs:(16)) !%rbx
   ++ cmpq !%rbx !%rcx
   ++ jl "__inf_true"
   ++ set_bool (imm 0)
   ++ ret
   ++ label "__inf_true"
   ++ set_bool (imm 1)
   ++ ret
   
   (* "__malloc" prend en paramètre un entier "size" et renvoie un entier : l'adresse d'un bloc mémoire libre de taille "size" *)
   
   ++ label "__fun___malloc"
   ++ get_int (ind rsp ~ofs:(8)) !%rdi
   ++ call "malloc"
   ++ set_int !%rax
   ++ ret

(* "standard_library" contient le code julia de l'interface entre les quelques fonctions codées en assembleur avec le reste des codes générés *)

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
   __non__uncheck(__moins_int(x, y))
end

function __undefined__uncheck() 
	__print_string(\"erreur: une variable est utilisee sans etre initialisee\\n\")
	__exit()
end

# elements types

function __arg(x :: Int64)
   return __deref(__access(x))
end

function __non(x :: Bool)
   __non__uncheck(x)
end

function __plus(x :: Int64, y :: Int64)
	__plus_int(x, y)
end

function __moins(x :: Int64, y :: Int64)
	__moins_int(x, y)
end

function __fois(x :: Int64, y :: Int64)
	__fois_int(x, y)
end

function __egal(x, y)
	if __egal__uncheck(typeof(x), 0) && __egal__uncheck(typeof(y), 0)
		return true
	end
	
	if (__egal__uncheck(typeof(x), 1) || __egal__uncheck(typeof(x), 2)) && (__egal__uncheck(typeof(y), 1) || __egal__uncheck(typeof(y), 2))
		return __egal__uncheck(x, y)
	end
	
   __egal__uncheck(__ref(x), __ref(y))
end

function __diff(x, y)
   !(x == y)
end

function __inf(x :: Int64, y :: Int64)
   __inf_int(x, y)
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

(* "label_id" contient le nombre d'étiquettes asm crées jusqu'à présent *)

let label_id = ref 0

(* "new_label" renvoie une nouvelle étiquette asm *)

let new_label () =
   label_id := 1 + !label_id;
   "__lbl_" ^ (string_of_int !label_id)

(* "code_expr" génère le code d'une expression, en prenant en paramètre l'ensemble des variables locales dans "vars", et l'expression dont il faut générer le code *)

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
	(* "__nb_args" et "__args_p" sont les variables qui contiennent respectivement le nombre d'arguments et le pointeur vers le début des arguments. 
	Elles sont gérées à part pour améliorer les performances mémoire. *)
	
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
		
		(* Si on utilise une variable sans l'avoir initialisée, on génère un erreur *)
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
   
   (* Les champs d'une structure sont obligatoirement initialisés par l'appel au constructeur : pas besoin de tester. *)
   ++ (match value with
   | None -> nop
   | Some e -> (pushq !%rbx ++ code_expr vars e ++ popq rbx 
   ++ movq !%rax (ind rbx ~ofs:(0)))
   )
   ++ movq (ind rbx ~ofs:(0)) !%rax
| ExprCall(name, args) -> 
   (* Le schéma d'appel est différent pour "et" et "ou" à cause de l'évaluation paresseuse. *)
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

(* "code_fct" s'occupe de générer le code assembleur d'une fonction *)

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

(* "string_arg" renvoie un code julia qui trouve le "i"-ème argument passé en paramètre. *)

let string_arg i =
   "__arg(__plus_int(__args_p, " ^ (string_of_int (8 * i)) ^ "))"

(* "check_sig" renvoie un code julia qui teste si les paramètres données à une fonction correspondent à la signature "s". *)

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

(* "is_specialised" teste si la signature f1 est une généralisation de la signature f2 *)

let rec is_specialised f1 f2 = match f1, f2 with
| [], [] -> true
| x1 :: r1, x2 :: r2 -> (x1 = "Any" || x1 = x2) && is_specialised r1 r2
| _ -> false

(* "jl_dispatch" renvoie un code julia qui sélectionne la bonne fonction à appeler suivant le type de ses paramètres. *)

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

(* "code_dispatch" s'occupe de générer le code du "dispatcher" de chaque nom de fonction, en julia. *)

let code_dispatch name l = 
   (* Il y a quelques fonctions qu'il ne faut pas dispatcher : ce sont les fonctions utilisées par les "dispatchers". *)
   
   if Filename.check_suffix name "__uncheck" || name = "__fun___arg" || name = "__fun_print" || name = "__fun_println" then (
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

(* "loop_exprs" parcourt toutes les expressions *)

let rec loop_exprs = function
   | [] -> nop
   | x :: r -> let code = (match x with
      | DeclExpr(expr) -> code_expr (ref Global) expr
      | _ -> nop
   ) in 
   code ++ loop_exprs r   

(* "loop_fcts" parcourt les déclarations de fonctions *)

let rec loop_fcts = function
   | [] -> ()
   | x :: r -> (match x with
      | DeclFonction(nom, args, t, body) -> (
      	(* On génère le code de la fonction *)
         let corps = code_fct args body in
         
         (* On ajoute la signature et le code de la fonction dans les "dispatchers", qui seront générés par "code_dispatch", pour gérer les fonctions qui portent le même nom. *)
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

(* "loop_structs" s'occupe de créer un nouveau type pour chaque structure, et de générer le code julia du constructeur de la structure, et des "getters" (les fonctions qui permettent d'accéder aux champs de structure).

En effet, une construction du type point.x sera remplacé par x(point). Elle renvoie l'adresse de la case mémoire contenant le champ "point" de x. Ces "getters" passeront dans le dispatch multiple pour gérer le cas où deux structures ont des champs nommés de la même manière. *)

let rec loop_structs = function
   | [] -> ()
   | x :: r -> (match x with
      | DeclStructure(nom, mut, vars) -> (
      	(* Création du nouveau type *)
         let type_id = Smap.cardinal !types in
         types := Smap.add nom type_id !types;
         
         getters := !getters ^ 
         (* Création du constructeur *)
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
         
         (* Création des getters *)
         ^ (String.concat "\n" (List.mapi (fun id x ->
            "function __get_" ^ (fst x) ^ "(s :: " ^ nom ^ ")
               return __ref(s) + " ^ (string_of_int (8 + 8 * id))^ "
            end\n"
         ) vars))
      )
      | _ -> ()
   );
   loop_structs r 

(* "code_fichier" génère le code assembleur d'un Ast.fichier *)

let code_fichier f =
	(* On parcourt les structures *)
   loop_structs f;
   
   (* Pour chaque structure/type, on crée une variable du nom du type et qui vaut l'entier associé au type (voir "types").
   Elles servent en particulier pour tester les égalités de la forme :
   "typeof(x) == Bool" *)
   
   let code_exprs = loop_exprs (parse_str 
      (String.concat "\n" (
         Smap.fold (fun nom id l -> ((nom ^ " = " ^ (string_of_int id) ^ "\n") :: l)) !types []
      ))
   )
   (* On parcourt toutes les expressions *)
   ++ loop_exprs f in
   
   (* On génère le code des fonctions déclarées dans le fichier "f", des "getters" générés par "loop_struct", et de la librairie standard. Ces fonctions sont préfixées par "__fun_". *)
   loop_fcts f;
   loop_fcts (parse_str !getters);
   loop_fcts (parse_str standard_library);
   
   (* On génère les "dispatchers", qui portent le vrai nom des fonctions (sans "__fun_"). *)
   let functions = (Smap.fold (fun name l code -> (code_dispatch name l) ++ code) !dispatchers nop) in
   
   (* On renvoie le code final généré *)
   { text= 
   globl "main"
   ++ label "main"
   ++ pushq !%rbp
   ++ movq !%rsp !%rbp
   
   (* On crée de la place pour les variables globales, "r14" contient l'addresse de base des variables globales. Elles sont initialisées avec le pointeur "NULL" pour pouvoir testé si elles ont été écrites avant leur utilisation. *)
   ++ movq !%rsp !%r14
 	++ (List.fold_left (++) nop (list_init
 		(Smap.cardinal !globals) (fun x -> pushq (imm 0))
 	))
   
   (* Création de la variable nothing de type Nothing *)
   ++ set_nothing ()
   ++ movq !%rax (ind r14 ~ofs:(-8))
   
   ++ code_exprs
   
   ++ movq !%rbp !%rsp
   ++ popq rbp
   
   (* Le programme renvoie 0 s'il s'est exécuté correctement. *)
   ++ movq (imm 0) !%rax
   ++ ret
   
   ++ library ()
   ++ functions
   
   ; data= label "int_format" ++ string "%lld"
   ++ label "string_format" ++ string "%s"
   ++ !csts
   }

