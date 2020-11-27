type fichier = decl list

and decl = 
(* Nom, estMutable, paramètres *)
| DeclStructure of string * bool * (string * string) list
(* Nom, Paramètres sous la forme (arg, type), Type, et corps de la fonction *)
| DeclFonction of string * (string * string) list * string option * expr
| DeclExpr of expr

and cst = CInt of int64 | CString of string | CBool of bool

and expr =
| ExprCst of cst
| ExprCall of string * expr list
| ExprListe of expr list
| ExprAssignement of lvalue * expr option
| ExprReturn of expr option
| ExprIfElse of expr * expr * expr
| ExprFor of string * expr * expr * expr
| ExprWhile of expr * expr

and lvalue =
| LvalueVar of string
| LvalueAttr of expr * string


type typeEl = Any|Nothing|Int64|Bool|String|S of string
let exemple =[DeclFonction ("f", [("x1", "Any"); ("x2", "Int64")], None,
  ExprListe
   [ExprReturn (Some (ExprAssignement (LvalueVar "x1", None)))]);
 DeclFonction ("f", [("x1", "Any"); ("x5", "Int64")], None,
  ExprListe
   [ExprReturn (Some (ExprAssignement (LvalueVar "x1", None)))])]

type fonction = F of int * typeEl list*typeEl

type donne = Struct of bool * typeEl list|Fonctions of fonction list|Variable of int * typeEl 

module Ntmap = Map.Make(String)
let contextGeneral = Ntmap.empty

let peutAller a b=(a=Any||b=Any||a=b)
let vaAller a b=(a=b||a=Any)  (*une fonction de type a et un arg donné b *)

let n=ref 0
let estStruct=function
|Struct(_)->true
|_ ->false

let rec verifierType l1 l2 = 
  match(l1,l2) with
  |([],[])-> true
  |(a::b,c::d)->(peutAller a c)&&(verifierType b d)
  |_ -> false

let fctCompatible l =function
  |F(a,b,c) -> verifierType b l

let rec verifierTypeFort l1 l2 = 
  match(l1,l2) with
  |([],[])-> true
  |(a::b,c::d)->(vaAller a c)&&(verifierTypeFort b d)
  |_ -> false

let fctNecessaire l =function
  |F(a,b,c) -> verifierTypeFort b l

let rec toutType =function
  |[]->failwith "pas dechoix incomprehensible"
  |F(a,b,c)::[]->c
  |F(a,b,c)::d->if(toutType  d=c)then c else Any

let rec typage context nom = function
|[] -> []
|(v, "Any")::b -> Any :: (typage context nom b)
|(v, "Nothing")::b -> Nothing :: (typage context nom b)
|(v, "Int64")::b -> Int64 :: (typage context nom b)
|(v, "Bool")::b -> Bool :: (typage context nom b)
|(v, "String")::b -> String :: (typage context nom b)
|(v, n)::b when(n=nom||(Ntmap.mem n context)&&estStruct(Ntmap.find n context))-> S(n) :: (typage context nom b)
|(v, n)::b -> failwith "nom non défini"

let imposerType context = function
|Some el ->List.hd (typage context "" [("",el)])
|None -> Any

let rec uniques context = function
|[] -> true
|(nom, _)::b when(not (Ntmap.mem nom context))-> uniques (Ntmap.add nom "" context) b
|_ -> failwith "plusieurs fois le même nom"

(** to do ajouter les fcts print, div, println**)

let rec variablesGlobales context = function
| ExprCall (op, els) -> List.fold_left variablesGlobales context els
| ExprListe(els) -> List.fold_left variablesGlobales context els
| ExprAssignement(LvalueVar(nom), None) -> incr n; Ntmap.add nom (Variable(!n, Any)) context
| ExprAssignement(LvalueVar(nom), Some el) -> variablesGlobales (Ntmap.add nom (Variable(!n, Any)) context) el
| ExprReturn(Some el) -> variablesGlobales context el
| ExprIfElse(a,b,c) -> List.fold_left variablesGlobales context [a;b;c]
| ExprFor (a,b,c,d) -> List.fold_left variablesGlobales context [b;c]
| ExprWhile(a,b) -> variablesGlobales context a
| _ -> context


let toF=function
  |Fonctions(l) ->l
  |_ -> failwith "conflit de noms"

let rec calculerContext1 context =function
|[] -> context
|DeclStructure(nom, modif, types)::l when((uniques Ntmap.empty types) && not (Ntmap.mem nom context))-> 
calculerContext1 (Ntmap.add nom (Struct(modif, (typage context nom types))) context) l
|DeclFonction (nom, args, types, corps)::l when((uniques Ntmap.empty args) && nom<>"div"&&nom!="print"&&nom!="println")-> 
if(Ntmap.mem nom context) then (
  incr n; calculerContext1 (Ntmap.add nom 
  (Fonctions((F((!n),(typage context "" args),(imposerType context types)))::
  (toF((Ntmap.find nom context))))) context) l)
else(
  incr n; calculerContext1 (Ntmap.add nom 
  (Fonctions((F((!n),(typage context "" args),(imposerType context types)))::
  [])) context) l)

|DeclExpr (expr) ::l-> calculerContext1 (variablesGlobales context expr) l
|_ -> failwith "mauvais nom"

let contextGeneral = (calculerContext1 (Ntmap.add "nothing" (Variable ((-1),Nothing)) contextGeneral) exemple) 


let rec typageExp context =function
|ExprCst(CInt n) -> ExprCst (CInt n),Int64, context
|ExprCst(CBool n) -> ExprCst (CBool n),Bool,context
|ExprCst(CString n) -> ExprCst (CString n),String,context
|ExprCall(op, [exp1;exp2]) when(op="__fois" ||op="__plus"||op="__moins"||op="mod"||op="__puis"||op="div")->
let a,ta,contexta=(typageExp context exp1) in 
let b,tb,contextb=(typageExp contexta exp2) in
(if((peutAller ta Int64)&&(peutAller tb Int64))then 
ExprCall(op, [a;b]),Int64, contextb
else failwith "pas bon type pour l'arithmetique")
|ExprCall(op, [exp1;exp2]) when(op="__egal" ||op="__diff")->
let a,ta,contexta=(typageExp context exp1) in 
let b,tb,contextb=(typageExp contexta exp2) in
ExprCall(op, [a;b]),Bool, contextb
|ExprCall("__non", [exp1]) ->let a,ta,contexta=(typageExp context exp1) in 
(if((peutAller ta Bool))then 
ExprCall("__non", [a]),Bool,contexta
else failwith "pas bon type pour la négation")
|ExprCall("__umoins", [exp1]) ->let a,ta,contexta=(typageExp context exp1) in 
(if((peutAller ta Int64))then 
ExprCall("__umoins", [a]),Int64, contexta
else failwith "pas bon type pour le moins unaire")
|ExprCall(op, [exp1;exp2]) when(op="__inf" ||op="__sup"||op="__infegal"||op="__supegal")->
let a,ta,contexta=(typageExp context exp1) in 
let b,tb,contextb=(typageExp contexta exp2) in
(if(((peutAller ta  Int64)||(peutAller ta  Bool))&&
((peutAller tb  Int64)||(peutAller tb  Bool)))then 
ExprCall(op, [a;b]),Bool, contextb
else failwith "pas bon type pour la comparaison")
|ExprCall(op, [exp1;exp2]) when(op="__et" ||op="__ou")->
let a,ta,contexta=(typageExp context exp1) in 
let b,tb,contextb=(typageExp contexta exp2) in
(if((peutAller ta  Bool)&&(peutAller tb  Bool))then 
ExprCall(op, [a;b]),Bool, contextb
else (failwith "pas bon type pour l'arithmetique booléenne"))
|ExprCall(op, l) -> let (retour, types, contextN) = List.fold_left 
  (fun (ex, tex, cex) x -> let a,ta,contextA=(typageExp cex x) in (a::ex, ta::tex, contextA)) 
    ([], [], context) l in  
    let l1 = List.filter (fctCompatible types) (Ntmap.find op context) and
      l2 =List.filter (fctNecessaire types) (Ntmap.find op context) in
  if(l1=[]||(List.length l2)>1) then failwith "pas de compatibilité de fct"
  else 
    ExprCall(op, retour), toutType l1, contextN
| _ -> failwith "pas trouvé"

(*
| ExprCallTyp of string * expr list * typeEl
| ExprListeTyp of expr list * typeEl
| ExprAssignementTyp of lvalue * expr option * typeEl
| ExprReturnTyp of expr option * typeEl
| ExprIfElseTyp of expr * expr * expr * typeEl
| ExprForTyp of string * expr * expr * expr * typeEl
| ExprWhileTyp of expr * expr * typeEl
*)
