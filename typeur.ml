type fichier = decl list

and decl = 
(* Nom, estMutable, paramètres *)
| DeclStructure of string * bool * (string * string option) list
(* Nom, Paramètres sous la forme (arg, type), Type, et corps de la fonction *)
| DeclFonction of string * (string * string option) list * string option * expr
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





type typeEl = Any|Nothing|Int64|Bool|String|S

and exprTyp =
| ExprCstTyp of cst * typeEl
| ExprCallTyp of string * exprTyp list * typeEl
| ExprListeTyp of exprTyp list * typeEl
| ExprAssignementTyp of lvalue * exprTyp option * typeEl
| ExprReturnTyp of exprTyp option * typeEl
| ExprIfElseTyp of exprTyp * exprTyp * exprTyp * typeEl
| ExprForTyp of string * exprTyp * exprTyp * exprTyp * typeEl
| ExprWhileTyp of exprTyp * exprTyp * typeEl


type donne = typeEl list

module Ntmap = Map.Make(String)
let contextGeneral = Ntmap.empty


let peutAller a b=(a=Any||b=Any||a=b)

let typede = function
|ExprCstTyp(a,b )-> b
|ExprCallTyp(a,b,c)->c
|ExprListeTyp(a,b)->b
|ExprAssignementTyp(a,b,c)->c
|ExprReturnTyp(a,b)->b
|ExprIfElseTyp(a,b,c,d)->d
|ExprForTyp (a,b,c,d,e)->e
|ExprWhileTyp (a,b,c)->c

let rec typageExp context =function
|ExprCst(CInt n) -> ExprCstTyp (CInt n, Int64),context
|ExprCst(CBool n) -> ExprCstTyp (CBool n, Bool),context
|ExprCst(CString n) -> ExprCstTyp (CString n, String),context
|ExprCall(op, [exp1;exp2]) when(op="__fois" ||op="__plus"||op="__moins"||op="mod"||op="__puis"||op="div")->
                                  let a,contexta=(typageExp context exp1) in 
                                  let b,contextb=(typageExp contexta exp2) in
                                  if((peutAller(typede a) Int64)&&(peutAller(typede b) Int64))then 
                                  ExprCallTyp(op, [a;b], Int64),contextb
                                  else failwith "pas bon type pour l'arithmetique"
|ExprCall(op, [exp1;exp2]) when(op="__egal" ||op="__diff")->
                                  let a,contexta=(typageExp context exp1) in 
                                  let b,contextb=(typageExp contexta exp2) in
                                  ExprCallTyp(op, [a;b], Bool),contextb
|ExprCall("__non", [exp1]) ->let a,contexta=(typageExp context exp1) in 
                                if((peutAller(typede a) Bool))then 
                                  ExprCallTyp("__non", [a], Bool),contexta
                                  else failwith "pas bon type pour la négation"
|ExprCall("__umoins", [exp1]) ->let a,contexta=(typageExp context exp1) in 
                                if((peutAller(typede a) Int64))then 
                                  ExprCallTyp("__umoins", [a], Int64),contexta
                                  else failwith "pas bon type pour le moins unaire"
|ExprCall(op, [exp1;exp2]) when(op="__inf" ||op="__sup"||op="__infegal"||op="__supegal")->
                                  let a,contexta=(typageExp context exp1) in 
                                  let b,contextb=(typageExp contexta exp2) in
                                  if(((peutAller(typede a) Int64)||(peutAller(typede a) Bool))&&
                                     ((peutAller(typede b) Int64)||(peutAller(typede b) Bool)))then 
                                  ExprCallTyp(op, [a;b], Bool),contextb
                                  else failwith "pas bon type pour la comparaison"
|ExprCall(op, [exp1;exp2]) when(op="__et" ||op="__ou")->
                                  let a,contexta=(typageExp context exp1) in 
                                  let b,contextb=(typageExp contexta exp2) in
                                  if((peutAller(typede a) Bool)&&(peutAller(typede b) Bool))then 
                                  ExprCallTyp(op, [a;b], Bool),contextb
                                  else failwith "pas bon type pour l'arithmetique booléenne"
|_-> failwith "pas trouvé"


(*
| ExprCallTyp of string * expr list * typeEl
| ExprListeTyp of expr list * typeEl
| ExprAssignementTyp of lvalue * expr option * typeEl
| ExprReturnTyp of expr option * typeEl
| ExprIfElseTyp of expr * expr * expr * typeEl
| ExprForTyp of string * expr * expr * expr * typeEl
| ExprWhileTyp of expr * expr * typeEl
*)