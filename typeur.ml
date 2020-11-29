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

type fonction = F of typeEl list*typeEl

type donne = Struct of bool * typeEl list|Fonctions of fonction list|Variable of int * typeEl 

module Ntmap = Map.Make(String)
let contextGeneral = Ntmap.empty

let peutAller a b=(a=Any||b=Any||a=b)
let vaAller a b=(a=b||a=Any)  (*une fonction de type a et un arg donné b *)

let n=ref 0
let nouv ()=
  n:=!n+1;!n

let estStruct=function
|Struct(_)->true
|_ ->false
let enFonc =function
|Fonctions(l) -> l
|_ -> failwith "pas une fonction "
let rec verifierType l1 l2 = 
  match(l1,l2) with
  |([],[])-> true
  |(a::b,c::d)->(peutAller a c)&&(verifierType b d)
  |_ -> false

let fctCompatible l =function
  |F(a,b) -> verifierType a l

let rec verifierTypeFort l1 l2 = 
  match(l1,l2) with
  |([],[])-> true
  |(a::b,c::d)->(vaAller a c)&&(verifierTypeFort b d)
  |_ -> false

let fctNecessaire l =function
  |F(a,b) -> verifierTypeFort a l

let rec toutType =function
  |[]->failwith "pas dechoix incomprehensible"
  |F(b,c)::[]->c
  |F(b,c)::d->if(toutType  d=c)then c else Any

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

let nomVariable nom num =
  String.concat "" [nom; string_of_int num]

(** to do ajouter les fcts print, div, println**)

let rec variablesExpression context = function
| ExprCall (op, els) -> List.fold_left variablesExpression context els
| ExprListe(els) -> List.fold_left variablesExpression context els
| ExprAssignement(LvalueVar(nom), None) -> context
| ExprAssignement(LvalueVar(nom), Some el) -> variablesExpression (Ntmap.add nom (Variable(!n, Any)) context) el
| ExprReturn(Some el) -> variablesExpression context el
| ExprIfElse(a,b,c) -> List.fold_left variablesExpression context [a;b;c]
| ExprFor (a,b,c,d) -> List.fold_left variablesExpression context [b;c]
| ExprWhile(a,b) -> variablesExpression context a
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
  calculerContext1 (Ntmap.add nom 
  (Fonctions((F((typage context "" args),(imposerType context types)))::
  (toF((Ntmap.find nom context))))) context) l)
else(
  calculerContext1 (Ntmap.add nom 
  (Fonctions((F((typage context "" args),(imposerType context types)))::
  [])) context) l)

|DeclExpr (expr) ::l-> calculerContext1 (variablesExpression context expr) l
|_ -> failwith "mauvais nom"

let contextGeneral = (calculerContext1 (Ntmap.add "nothing" (Variable ((-1),Nothing)) contextGeneral) exemple) 


let rec typageExp (context:(donne Ntmap.t)) =function
|ExprCst(CInt n) -> ExprCst (CInt n),Int64
|ExprCst(CBool n) -> ExprCst (CBool n),Bool
|ExprCst(CString n) -> ExprCst (CString n),String
|ExprCall(op, [exp1;exp2]) when(op="__fois" ||op="__plus"||op="__moins"||op="mod"||op="__puis"||op="div")->
let a,ta=(typageExp context exp1) in 
let b,tb=(typageExp context exp2) in
(if((peutAller ta Int64)&&(peutAller tb Int64))then 
ExprCall(op, [a;b]),Int64
else failwith "pas bon type pour l'arithmetique")
|ExprCall(op, [exp1;exp2]) when(op="__egal" ||op="__diff")->
let a,ta=(typageExp context exp1) in 
let b,tb=(typageExp context exp2) in
ExprCall(op, [a;b]),Bool
|ExprCall("__non", [exp1]) ->let a,ta=(typageExp context exp1) in 
(if((peutAller ta Bool))then 
ExprCall("__non", [a]),Bool
else failwith "pas bon type pour la négation")
|ExprCall("__umoins", [exp1]) ->let a,ta=(typageExp context exp1) in 
(if((peutAller ta Int64))then 
ExprCall("__umoins", [a]),Int64
else failwith "pas bon type pour le moins unaire")
|ExprCall(op, [exp1;exp2]) when(op="__inf" ||op="__sup"||op="__infegal"||op="__supegal")->
let a,ta=(typageExp context exp1) in 
let b,tb=(typageExp context exp2) in
(if(((peutAller ta  Int64)||(peutAller ta  Bool))&&
((peutAller tb  Int64)||(peutAller tb  Bool)))then 
ExprCall(op, [a;b]),Bool
else failwith "pas bon type pour la comparaison")
|ExprCall(op, [exp1;exp2]) when(op="__et" ||op="__ou")->
let a,ta=(typageExp context exp1) in 
let b,tb=(typageExp context exp2) in
(if((peutAller ta  Bool)&&(peutAller tb  Bool))then 
ExprCall(op, [a;b]),Bool
else (failwith "pas bon type pour l'arithmetique booléenne"))
|ExprCall(op, l) -> let (retour, types) = List.fold_left 
  (fun (ex, tex) x -> let a,ta=(typageExp context x) in (a::ex, ta::tex)) 
    ([], []) l in  
    let l1 = List.filter (fctCompatible types) (enFonc(Ntmap.find op context))and
      l2 =List.filter (fctNecessaire types) (enFonc(Ntmap.find op context)) in
  if(l1=[]||(List.length l2)>1) then failwith "pas de compatibilité de fct"
  else 
    ExprCall(op, retour), toutType l1
|ExprListe(l) -> let (retour, types) = List.fold_left 
  (fun (ex, tex) x -> let a,ta=(typageExp context x) in (a::ex, ta)) 
    ([], Any) l in ExprListe(retour), types
|ExprReturn(Some a) -> ExprReturn(Some a),Any
|ExprReturn(None) -> ExprReturn(None),Any
|ExprIfElse(e1,e2,e3) -> let ee1, b = typageExp  context e1 in if(peutAller b Bool) then 
  let ee2, b2=typageExp context e2 and ee3, b3=typageExp context e3 in
  ExprIfElse(ee1,ee2,ee3), (if(b2=b3)then b2 else Any)
else failwith "c'est un if mais pas de type bool"
|ExprFor(nom, e1,e2,corps) -> let ee1,b1=typageExp context e1 and ee2,b2=typageExp context e2 in
  if(peutAller b1 Int64 && peutAller b2 Int64) then
  (incr n;let a=(!n) in
  let e,t=typageExp (variablesExpression (Ntmap.add nom (Variable (a,Int64)) context) corps) corps in
    ExprFor(nomVariable nom a, ee1,ee2,e),Nothing
  )else
   failwith "iterateurs pas entiers"
|ExprWhile(condition,corps) -> let el, tp = typageExp context condition in 
  if(peutAller tp Bool) then 
    let e,t=typageExp (variablesExpression context corps) corps in
      ExprWhile(el, e), Nothing
  else
    failwith "condition while pas booléenne"
| _ -> failwith "pas trouvé"

(*
| ExprAssignement of lvalue * expr option
*)


(* let rec typageExp context =function
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
|ExprListe(l) -> let (retour, types, contextN) = List.fold_left 
  (fun (ex, tex, cex) x -> let a,ta,contextA=(typageExp cex x) in (a::ex, ta, contextA)) 
    ([], Any, context) l in ExprListe(retour), types, contextN
|
| _ -> failwith "pas trouvé"
*)
