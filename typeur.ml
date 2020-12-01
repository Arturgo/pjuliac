(*type fichier = decl list
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
let ex=[DeclFonction ("f", [("n", "Any")], None,
  ExprListe
   [ExprFor ("n", ExprCst (CInt 1L),
     ExprAssignement (LvalueVar "n", None),
     ExprListe
      [ExprCall ("println",
        [ExprAssignement (LvalueVar "n", None)])])]);
 DeclExpr (ExprCall ("f", [ExprCst (CInt 5L)]))]


*)open Ast


type fonction = F of typeEl list*typeEl

type donne = Struct of bool * typeEl list|Fonctions of fonction list|Variable of bool * typeEl *int

module Ntmap = Map.Make(String)
let tousChamps = ref Ntmap.empty
let typeChamps = ref Ntmap.empty
let nomChamps = ref Ntmap.empty
let typeencours = ref Any
let glob=ref true

let recupVariable= function
|Variable(n, tp,num) -> n, tp,num
|_ -> failwith "fonction non attendue dans Lvalue"

let afficher = function
|Any->print_string "Any\n"
|Nothing->print_string "Nothing\n"
|Int64->print_string "Int64\n"
|Bool->print_string "Bool\n"
|S(n) -> print_string n;print_newline()
|String ->prerr_string "String\n"

let peutAller a b=(a=Any||b=Any||a=b)
let vaAller a b=(a=b||a=Any)  (*une fonction de type a et un arg donné b *)

let inf a b = if(a==Any) then b else a
let randInt ()=
  Random.int 10000

let smodifiable =function
|Struct(true, _) -> true
| _ -> false

let donnestruc = function
|Struct(a,b) -> b
|_ -> failwith "pas de structure"

let estStruct=function
|Struct(_)->true
|_ ->false
let enFonc =function
|Fonctions(l) -> l
|_ -> failwith "pas une fonction "
let toL =function
|F(a,b) ->a

let rec verifierType l1 l2 = 
  match(l1,l2) with
  |([],[])-> true
  |(a::b,c::d)->(peutAller a c)&&(verifierType b d)
  |_ -> false

let fctCompatible l =function
  |F(a,b) -> verifierType a l
let iterG a b c =let e,f =List.fold_left a b c in List.rev e,List.rev f

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

let first = function
|(a,b) -> a
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

let nomVariable nom num=
  if (num<>0)then
    String.concat "" [nom; string_of_int num]
  else
    nom
let premier =function
|Variable(glob, _,_)->glob
|_->failwith "pas une variable"

let ajoutV context nom obj=if(Ntmap.mem nom context) then (if !glob||(premier(Ntmap.find nom context)) then
  Ntmap.add nom (Variable(!glob, obj,  (randInt ()))) context else context)
  else
    Ntmap.add nom (Variable(!glob, obj,(randInt ()))) context
let ajoutV2 context nom obj=if(Ntmap.mem nom context) then (if !glob||(premier(Ntmap.find nom context)) then
  Ntmap.add nom (Variable(!glob, obj,  0)) context else context)
  else
    Ntmap.add nom (Variable(!glob, obj,0)) context


let rec ajoutVariables context=function
|[] -> context
|(a,b)::l -> 
ajoutVariables (ajoutV2 context a (imposerType context (Some b))) l 
(** to do ajouter les fcts print, println**)

let rec variablesExpression (context:donne Ntmap.t) = function
| ExprCall (op, els) -> List.fold_left variablesExpression context els
| ExprListe(els) -> List.fold_left variablesExpression context els
| ExprAssignement(LvalueVar(nom), None) -> context
| ExprAssignement(LvalueVar(nom), Some el) -> variablesExpression (ajoutV context nom Any) el
| ExprReturn(Some el) -> variablesExpression context el
| ExprIfElse(a,b,c) -> List.fold_left variablesExpression context [a;b;c]
| ExprFor (a,b,c,d) -> List.fold_left variablesExpression context [b;c]
| ExprWhile(a,b) -> variablesExpression context a
| _ -> context (*on ne peut pas créer de variable dans exprassignement si lvalue compliquee *)

let toF=function
  |Fonctions(l) ->l
  |_ -> failwith "conflit de noms"

let rec calculerContext1 context =function
|[] -> context
|DeclStructure(nom, modif, types)::l when((uniques Ntmap.empty types) && not (Ntmap.mem nom context))-> 
  let st=(Struct(modif, (typage context nom types))) in
  List.iter (fun (x,y)-> if(Ntmap.mem x !tousChamps)then failwith "champs deja present"else
    tousChamps:=(Ntmap.add x st !tousChamps);
    nomChamps:=(Ntmap.add x (S(nom)) !nomChamps);
    typeChamps:=(Ntmap.add x (List.hd (typage context "" [("",y)])) !typeChamps)) types;
calculerContext1 (Ntmap.add nom st context) l
|DeclFonction (nom, args, types, corps)::l when((uniques Ntmap.empty args) && nom<>"div"&&nom<>"print"&&nom<>"println")-> 
if(Ntmap.mem nom context) then (
  if(List.exists (fun (F(arg, t)) -> arg=(typage context "" args)) (toF((Ntmap.find nom context))))then
    failwith "plusieurs fcts indistinguables"
  else(
  calculerContext1 (Ntmap.add nom 
  (Fonctions((F((typage context "" args),(imposerType context types)))::
  (toF((Ntmap.find nom context))))) context) l))
else(
  calculerContext1 (Ntmap.add nom 
  (Fonctions((F((typage context "" args),(imposerType context types)))::
  [])) context) l)

|DeclExpr (expr) ::l-> calculerContext1 (variablesExpression context expr) l
|_ -> failwith "mauvais nom"

let rec less a b=
  match(a,b) with
    |([],[])-> true
    |(c::d,e::f) -> (vaAller e c)&&(less d f)
    |_ -> false

let rec meilleur av = function
|[] -> true
|(F(a,c))::b when(less a av)-> meilleur a b
|(F(a,c))::b when(less av a)-> meilleur av b
|_ -> false
let rec creerL n =if(n==0) then [] else Any::(creerL (n-1))

let modifiable context = function
|S(nom) -> smodifiable (Ntmap.find nom context)
|_ -> true

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
|ExprCall(op, l) when(op="print" || op="println") -> let (retour, types) = iterG 
  (fun (ex, tex) x -> let a,ta=(typageExp context x) in (a::ex, ta::tex)) 
    ([], []) l in  
    ExprCall(op, retour), Nothing

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
|ExprCall(op, l) when(not (Ntmap.mem op context)) -> failwith "operation inconnue"
|ExprCall(op, l) when( estStruct (Ntmap.find op context)) -> let s =donnestruc(Ntmap.find op context)
in let (retour, types) = iterG (*ajouter  *)
  (fun (ex, tex) x -> let a,ta=(typageExp context x) in (a::ex, ta::tex)) 
    ([], []) l in
  if(List.for_all2 peutAller s types) then ExprCall(op, retour), S(op)
  else
    failwith "constructeur incompatible"
|ExprCall(op, l) -> let (retour, types) = iterG (*ajouter  *)
  (fun (ex, tex) x -> let a,ta=(typageExp context x) in (a::ex, ta::tex)) 
    ([], []) l in  
    let l1 = List.filter (fctCompatible types) (enFonc(Ntmap.find op context))and
      l2 =List.filter (fctNecessaire types) (enFonc(Ntmap.find op context)) in
  if(l1=[]) then failwith "pas de fct compatible"
  else if ((List.length l2)>0 && not (meilleur (creerL (List.length (toL(List.hd l2)))) l2)) then
    failwith "choix impossible à faire entre 2 fcts"
  else 
    ExprCall(op, retour), toutType l1
|ExprListe(l) -> let (retour, types) = iterG 
  (fun (ex, tex) x -> let a,ta=(typageExp context x) in (a::ex, [ta])) 
    ([], [Any]) l in ExprListe(retour), List.hd types
|ExprReturn(Some el) -> let a,ta=typageExp context el in if(peutAller ta !typeencours)then
   ExprReturn(Some a),Any else failwith "retour de fonction incorrect"
|ExprReturn(None) -> if(peutAller Nothing !typeencours)then
   ExprReturn(None),Any else failwith "retour de fonction incorrect et vide"
|ExprIfElse(e1,e2,e3) -> let ee1, b = typageExp  context e1 in if(peutAller b Bool) then 
  let ee2, b2=typageExp context e2 and ee3, b3=typageExp context e3 in
  ExprIfElse(ee1,ee2,ee3), (if(b2=b3)then b2 else Any)
else failwith "c'est un if mais pas de type bool"
|ExprFor(nom, e1,e2,corps) -> let ee1,b1=typageExp context e1 and ee2,b2=typageExp context e2 in
  if(peutAller b1 Int64 && peutAller b2 Int64) then
  (let rep= !glob in glob:=false;let num= (randInt ()) in
  let e,t=typageExp (variablesExpression (Ntmap.add nom (Variable(false, Int64,num)) context) corps) corps in
    glob:=rep;ExprFor(nomVariable nom num, ee1,ee2,e),Nothing
  )else
   failwith "iterateurs pas entiers"
|ExprWhile(condition,corps) -> let el, tp = typageExp context condition in 
  if(peutAller tp Bool) then(
  let rep = !glob in glob:=false; 
    let e,t=typageExp (variablesExpression context corps) corps in
      glob:=rep;ExprWhile(el, e), Nothing)
  else
    failwith "condition while pas booléenne"
|ExprAssignement(lv, None) -> let vl, tval, modif =typageLvalue context lv in
  ExprAssignement(vl, None), tval
|ExprAssignement(lv, Some valeur) -> let el, tEl,modif = typageLvalue context lv in 
let vl, tval = typageExp context valeur in
  if(peutAller tEl tval) then
    if(modif) then
      ExprAssignement(el, Some vl), inf tEl tval
    else
      failwith "structure non mutable"
  else
    failwith "egalite types incompatibles"

and typageLvalue context= function
|LvalueVar(nom) when(Ntmap.mem nom context)-> 
  let gl, tp,num = recupVariable (Ntmap.find nom context) in
    LvalueVar(nomVariable nom num), tp,modifiable context tp
| LvalueAttr(gauche, nom) -> let el, tp = typageExp context gauche in
  if(Ntmap.mem nom !nomChamps)then(
  if(peutAller tp (Ntmap.find nom !nomChamps))then
    LvalueAttr(el, nom), Ntmap.find nom !typeChamps, smodifiable (Ntmap.find nom !tousChamps)
  else 
    failwith "la structure n'a pas le bon attribut")
  else 
    failwith "attribut introuvable"
| _ -> failwith "pas le bon type"

let rec calculerRep context = function
|[] -> []
|DeclStructure(a,b,c)::l -> (DeclStructure(a,b,c))::(calculerRep context l)
|DeclFonction(nom, args, types, corps)::l -> typeencours:= imposerType context types;
glob:=false;
let c = variablesExpression context corps in 
let ret, tp = typageExp (ajoutVariables c args) corps in
glob:=true;
if(peutAller tp !typeencours) then (typeencours:=Any;
(DeclFonction(nom, args, types, ret))::(calculerRep context l))
else 
  failwith "retour implicite de fonction illegal"
|DeclExpr(ex)::l -> (DeclExpr(first(typageExp context ex)))::(calculerRep context l)

let calculerTypage arbre=
tousChamps := Ntmap.empty;
 typeChamps := Ntmap.empty;
 nomChamps := Ntmap.empty;
 typeencours := Any;
  let contextGeneral1 = Ntmap.add "nothing" (Variable (true,Nothing, 0)) (calculerContext1  Ntmap.empty arbre) in
  let rep = calculerRep contextGeneral1 arbre in
  (*assert(rep=arbre);*)rep
