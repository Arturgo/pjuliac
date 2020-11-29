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
