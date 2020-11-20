%{
open Ast
%}

%token ELSE ELSEIF END FOR FUNCTION IF MUTABLE RETURN STRUCT WHILE

%token <string> IDENT
%token <string> IDENT_PARG
%token <int64> ENTIER_PARG
%token <int64 * string> ENTIER_IDENT
%token <string> PARD_IDENT

%token SEMI VIRGULE PARG PARD POINT EG_SIGNE TYPAGE TO
%token EOF

%token <int64> ENTIER
%token <string> CHAINE
%token <bool> BOOLEEN

%token PLUS MOINS FOIS MOD NON EGAL DIFF INF INFEGAL SUP SUPEGAL PUIS ET OU

%nonassoc inner
%nonassoc FOR WHILE IF RETURN ENTIER CHAINE BOOLEEN PARG IDENT_PARG IDENT ENTIER_PARG ENTIER_IDENT 
%right EG_SIGNE
%left OU
%left ET
%left EGAL DIFF SUP SUPEGAL INF INFEGAL
%left PLUS MOINS
%left FOIS MOD
%nonassoc NON unaire
%right PUIS
%left POINT
%nonassoc error


%start fichier

%type <Ast.fichier> fichier

%%




fichier:
| _decls = list(decl) EOF { _decls }
;




decl:
| _expr = expr; SEMI { DeclExpr(_expr) }
| _fonction = fonction { _fonction }
| _structure = structure { _structure }
;




param_bloc:
| { [] }
| _param = param { [_param] }
| _param = param; SEMI; _param_bloc = param_bloc { _param :: _param_bloc }
| SEMI; _param_bloc = param_bloc { _param_bloc }
;



structure:
| STRUCT; _name = IDENT; _params = param_bloc; END; SEMI {
   DeclStructure(_name, false, _params)
}
| MUTABLE; STRUCT; _name = IDENT; _params = param_bloc; END; SEMI {
   DeclStructure(_name, true, _params)
}
;




fonction:
| FUNCTION; _name = IDENT_PARG; _params = separated_list(VIRGULE, param); PARD; _bloc = bloc; END; SEMI {
   DeclFonction(_name, _params, None, ExprListe(_bloc))
}
| FUNCTION; _name = IDENT_PARG; _params = separated_list(VIRGULE, param); PARD; TYPAGE; _type = IDENT; _bloc = bloc; END; SEMI {
   DeclFonction(_name, _params, Some _type, ExprListe(_bloc))
}
;




param:
| _ident = IDENT { _ident, None }
| _ident = IDENT; TYPAGE; _type = IDENT { _ident, Some _type }
;




expr_sans_prefixe:
(* Expressions constantes *)
| _entier = ENTIER { ExprCst(CInt _entier) }
| _chaine = CHAINE { ExprCst(CString _chaine) }
| _booleen = BOOLEEN { ExprCst(CBool _booleen) }

(* Blocs parenthésés *)
| _entier_ident = ENTIER_IDENT { 
   ExprCall("@fois", [ExprCst(CInt (fst _entier_ident)); ExprAssignement(LvalueVar(snd _entier_ident), None)])
}
| _entier = ENTIER_PARG; _bloc = bloc_un; PARD { 
   ExprCall("@fois", [ExprCst(CInt _entier); ExprListe(_bloc)]) 
}
| PARG; _bloc = bloc_un; PARD { ExprListe(_bloc) }
| PARG; _expr = expr; _ident = PARD_IDENT { 
   ExprCall("@fois", [_expr; ExprAssignement(LvalueVar(_ident), None)])
}

(* Appel de fonction *)
| _ident = IDENT_PARG; _args = separated_list(VIRGULE, expr); PARD {
   ExprCall(_ident, _args)
}

(* Opérations arithmétiques sans préfixe *)
| _lexpr = expr_sans_prefixe; op = operateur; _rexpr = expr { ExprCall(op, [_lexpr; _rexpr]) }

(* Valeur unique *)
| _lvalue = lvalue { ExprAssignement(_lvalue, None) }

(* Opérations d'assignement sans préfixe *)
| _lvalue = lvalue; EG_SIGNE; _expr = expr { ExprAssignement(_lvalue, Some _expr) }

(* Return : également pour éviter les if return return : *)
| RETURN; _expr = expr %prec inner { ExprReturn(Some _expr) }
| RETURN %prec inner { ExprReturn(None) }

(* For *)
| FOR; _ident = IDENT; EG_SIGNE; _deb = expr; TO; _fin = expr; _bloc = bloc_sans_prefixe; END {
   ExprFor(_ident, _deb, _fin, ExprListe(_bloc))
}

(* While *)
| WHILE; _expr = expr; _bloc = bloc_sans_prefixe; END {
   ExprWhile(_expr, ExprListe(_bloc))
}

(* If *)
| IF; _expr = expr; _bloc = bloc_sans_prefixe; _else = else_bloc {
   ExprIfElse(_expr, ExprListe(_bloc), _else)
}

(* Non *)
| NON; _expr = expr { ExprCall("@non", [_expr]) }
;





expr_avec_prefixe:
(* Opérations arithmétiques avec préfixe *)
| _lexpr = expr_avec_prefixe; op = operateur; _rexpr = expr { ExprCall(op, [_lexpr; _rexpr]) }

(* Moins : c'est lui qui demande à être géré à part pour matcher la plus grande expression après un if *)
| MOINS; _expr = expr %prec unaire { ExprCall("@umoins", [_expr]) }
;




%inline expr:
| _expr = expr_avec_prefixe { _expr }
| _expr = expr_sans_prefixe { _expr }
;




%inline lvalue:
| _ident = IDENT { LvalueVar(_ident) }
| _expr = expr_sans_prefixe; POINT; _ident = IDENT { LvalueAttr(_expr, _ident) }
;




else_bloc:
| END { ExprListe([]) }
| ELSE; _bloc = bloc; END { ExprListe(_bloc) }
| ELSE; IF %prec error { failwith "else if -> elseif" }
| ELSEIF; _expr = expr; _bloc = bloc_sans_prefixe; _else = else_bloc { 
   ExprIfElse(_expr, ExprListe(_bloc), _else) 
}
;




%inline operateur:
| PLUS { "@plus" }
| MOINS { "@moins" }
| FOIS { "@fois" }
| MOD { "@div" }
| EGAL { "@egal" }
| DIFF { "@diff" }
| INF { "@inf" }
| INFEGAL { "@infegal" }
| SUP { "@sup" }
| SUPEGAL { "@supegal" }
| PUIS { "@puis" }
| ET { "@et" }
| OU { "@ou" }
;





bloc_sans_prefixe:
| { [] }
| _expr = expr_sans_prefixe { [_expr] }
| _expr = expr_sans_prefixe; SEMI; _bloc = bloc { _expr :: _bloc }
| SEMI; _bloc = bloc { _bloc }
;




bloc:
| { [] }
| _expr = expr { [_expr] }
| _expr = expr; SEMI; _bloc = bloc { _expr :: _bloc }
| SEMI; _bloc = bloc { _bloc }
;




%inline bloc_un:
| _expr = expr { [_expr] }
| _expr = expr; SEMI; _bloc = bloc { _expr :: _bloc }
;
