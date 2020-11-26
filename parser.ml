
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | VIRGULE
    | TYPAGE
    | TO
    | SUPEGAL
    | SUP
    | STRUCT
    | SEMI
    | RETURN
    | PUIS
    | POINT
    | PLUS
    | PARG
    | PARD_IDENT of (
# 11 "parser.mly"
       (string)
# 24 "parser.ml"
  )
    | PARD
    | OU
    | NON
    | MUTABLE
    | MOINS
    | MOD
    | INFEGAL
    | INF
    | IF
    | IDENT_PARG of (
# 8 "parser.mly"
       (string)
# 38 "parser.ml"
  )
    | IDENT of (
# 7 "parser.mly"
       (string)
# 43 "parser.ml"
  )
    | FUNCTION
    | FOR
    | FOIS
    | ET
    | EOF
    | ENTIER_PARG of (
# 9 "parser.mly"
       (string)
# 53 "parser.ml"
  )
    | ENTIER_IDENT of (
# 10 "parser.mly"
       (string * string)
# 58 "parser.ml"
  )
    | ENTIER of (
# 16 "parser.mly"
       (string)
# 63 "parser.ml"
  )
    | END
    | ELSEIF
    | ELSE
    | EG_SIGNE
    | EGAL
    | DIFF
    | CHAINE of (
# 17 "parser.mly"
       (string)
# 74 "parser.ml"
  )
    | BOOLEEN of (
# 18 "parser.mly"
       (bool)
# 79 "parser.ml"
  )
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState236
  | MenhirState234
  | MenhirState232
  | MenhirState220
  | MenhirState218
  | MenhirState215
  | MenhirState212
  | MenhirState207
  | MenhirState200
  | MenhirState194
  | MenhirState193
  | MenhirState189
  | MenhirState186
  | MenhirState185
  | MenhirState184
  | MenhirState179
  | MenhirState178
  | MenhirState173
  | MenhirState172
  | MenhirState171
  | MenhirState170
  | MenhirState169
  | MenhirState168
  | MenhirState166
  | MenhirState165
  | MenhirState162
  | MenhirState161
  | MenhirState157
  | MenhirState156
  | MenhirState155
  | MenhirState154
  | MenhirState152
  | MenhirState151
  | MenhirState149
  | MenhirState148
  | MenhirState146
  | MenhirState145
  | MenhirState141
  | MenhirState140
  | MenhirState137
  | MenhirState134
  | MenhirState133
  | MenhirState132
  | MenhirState129
  | MenhirState125
  | MenhirState124
  | MenhirState122
  | MenhirState121
  | MenhirState120
  | MenhirState119
  | MenhirState115
  | MenhirState114
  | MenhirState107
  | MenhirState106
  | MenhirState105
  | MenhirState104
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState98
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState48
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState30
  | MenhirState29
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState25
  | MenhirState22
  | MenhirState21
  | MenhirState20
  | MenhirState19
  | MenhirState18
  | MenhirState13
  | MenhirState12
  | MenhirState9
  | MenhirState7
  | MenhirState6
  | MenhirState5
  | MenhirState4
  | MenhirState3
  | MenhirState2
  | MenhirState1
  | MenhirState0

# 1 "parser.mly"
  
open Ast

# 253 "parser.ml"

let rec _menhir_goto_structure : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_structure -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1001) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_structure) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv999) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_structure : 'tv_structure) : 'tv_structure) = _v in
    ((let _v : 'tv_decl = 
# 55 "parser.mly"
                         ( _structure )
# 268 "parser.ml"
     in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1000)) : 'freshtv1002)

and _menhir_goto_separated_nonempty_list_VIRGULE_param_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_VIRGULE_param_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState212 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv993) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_VIRGULE_param_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv991) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_VIRGULE_param_) : 'tv_separated_nonempty_list_VIRGULE_param_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_VIRGULE_param__ = 
# 144 "<standard.mly>"
    ( x )
# 287 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_VIRGULE_param__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv992)) : 'freshtv994)
    | MenhirState215 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv997 * _menhir_state * 'tv_param)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_VIRGULE_param_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv995 * _menhir_state * 'tv_param)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_VIRGULE_param_) : 'tv_separated_nonempty_list_VIRGULE_param_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_param)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_VIRGULE_param_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 304 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_VIRGULE_param_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv996)) : 'freshtv998)
    | _ ->
        _menhir_fail ()

and _menhir_run153 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv989) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_else_bloc = 
# 189 "parser.mly"
      ( ExprListe([]) )
# 320 "parser.ml"
     in
    _menhir_goto_else_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv990)

and _menhir_run154 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154

and _menhir_run157 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | SEMI ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | END ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157

and _menhir_goto_fonction : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_fonction -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv987) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_fonction) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv985) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_fonction : 'tv_fonction) : 'tv_fonction) = _v in
    ((let _v : 'tv_decl = 
# 54 "parser.mly"
                       ( _fonction )
# 419 "parser.ml"
     in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv986)) : 'freshtv988)

and _menhir_goto_else_bloc : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_else_bloc -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv971 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_else_bloc) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv969 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_else : 'tv_else_bloc) : 'tv_else_bloc) = _v in
        ((let (((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_sans_prefixe)), _, (_bloc : 'tv_bloc_sans_prefixe)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_else_bloc = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 440 "parser.ml"
         in
        
# 191 "parser.mly"
                                                                     ( 
   ExprIfElse(_expr, ExprListe(_bloc), _else) 
)
# 447 "parser.ml"
         in
        _menhir_goto_else_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv970)) : 'freshtv972)
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv975 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_else_bloc) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv973 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_else : 'tv_else_bloc) : 'tv_else_bloc) = _v in
        ((let (((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_avec_prefixe)), _, (_bloc : 'tv_bloc_sans_prefixe)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_else_bloc = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 464 "parser.ml"
         in
        
# 191 "parser.mly"
                                                                     ( 
   ExprIfElse(_expr, ExprListe(_bloc), _else) 
)
# 471 "parser.ml"
         in
        _menhir_goto_else_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv974)) : 'freshtv976)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv979 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_else_bloc) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv977 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_else : 'tv_else_bloc) : 'tv_else_bloc) = _v in
        ((let (((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_sans_prefixe)), _, (_bloc : 'tv_bloc_sans_prefixe)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_expr_sans_prefixe = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 488 "parser.ml"
         in
        
# 149 "parser.mly"
                                                                 (
   ExprIfElse(_expr, ExprListe(_bloc), _else)
)
# 495 "parser.ml"
         in
        _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv978)) : 'freshtv980)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv983 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_else_bloc) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv981 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_else : 'tv_else_bloc) : 'tv_else_bloc) = _v in
        ((let (((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_avec_prefixe)), _, (_bloc : 'tv_bloc_sans_prefixe)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_expr_sans_prefixe = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 512 "parser.ml"
         in
        
# 149 "parser.mly"
                                                                 (
   ExprIfElse(_expr, ExprListe(_bloc), _else)
)
# 519 "parser.ml"
         in
        _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv982)) : 'freshtv984)
    | _ ->
        _menhir_fail ()

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_avec_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_goto_param_bloc : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_param_bloc -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState194 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv939 * _menhir_state) * _menhir_state * 'tv_param_bloc) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv937 * _menhir_state) * _menhir_state * 'tv_param_bloc) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_param_bloc : 'tv_param_bloc)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_param_bloc = 
# 65 "parser.mly"
                                 ( _param_bloc )
# 1046 "parser.ml"
         in
        _menhir_goto_param_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv938)) : 'freshtv940)
    | MenhirState200 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv943 * _menhir_state * 'tv_param)) * _menhir_state * 'tv_param_bloc) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv941 * _menhir_state * 'tv_param)) * _menhir_state * 'tv_param_bloc) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_param : 'tv_param)), _, (_param_bloc : 'tv_param_bloc)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_param_bloc = 
# 64 "parser.mly"
                                                 ( _param :: _param_bloc )
# 1059 "parser.ml"
         in
        _menhir_goto_param_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv942)) : 'freshtv944)
    | MenhirState193 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv955 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1067 "parser.ml"
        )) * _menhir_state * 'tv_param_bloc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv951 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1077 "parser.ml"
            )) * _menhir_state * 'tv_param_bloc) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv947 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1087 "parser.ml"
                )) * _menhir_state * 'tv_param_bloc)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv945 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1094 "parser.ml"
                )) * _menhir_state * 'tv_param_bloc)) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s), (_name : (
# 7 "parser.mly"
       (string)
# 1099 "parser.ml"
                ))), _, (_params : 'tv_param_bloc)) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _1 = () in
                let _v : 'tv_structure = 
# 71 "parser.mly"
                                                         (
   DeclStructure(_name, false, _params)
)
# 1109 "parser.ml"
                 in
                _menhir_goto_structure _menhir_env _menhir_stack _menhir_s _v) : 'freshtv946)) : 'freshtv948)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv949 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1119 "parser.ml"
                )) * _menhir_state * 'tv_param_bloc)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv950)) : 'freshtv952)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv953 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1130 "parser.ml"
            )) * _menhir_state * 'tv_param_bloc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv954)) : 'freshtv956)
    | MenhirState207 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv967 * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 1139 "parser.ml"
        )) * _menhir_state * 'tv_param_bloc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv963 * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 1149 "parser.ml"
            )) * _menhir_state * 'tv_param_bloc) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv959 * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 1159 "parser.ml"
                )) * _menhir_state * 'tv_param_bloc)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv957 * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 1166 "parser.ml"
                )) * _menhir_state * 'tv_param_bloc)) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s), (_name : (
# 7 "parser.mly"
       (string)
# 1171 "parser.ml"
                ))), _, (_params : 'tv_param_bloc)) = _menhir_stack in
                let _6 = () in
                let _5 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_structure = 
# 74 "parser.mly"
                                                                  (
   DeclStructure(_name, true, _params)
)
# 1182 "parser.ml"
                 in
                _menhir_goto_structure _menhir_env _menhir_stack _menhir_s _v) : 'freshtv958)) : 'freshtv960)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv961 * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 1192 "parser.ml"
                )) * _menhir_state * 'tv_param_bloc)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv962)) : 'freshtv964)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv965 * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 1203 "parser.ml"
            )) * _menhir_state * 'tv_param_bloc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv966)) : 'freshtv968)
    | _ ->
        _menhir_fail ()

and _menhir_goto_param : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_param -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState207 | MenhirState193 | MenhirState200 | MenhirState194 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv927 * _menhir_state * 'tv_param) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv921 * _menhir_state * 'tv_param) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
            | SEMI ->
                _menhir_run194 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | END ->
                _menhir_reduce118 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState200) : 'freshtv922)
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv923 * _menhir_state * 'tv_param) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_param : 'tv_param)) = _menhir_stack in
            let _v : 'tv_param_bloc = 
# 63 "parser.mly"
                 ( [_param] )
# 1243 "parser.ml"
             in
            _menhir_goto_param_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv924)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv925 * _menhir_state * 'tv_param) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv926)) : 'freshtv928)
    | MenhirState215 | MenhirState212 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv935 * _menhir_state * 'tv_param) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | VIRGULE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv929 * _menhir_state * 'tv_param) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState215) : 'freshtv930)
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv931 * _menhir_state * 'tv_param) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_param)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_VIRGULE_param_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1278 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_VIRGULE_param_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv932)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv933 * _menhir_state * 'tv_param) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv934)) : 'freshtv936)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_decl -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv919 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState236
    | FUNCTION ->
        _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState236
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState236
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState236
    | MUTABLE ->
        _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState236
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState236
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState236
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState236
    | STRUCT ->
        _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState236
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState236
    | EOF ->
        _menhir_reduce110 _menhir_env (Obj.magic _menhir_stack) MenhirState236
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState236) : 'freshtv920)

and _menhir_goto_separated_nonempty_list_VIRGULE_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_VIRGULE_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv909) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_VIRGULE_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv907) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_VIRGULE_expr_) : 'tv_separated_nonempty_list_VIRGULE_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_VIRGULE_expr__ = 
# 144 "<standard.mly>"
    ( x )
# 1360 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_VIRGULE_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv908)) : 'freshtv910)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv913 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_VIRGULE_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv911 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_VIRGULE_expr_) : 'tv_separated_nonempty_list_VIRGULE_expr_) = _v in
        ((let ((_menhir_stack, _menhir_s, (_expr : 'tv_expr_sans_prefixe)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_VIRGULE_expr_ = let x = 
# 174 "parser.mly"
                            ( _expr )
# 1377 "parser.ml"
         in
        
# 243 "<standard.mly>"
    ( x :: xs )
# 1382 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_VIRGULE_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv912)) : 'freshtv914)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv917 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_VIRGULE_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv915 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_VIRGULE_expr_) : 'tv_separated_nonempty_list_VIRGULE_expr_) = _v in
        ((let ((_menhir_stack, _menhir_s, (_expr : 'tv_expr_avec_prefixe)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_VIRGULE_expr_ = let x = 
# 173 "parser.mly"
                            ( _expr )
# 1399 "parser.ml"
         in
        
# 243 "<standard.mly>"
    ( x :: xs )
# 1404 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_VIRGULE_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv916)) : 'freshtv918)
    | _ ->
        _menhir_fail ()

and _menhir_goto_bloc_sans_prefixe : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_bloc_sans_prefixe -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv857 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1419 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv853 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1429 "parser.ml"
            ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv851 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1436 "parser.ml"
            ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), (_ident : (
# 7 "parser.mly"
       (string)
# 1441 "parser.ml"
            ))), _, (_expr : 'tv_expr_sans_prefixe)), _), _, (_expr_inlined1 : 'tv_expr_sans_prefixe)), _, (_bloc : 'tv_bloc_sans_prefixe)) = _menhir_stack in
            let _8 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _fin =
              let _expr = _expr_inlined1 in
              
# 174 "parser.mly"
                            ( _expr )
# 1452 "parser.ml"
              
            in
            let _deb = 
# 174 "parser.mly"
                            ( _expr )
# 1458 "parser.ml"
             in
            
# 139 "parser.mly"
                                                                                              (
   ExprFor(_ident, _deb, _fin, ExprListe(_bloc))
)
# 1465 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv852)) : 'freshtv854)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv855 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1475 "parser.ml"
            ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv856)) : 'freshtv858)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv865 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1484 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv861 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1494 "parser.ml"
            ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv859 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1501 "parser.ml"
            ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), (_ident : (
# 7 "parser.mly"
       (string)
# 1506 "parser.ml"
            ))), _, (_expr : 'tv_expr_sans_prefixe)), _), _, (_expr_inlined1 : 'tv_expr_avec_prefixe)), _, (_bloc : 'tv_bloc_sans_prefixe)) = _menhir_stack in
            let _8 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _fin =
              let _expr = _expr_inlined1 in
              
# 173 "parser.mly"
                            ( _expr )
# 1517 "parser.ml"
              
            in
            let _deb = 
# 174 "parser.mly"
                            ( _expr )
# 1523 "parser.ml"
             in
            
# 139 "parser.mly"
                                                                                              (
   ExprFor(_ident, _deb, _fin, ExprListe(_bloc))
)
# 1530 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv860)) : 'freshtv862)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv863 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1540 "parser.ml"
            ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv864)) : 'freshtv866)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv873 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1549 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv869 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1559 "parser.ml"
            ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv867 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1566 "parser.ml"
            ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), (_ident : (
# 7 "parser.mly"
       (string)
# 1571 "parser.ml"
            ))), _, (_expr : 'tv_expr_avec_prefixe)), _), _, (_expr_inlined1 : 'tv_expr_sans_prefixe)), _, (_bloc : 'tv_bloc_sans_prefixe)) = _menhir_stack in
            let _8 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _fin =
              let _expr = _expr_inlined1 in
              
# 174 "parser.mly"
                            ( _expr )
# 1582 "parser.ml"
              
            in
            let _deb = 
# 173 "parser.mly"
                            ( _expr )
# 1588 "parser.ml"
             in
            
# 139 "parser.mly"
                                                                                              (
   ExprFor(_ident, _deb, _fin, ExprListe(_bloc))
)
# 1595 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv868)) : 'freshtv870)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv871 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1605 "parser.ml"
            ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv872)) : 'freshtv874)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv881 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1614 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv877 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1624 "parser.ml"
            ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv875 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1631 "parser.ml"
            ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), (_ident : (
# 7 "parser.mly"
       (string)
# 1636 "parser.ml"
            ))), _, (_expr : 'tv_expr_avec_prefixe)), _), _, (_expr_inlined1 : 'tv_expr_avec_prefixe)), _, (_bloc : 'tv_bloc_sans_prefixe)) = _menhir_stack in
            let _8 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _fin =
              let _expr = _expr_inlined1 in
              
# 173 "parser.mly"
                            ( _expr )
# 1647 "parser.ml"
              
            in
            let _deb = 
# 173 "parser.mly"
                            ( _expr )
# 1653 "parser.ml"
             in
            
# 139 "parser.mly"
                                                                                              (
   ExprFor(_ident, _deb, _fin, ExprListe(_bloc))
)
# 1660 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv876)) : 'freshtv878)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv879 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1670 "parser.ml"
            ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv880)) : 'freshtv882)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv883 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | ELSEIF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | END ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152) : 'freshtv884)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv885 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | ELSEIF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | END ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156) : 'freshtv886)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv887 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | ELSEIF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | END ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162) : 'freshtv888)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv889 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | ELSEIF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | END ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166) : 'freshtv890)
    | MenhirState186 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv897 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv893 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv891 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_sans_prefixe)), _, (_bloc : 'tv_bloc_sans_prefixe)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 1756 "parser.ml"
             in
            
# 144 "parser.mly"
                                                      (
   ExprWhile(_expr, ExprListe(_bloc))
)
# 1763 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv892)) : 'freshtv894)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv895 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv896)) : 'freshtv898)
    | MenhirState189 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv905 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv901 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv899 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_avec_prefixe)), _, (_bloc : 'tv_bloc_sans_prefixe)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 1791 "parser.ml"
             in
            
# 144 "parser.mly"
                                                      (
   ExprWhile(_expr, ExprListe(_bloc))
)
# 1798 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv900)) : 'freshtv902)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv903 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv904)) : 'freshtv906)
    | _ ->
        _menhir_fail ()

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_bloc_sans_prefixe = 
# 220 "parser.mly"
  ( [] )
# 1816 "parser.ml"
     in
    _menhir_goto_bloc_sans_prefixe _menhir_env _menhir_stack _menhir_s _v

and _menhir_run122 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | SEMI ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | ELSE | ELSEIF | END ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122

and _menhir_goto_bloc : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_bloc -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv769 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv767 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_expr : 'tv_expr_avec_prefixe)), _), _, (_bloc : 'tv_bloc)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_bloc = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 1877 "parser.ml"
         in
        
# 232 "parser.mly"
                                   ( _expr :: _bloc )
# 1882 "parser.ml"
         in
        _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv768)) : 'freshtv770)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv773 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv771 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_expr : 'tv_expr_sans_prefixe)), _), _, (_bloc : 'tv_bloc)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_bloc = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 1895 "parser.ml"
         in
        
# 232 "parser.mly"
                                   ( _expr :: _bloc )
# 1900 "parser.ml"
         in
        _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv772)) : 'freshtv774)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv777 * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv775 * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_bloc : 'tv_bloc)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_bloc = 
# 233 "parser.mly"
                     ( _bloc )
# 1913 "parser.ml"
         in
        _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv776)) : 'freshtv778)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv785 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1921 "parser.ml"
        )) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv781 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1931 "parser.ml"
            )) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv779 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1938 "parser.ml"
            )) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (_entier : (
# 9 "parser.mly"
       (string)
# 1943 "parser.ml"
            ))), _, (_expr : 'tv_expr_sans_prefixe)), _), _, (_bloc : 'tv_bloc)) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : 'tv_expr_sans_prefixe = let _bloc =
              let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 1951 "parser.ml"
               in
              
# 241 "parser.mly"
                                   ( _expr :: _bloc )
# 1956 "parser.ml"
              
            in
            
# 112 "parser.mly"
                                               ( 
   ExprCall("@fois", [ExprCst(CInt _entier); ExprListe(_bloc)]) 
)
# 1964 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv780)) : 'freshtv782)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv783 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1974 "parser.ml"
            )) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv784)) : 'freshtv786)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv793 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1983 "parser.ml"
        )) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv789 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1993 "parser.ml"
            )) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv787 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 2000 "parser.ml"
            )) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (_entier : (
# 9 "parser.mly"
       (string)
# 2005 "parser.ml"
            ))), _, (_expr : 'tv_expr_avec_prefixe)), _), _, (_bloc : 'tv_bloc)) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : 'tv_expr_sans_prefixe = let _bloc =
              let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 2013 "parser.ml"
               in
              
# 241 "parser.mly"
                                   ( _expr :: _bloc )
# 2018 "parser.ml"
              
            in
            
# 112 "parser.mly"
                                               ( 
   ExprCall("@fois", [ExprCst(CInt _entier); ExprListe(_bloc)]) 
)
# 2026 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv788)) : 'freshtv790)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv791 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 2036 "parser.ml"
            )) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv792)) : 'freshtv794)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv797 * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv795 * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_bloc : 'tv_bloc)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_bloc_sans_prefixe = 
# 223 "parser.mly"
                     ( _bloc )
# 2050 "parser.ml"
         in
        _menhir_goto_bloc_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv796)) : 'freshtv798)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv801 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv799 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_expr : 'tv_expr_sans_prefixe)), _), _, (_bloc : 'tv_bloc)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_bloc_sans_prefixe = 
# 222 "parser.mly"
                                                ( _expr :: _bloc )
# 2063 "parser.ml"
         in
        _menhir_goto_bloc_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv800)) : 'freshtv802)
    | MenhirState157 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv809 * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv805 * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv803 * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_bloc : 'tv_bloc)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_else_bloc = 
# 190 "parser.mly"
                          ( ExprListe(_bloc) )
# 2084 "parser.ml"
             in
            _menhir_goto_else_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv804)) : 'freshtv806)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv807 * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv808)) : 'freshtv810)
    | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv817 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv813 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv811 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_sans_prefixe)), _), _, (_bloc : 'tv_bloc)) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _bloc =
              let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 2114 "parser.ml"
               in
              
# 241 "parser.mly"
                                   ( _expr :: _bloc )
# 2119 "parser.ml"
              
            in
            
# 115 "parser.mly"
                              ( ExprListe(_bloc) )
# 2125 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv812)) : 'freshtv814)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv815 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv816)) : 'freshtv818)
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv825 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv821 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv819 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_avec_prefixe)), _), _, (_bloc : 'tv_bloc)) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _bloc =
              let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 2155 "parser.ml"
               in
              
# 241 "parser.mly"
                                   ( _expr :: _bloc )
# 2160 "parser.ml"
              
            in
            
# 115 "parser.mly"
                              ( ExprListe(_bloc) )
# 2166 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv820)) : 'freshtv822)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv823 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv824)) : 'freshtv826)
    | MenhirState220 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv837 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2181 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 2185 "parser.ml"
        )) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv833 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2195 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 2199 "parser.ml"
            )) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv829 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2209 "parser.ml"
                )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 2213 "parser.ml"
                )) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv827 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2220 "parser.ml"
                )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 2224 "parser.ml"
                )) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
                ((let ((((((_menhir_stack, _menhir_s), (_name : (
# 8 "parser.mly"
       (string)
# 2229 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_VIRGULE_param__)), _), (_type : (
# 7 "parser.mly"
       (string)
# 2233 "parser.ml"
                ))), _, (_bloc : 'tv_bloc)) = _menhir_stack in
                let _9 = () in
                let _8 = () in
                let _5 = () in
                let _4 = () in
                let _1 = () in
                let _v : 'tv_fonction = let _params = 
# 232 "<standard.mly>"
    ( xs )
# 2243 "parser.ml"
                 in
                
# 86 "parser.mly"
                                                                                                                               (
   DeclFonction(_name, _params, Some _type, ExprListe(_bloc))
)
# 2250 "parser.ml"
                 in
                _menhir_goto_fonction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv828)) : 'freshtv830)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv831 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2260 "parser.ml"
                )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 2264 "parser.ml"
                )) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv832)) : 'freshtv834)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv835 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2275 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 2279 "parser.ml"
            )) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv836)) : 'freshtv838)
    | MenhirState218 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv849 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2288 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv845 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2298 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv841 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2308 "parser.ml"
                )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv839 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2315 "parser.ml"
                )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), (_name : (
# 8 "parser.mly"
       (string)
# 2320 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_VIRGULE_param__)), _, (_bloc : 'tv_bloc)) = _menhir_stack in
                let _7 = () in
                let _6 = () in
                let _4 = () in
                let _1 = () in
                let _v : 'tv_fonction = let _params = 
# 232 "<standard.mly>"
    ( xs )
# 2329 "parser.ml"
                 in
                
# 83 "parser.mly"
                                                                                                        (
   DeclFonction(_name, _params, None, ExprListe(_bloc))
)
# 2336 "parser.ml"
                 in
                _menhir_goto_fonction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv840)) : 'freshtv842)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv843 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2346 "parser.ml"
                )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv844)) : 'freshtv846)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv847 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2357 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv848)) : 'freshtv850)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr_avec_prefixe : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr_avec_prefixe -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv593 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv591 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2384 "parser.ml"
             in
            let op = 
# 203 "parser.mly"
      ( "@div" )
# 2389 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2394 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv592)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv594)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv597 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv595 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2417 "parser.ml"
             in
            let op = 
# 210 "parser.mly"
       ( "@puis" )
# 2422 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2427 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv596)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv598)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv601 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv599 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2450 "parser.ml"
             in
            let op = 
# 202 "parser.mly"
       ( "@fois" )
# 2455 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2460 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv600)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39) : 'freshtv602)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv605 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv603 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2487 "parser.ml"
             in
            let op = 
# 200 "parser.mly"
       ( "@plus" )
# 2492 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2497 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv604)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv606)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv609 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv607 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2520 "parser.ml"
             in
            let op = 
# 203 "parser.mly"
      ( "@div" )
# 2525 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2530 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv608)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv610)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv613 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv611 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2553 "parser.ml"
             in
            let op = 
# 202 "parser.mly"
       ( "@fois" )
# 2558 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2563 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv612)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46) : 'freshtv614)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv617 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv615 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2590 "parser.ml"
             in
            let op = 
# 201 "parser.mly"
        ( "@moins" )
# 2595 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2600 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv616)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv618)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv621 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv619 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2631 "parser.ml"
             in
            let op = 
# 208 "parser.mly"
      ( "@sup" )
# 2636 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2641 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv620)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv622)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv625 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv623 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2668 "parser.ml"
             in
            let op = 
# 200 "parser.mly"
       ( "@plus" )
# 2673 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2678 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv624)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv626)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv629 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv627 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2705 "parser.ml"
             in
            let op = 
# 201 "parser.mly"
        ( "@moins" )
# 2710 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2715 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv628)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv630)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv633 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv631 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2746 "parser.ml"
             in
            let op = 
# 207 "parser.mly"
          ( "@infegal" )
# 2751 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2756 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv632)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv634)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv637 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv635 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2787 "parser.ml"
             in
            let op = 
# 206 "parser.mly"
      ( "@inf" )
# 2792 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2797 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv636)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv638)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv641 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv639 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2828 "parser.ml"
             in
            let op = 
# 204 "parser.mly"
       ( "@egal" )
# 2833 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2838 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv640)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69) : 'freshtv642)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv645 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv643 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2869 "parser.ml"
             in
            let op = 
# 205 "parser.mly"
       ( "@diff" )
# 2874 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2879 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv644)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv646)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv649 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv647 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2922 "parser.ml"
             in
            let op = 
# 211 "parser.mly"
     ( "@et" )
# 2927 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2932 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv648)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv650)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv653 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv651 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 2963 "parser.ml"
             in
            let op = 
# 209 "parser.mly"
          ( "@supegal" )
# 2968 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 2973 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv652)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv654)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv657 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv655 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 3004 "parser.ml"
             in
            let op = 
# 208 "parser.mly"
      ( "@sup" )
# 3009 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 3014 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv656)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv658)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv661 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv659 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 3045 "parser.ml"
             in
            let op = 
# 207 "parser.mly"
          ( "@infegal" )
# 3050 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 3055 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv660)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv662)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv665 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv663 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 3086 "parser.ml"
             in
            let op = 
# 206 "parser.mly"
      ( "@inf" )
# 3091 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 3096 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv664)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv666)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv669 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv667 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 3127 "parser.ml"
             in
            let op = 
# 204 "parser.mly"
       ( "@egal" )
# 3132 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 3137 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv668)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv670)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv673 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv671 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 3168 "parser.ml"
             in
            let op = 
# 205 "parser.mly"
       ( "@diff" )
# 3173 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 3178 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv672)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv674)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv677 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | FOR | IDENT _ | IDENT_PARG _ | IF | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv675 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 3223 "parser.ml"
             in
            let op = 
# 212 "parser.mly"
     ( "@ou" )
# 3228 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 3233 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv676)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv678)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv681 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv679 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 3276 "parser.ml"
             in
            let op = 
# 211 "parser.mly"
     ( "@et" )
# 3281 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 3286 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv680)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv682)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv685 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 3298 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | FOR | IDENT _ | IDENT_PARG _ | IF | NON | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv683 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 3334 "parser.ml"
            ))) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (_expr_inlined1 : 'tv_expr_sans_prefixe)), _), (_ident : (
# 7 "parser.mly"
       (string)
# 3339 "parser.ml"
            ))), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _2 = () in
            let _2_inlined1 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 3346 "parser.ml"
             in
            let _lvalue =
              let (_2, _expr) = (_2_inlined1, _expr_inlined1) in
              
# 182 "parser.mly"
                                                   ( LvalueAttr(_expr, _ident) )
# 3353 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                           ( ExprAssignement(_lvalue, Some _expr) )
# 3359 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv684)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv686)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv689 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | FOR | IDENT _ | IDENT_PARG _ | IF | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv687 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 3404 "parser.ml"
             in
            let op = 
# 212 "parser.mly"
     ( "@ou" )
# 3409 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 3414 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv688)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv690)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv693 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv691 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 3437 "parser.ml"
             in
            let op = 
# 210 "parser.mly"
       ( "@puis" )
# 3442 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 3447 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv692)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv694)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv697 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv695 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 173 "parser.mly"
                            ( _expr )
# 3478 "parser.ml"
             in
            let op = 
# 209 "parser.mly"
          ( "@supegal" )
# 3483 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 3488 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv696)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv698)
    | MenhirState218 | MenhirState220 | MenhirState179 | MenhirState173 | MenhirState157 | MenhirState125 | MenhirState122 | MenhirState115 | MenhirState102 | MenhirState103 | MenhirState107 | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv703 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv699 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState106 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | SEMI ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | ELSE | ELSEIF | END | PARD ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv700)
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | ELSE | ELSEIF | END | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv701 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _v : 'tv_bloc = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 3578 "parser.ml"
             in
            
# 231 "parser.mly"
               ( [_expr] )
# 3583 "parser.ml"
             in
            _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv702)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106) : 'freshtv704)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv711 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 3595 "parser.ml"
        )) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv707 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 3623 "parser.ml"
            )) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState114 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv705 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 3631 "parser.ml"
            )) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, (_entier : (
# 9 "parser.mly"
       (string)
# 3637 "parser.ml"
            ))), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _3 = () in
            let _v : 'tv_expr_sans_prefixe = let _bloc =
              let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 3644 "parser.ml"
               in
              
# 240 "parser.mly"
               ( [_expr] )
# 3649 "parser.ml"
              
            in
            
# 112 "parser.mly"
                                               ( 
   ExprCall("@fois", [ExprCst(CInt _entier); ExprListe(_bloc)]) 
)
# 3657 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv706)) : 'freshtv708)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv709 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 3669 "parser.ml"
            )) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState114 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | SEMI ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | PARD ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv710)
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv712)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv713 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 3725 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | SEMI ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | END ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv714)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv717 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 3795 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | TO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv715 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 3831 "parser.ml"
            ))) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState132 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133) : 'freshtv716)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132) : 'freshtv718)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv719 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 3879 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | SEMI ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | END ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137) : 'freshtv720)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv723 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 3949 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | FOR | IDENT _ | IDENT_PARG _ | IF | NON | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv721 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 3985 "parser.ml"
            ))) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_ident : (
# 7 "parser.mly"
       (string)
# 3990 "parser.ml"
            ))), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 3996 "parser.ml"
             in
            let _lvalue = 
# 181 "parser.mly"
                 ( LvalueVar(_ident) )
# 4001 "parser.ml"
             in
            
# 132 "parser.mly"
                                           ( ExprAssignement(_lvalue, Some _expr) )
# 4006 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv722)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141) : 'freshtv724)
    | MenhirState7 | MenhirState149 | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv729 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | VIRGULE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv725 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState148 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149) : 'freshtv726)
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv727 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_VIRGULE_expr_ = let x = 
# 173 "parser.mly"
                            ( _expr )
# 4092 "parser.ml"
             in
            
# 241 "<standard.mly>"
    ( [ x ] )
# 4097 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_VIRGULE_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv728)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148) : 'freshtv730)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv731 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SEMI ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | ELSE | ELSEIF | END ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161) : 'freshtv732)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv733 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | SEMI ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | ELSE | ELSEIF | END ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165) : 'freshtv734)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv737 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv735 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 4252 "parser.ml"
             in
            
# 166 "parser.mly"
                                   ( ExprCall("@umoins", [_expr]) )
# 4257 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv736)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169) : 'freshtv738)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv741 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState171
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv739 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 4280 "parser.ml"
             in
            
# 154 "parser.mly"
                    ( ExprCall("@non", [_expr]) )
# 4285 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv740)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171) : 'freshtv742)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv753 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv745 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState178 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv743 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _bloc =
              let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 4331 "parser.ml"
               in
              
# 240 "parser.mly"
               ( [_expr] )
# 4336 "parser.ml"
              
            in
            
# 115 "parser.mly"
                              ( ExprListe(_bloc) )
# 4342 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv744)) : 'freshtv746)
        | PARD_IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv749 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState178 in
            let (_v : (
# 11 "parser.mly"
       (string)
# 4352 "parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv747 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((_ident : (
# 11 "parser.mly"
       (string)
# 4361 "parser.ml"
            )) : (
# 11 "parser.mly"
       (string)
# 4365 "parser.ml"
            )) = _v in
            ((let ((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 4372 "parser.ml"
             in
            
# 116 "parser.mly"
                                          ( 
   ExprCall("@fois", [_expr; ExprAssignement(LvalueVar(_ident), None)])
)
# 4379 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv748)) : 'freshtv750)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv751 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState178 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | SEMI ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | PARD ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179) : 'freshtv752)
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178) : 'freshtv754)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv757 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | FOR | IDENT _ | IDENT_PARG _ | IF | NON | PARD | PARD_IDENT _ | PARG | POINT | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv755 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 4478 "parser.ml"
             in
            
# 135 "parser.mly"
                                   ( ExprReturn(Some _expr) )
# 4483 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv756)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185) : 'freshtv758)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv759 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | SEMI ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | END ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189) : 'freshtv760)
    | MenhirState236 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv765 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | EGAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | ET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | INF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | INFEGAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | MOINS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | OU ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | PUIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv763 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState234 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv761 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (_expr : 'tv_expr_avec_prefixe)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_decl = let _expr = 
# 173 "parser.mly"
                            ( _expr )
# 4597 "parser.ml"
             in
            
# 53 "parser.mly"
                     ( DeclExpr(_expr) )
# 4602 "parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv762)) : 'freshtv764)
        | SUP ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | SUPEGAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState234) : 'freshtv766)
    | _ ->
        _menhir_fail ()

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_bloc = 
# 230 "parser.mly"
  ( [] )
# 4699 "parser.ml"
     in
    _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | SEMI ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ELSE | ELSEIF | END | PARD ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run21 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv587 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 7 "parser.mly"
       (string)
# 4797 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EG_SIGNE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv581 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 4808 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv582)
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | PUIS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv583 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 4850 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_expr : 'tv_expr_sans_prefixe)), _), (_ident : (
# 7 "parser.mly"
       (string)
# 4855 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr_sans_prefixe = let _lvalue = 
# 182 "parser.mly"
                                                   ( LvalueAttr(_expr, _ident) )
# 4861 "parser.ml"
             in
            
# 129 "parser.mly"
                   ( ExprAssignement(_lvalue, None) )
# 4866 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv584)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv585 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 4876 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv586)) : 'freshtv588)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv589 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv590)

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr_sans_prefixe -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_decl_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv575 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv571 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv569 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_decls : 'tv_list_decl_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 38 "parser.mly"
      (Ast.fichier)
# 5298 "parser.ml"
            ) = 
# 46 "parser.mly"
                          ( _decls )
# 5302 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv567) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 38 "parser.mly"
      (Ast.fichier)
# 5310 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv565) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 38 "parser.mly"
      (Ast.fichier)
# 5318 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv563) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 38 "parser.mly"
      (Ast.fichier)
# 5326 "parser.ml"
            )) : (
# 38 "parser.mly"
      (Ast.fichier)
# 5330 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv564)) : 'freshtv566)) : 'freshtv568)) : 'freshtv570)) : 'freshtv572)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv573 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv574)) : 'freshtv576)
    | MenhirState236 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv579 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv577 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_decl)), _, (xs : 'tv_list_decl_)) = _menhir_stack in
        let _v : 'tv_list_decl_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 5349 "parser.ml"
         in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv578)) : 'freshtv580)
    | _ ->
        _menhir_fail ()

and _menhir_reduce118 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_param_bloc = 
# 62 "parser.mly"
  ( [] )
# 5360 "parser.ml"
     in
    _menhir_goto_param_bloc _menhir_env _menhir_stack _menhir_s _v

and _menhir_run194 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
    | SEMI ->
        _menhir_run194 _menhir_env (Obj.magic _menhir_stack) MenhirState194
    | END ->
        _menhir_reduce118 _menhir_env (Obj.magic _menhir_stack) MenhirState194
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194

and _menhir_goto_loption_separated_nonempty_list_VIRGULE_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_VIRGULE_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv561 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 5388 "parser.ml"
    )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_expr__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PARD ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv557 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 5398 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_expr__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv555 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 5405 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_expr__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_ident : (
# 8 "parser.mly"
       (string)
# 5410 "parser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_VIRGULE_expr__)) = _menhir_stack in
        let _3 = () in
        let _v : 'tv_expr_sans_prefixe = let _args = 
# 232 "<standard.mly>"
    ( xs )
# 5416 "parser.ml"
         in
        
# 121 "parser.mly"
                                                                   (
   ExprCall(_ident, _args)
)
# 5423 "parser.ml"
         in
        _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv556)) : 'freshtv558)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv559 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 5433 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_expr__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv560)) : 'freshtv562)

and _menhir_goto_loption_separated_nonempty_list_VIRGULE_param__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_VIRGULE_param__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv553 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5445 "parser.ml"
    )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PARD ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv549 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5455 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | MOINS ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | SEMI ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | TYPAGE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv547 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5493 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState218 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv543 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5505 "parser.ml"
                )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state) = Obj.magic _menhir_stack in
                let (_v : (
# 7 "parser.mly"
       (string)
# 5510 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | BOOLEEN _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
                | CHAINE _v ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
                | ENTIER _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
                | ENTIER_IDENT _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
                | ENTIER_PARG _v ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
                | FOR ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState220
                | IDENT _v ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
                | IDENT_PARG _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
                | IF ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState220
                | MOINS ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState220
                | NON ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState220
                | PARG ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState220
                | RETURN ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState220
                | SEMI ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState220
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState220
                | END ->
                    _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState220
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState220) : 'freshtv544)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv545 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5559 "parser.ml"
                )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv546)) : 'freshtv548)
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | END ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState218) : 'freshtv550)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv551 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5578 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv552)) : 'freshtv554)

and _menhir_run195 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 5586 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TYPAGE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv537 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 5598 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv533 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 5608 "parser.ml"
            ))) = Obj.magic _menhir_stack in
            let (_v : (
# 7 "parser.mly"
       (string)
# 5613 "parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv531 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 5620 "parser.ml"
            ))) = Obj.magic _menhir_stack in
            let ((_type : (
# 7 "parser.mly"
       (string)
# 5625 "parser.ml"
            )) : (
# 7 "parser.mly"
       (string)
# 5629 "parser.ml"
            )) = _v in
            ((let (_menhir_stack, _menhir_s, (_ident : (
# 7 "parser.mly"
       (string)
# 5634 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_param = 
# 96 "parser.mly"
                                        ( _ident, Some _type )
# 5640 "parser.ml"
             in
            _menhir_goto_param _menhir_env _menhir_stack _menhir_s _v) : 'freshtv532)) : 'freshtv534)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv535 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 5650 "parser.ml"
            ))) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv536)) : 'freshtv538)
    | END | PARD | SEMI | VIRGULE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv539 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 5659 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_ident : (
# 7 "parser.mly"
       (string)
# 5664 "parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_param = 
# 95 "parser.mly"
                 ( _ident, None )
# 5669 "parser.ml"
         in
        _menhir_goto_param _menhir_env _menhir_stack _menhir_s _v) : 'freshtv540)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv541 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 5679 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv542)

and _menhir_goto_expr_sans_prefixe : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr_sans_prefixe -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 5693 "parser.ml"
        )) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv351 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 5721 "parser.ml"
            )) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState18 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv349 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 5729 "parser.ml"
            )) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, (_entier : (
# 9 "parser.mly"
       (string)
# 5735 "parser.ml"
            ))), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _3 = () in
            let _v : 'tv_expr_sans_prefixe = let _bloc =
              let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 5742 "parser.ml"
               in
              
# 240 "parser.mly"
               ( [_expr] )
# 5747 "parser.ml"
              
            in
            
# 112 "parser.mly"
                                               ( 
   ExprCall("@fois", [ExprCst(CInt _entier); ExprListe(_bloc)]) 
)
# 5755 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv353 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 5769 "parser.ml"
            )) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState18 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | SEMI ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | PARD ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv354)
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv356)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv359 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv357 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 5846 "parser.ml"
             in
            let op = 
# 209 "parser.mly"
          ( "@supegal" )
# 5851 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 5856 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv360)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv363 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv361 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 5881 "parser.ml"
             in
            let op = 
# 210 "parser.mly"
       ( "@puis" )
# 5886 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 5891 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv362)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv364)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv367 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5903 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | FOR | IDENT _ | IDENT_PARG _ | IF | NON | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv365 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5941 "parser.ml"
            ))) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (_expr_inlined1 : 'tv_expr_sans_prefixe)), _), (_ident : (
# 7 "parser.mly"
       (string)
# 5946 "parser.ml"
            ))), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _2 = () in
            let _2_inlined1 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 5953 "parser.ml"
             in
            let _lvalue =
              let (_2, _expr) = (_2_inlined1, _expr_inlined1) in
              
# 182 "parser.mly"
                                                   ( LvalueAttr(_expr, _ident) )
# 5960 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                           ( ExprAssignement(_lvalue, Some _expr) )
# 5966 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv366)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv368)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv371 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv369 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 5999 "parser.ml"
             in
            let op = 
# 208 "parser.mly"
      ( "@sup" )
# 6004 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6009 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv370)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv372)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv375 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv373 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6038 "parser.ml"
             in
            let op = 
# 200 "parser.mly"
       ( "@plus" )
# 6043 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6048 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv374)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv376)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv379 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv377 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6073 "parser.ml"
             in
            let op = 
# 203 "parser.mly"
      ( "@div" )
# 6078 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6083 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv378)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv380)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv383 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv381 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6108 "parser.ml"
             in
            let op = 
# 210 "parser.mly"
       ( "@puis" )
# 6113 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6118 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv382)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv384)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv387 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv385 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6143 "parser.ml"
             in
            let op = 
# 202 "parser.mly"
       ( "@fois" )
# 6148 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6153 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv386)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv388)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv391 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv389 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6178 "parser.ml"
             in
            let op = 
# 203 "parser.mly"
      ( "@div" )
# 6183 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6188 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv390)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv392)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv395 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv393 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6213 "parser.ml"
             in
            let op = 
# 202 "parser.mly"
       ( "@fois" )
# 6218 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6223 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv396)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv397 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6252 "parser.ml"
             in
            let op = 
# 201 "parser.mly"
        ( "@moins" )
# 6257 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6262 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv400)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv403 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6291 "parser.ml"
             in
            let op = 
# 200 "parser.mly"
       ( "@plus" )
# 6296 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6301 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv404)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv407 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv405 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6330 "parser.ml"
             in
            let op = 
# 201 "parser.mly"
        ( "@moins" )
# 6335 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6340 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv408)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | FOR | IDENT _ | IDENT_PARG _ | IF | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv409 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6387 "parser.ml"
             in
            let op = 
# 212 "parser.mly"
     ( "@ou" )
# 6392 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6397 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv410)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv412)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv415 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv413 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6430 "parser.ml"
             in
            let op = 
# 207 "parser.mly"
          ( "@infegal" )
# 6435 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6440 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv414)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv416)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv419 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv417 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6473 "parser.ml"
             in
            let op = 
# 206 "parser.mly"
      ( "@inf" )
# 6478 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6483 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv418)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv420)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv423 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv421 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6528 "parser.ml"
             in
            let op = 
# 211 "parser.mly"
     ( "@et" )
# 6533 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6538 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv422)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv424)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv427 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv425 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6571 "parser.ml"
             in
            let op = 
# 204 "parser.mly"
       ( "@egal" )
# 6576 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6581 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv428)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv431 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv429 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_sans_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6614 "parser.ml"
             in
            let op = 
# 205 "parser.mly"
       ( "@diff" )
# 6619 "parser.ml"
             in
            
# 126 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6624 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv430)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv432)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv435 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv433 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6657 "parser.ml"
             in
            let op = 
# 209 "parser.mly"
          ( "@supegal" )
# 6662 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6667 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv434)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv436)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv439 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv437 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6700 "parser.ml"
             in
            let op = 
# 208 "parser.mly"
      ( "@sup" )
# 6705 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6710 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv438)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv440)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv443 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv441 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6743 "parser.ml"
             in
            let op = 
# 207 "parser.mly"
          ( "@infegal" )
# 6748 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6753 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv444)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv447 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv445 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6786 "parser.ml"
             in
            let op = 
# 206 "parser.mly"
      ( "@inf" )
# 6791 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6796 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv446)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv448)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv451 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv449 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6829 "parser.ml"
             in
            let op = 
# 204 "parser.mly"
       ( "@egal" )
# 6834 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6839 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv450)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv452)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv455 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv453 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6872 "parser.ml"
             in
            let op = 
# 205 "parser.mly"
       ( "@diff" )
# 6877 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6882 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv454)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv456)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv459 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOR | IDENT _ | IDENT_PARG _ | IF | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv457 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6927 "parser.ml"
             in
            let op = 
# 211 "parser.mly"
     ( "@et" )
# 6932 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6937 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv458)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94) : 'freshtv460)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv463 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | FOR | IDENT _ | IDENT_PARG _ | IF | NON | OU | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv461 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_lexpr : 'tv_expr_avec_prefixe)), _), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _rexpr = 
# 174 "parser.mly"
                            ( _expr )
# 6984 "parser.ml"
             in
            let op = 
# 212 "parser.mly"
     ( "@ou" )
# 6989 "parser.ml"
             in
            
# 163 "parser.mly"
                                                            ( ExprCall(op, [_lexpr; _rexpr]) )
# 6994 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv462)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv464)
    | MenhirState218 | MenhirState220 | MenhirState179 | MenhirState173 | MenhirState157 | MenhirState125 | MenhirState122 | MenhirState115 | MenhirState102 | MenhirState107 | MenhirState105 | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv469 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv465 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState104 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | SEMI ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | ELSE | ELSEIF | END | PARD ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv466)
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | ELSE | ELSEIF | END | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv467 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _v : 'tv_bloc = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 7086 "parser.ml"
             in
            
# 231 "parser.mly"
               ( [_expr] )
# 7091 "parser.ml"
             in
            _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv468)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104) : 'freshtv470)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv473 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 7103 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | TO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv471 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 7141 "parser.ml"
            ))) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState119 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv472)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119) : 'freshtv474)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv475 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 7189 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | SEMI ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | END ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv476)
    | MenhirState189 | MenhirState186 | MenhirState165 | MenhirState161 | MenhirState155 | MenhirState151 | MenhirState137 | MenhirState134 | MenhirState129 | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv481 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv477 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState124 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | SEMI ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | ELSE | ELSEIF | END ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125) : 'freshtv478)
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | ELSE | ELSEIF | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv479 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _v : 'tv_bloc_sans_prefixe = 
# 221 "parser.mly"
                            ( [_expr] )
# 7341 "parser.ml"
             in
            _menhir_goto_bloc_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv480)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124) : 'freshtv482)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv483 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 7353 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | SEMI ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | END ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134) : 'freshtv484)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv487 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 7425 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | FOR | IDENT _ | IDENT_PARG _ | IF | NON | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv485 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 7463 "parser.ml"
            ))) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_ident : (
# 7 "parser.mly"
       (string)
# 7468 "parser.ml"
            ))), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 7474 "parser.ml"
             in
            let _lvalue = 
# 181 "parser.mly"
                 ( LvalueVar(_ident) )
# 7479 "parser.ml"
             in
            
# 132 "parser.mly"
                                           ( ExprAssignement(_lvalue, Some _expr) )
# 7484 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv486)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140) : 'freshtv488)
    | MenhirState149 | MenhirState146 | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv493 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | VIRGULE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv489 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState145 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146) : 'freshtv490)
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv491 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_VIRGULE_expr_ = let x = 
# 174 "parser.mly"
                            ( _expr )
# 7572 "parser.ml"
             in
            
# 241 "<standard.mly>"
    ( [ x ] )
# 7577 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_VIRGULE_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv492)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145) : 'freshtv494)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv495 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | SEMI ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | ELSE | ELSEIF | END ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151) : 'freshtv496)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv497 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | SEMI ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | ELSE | ELSEIF | END ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155) : 'freshtv498)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv501 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv499 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_avec_prefixe = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 7738 "parser.ml"
             in
            
# 166 "parser.mly"
                                   ( ExprCall("@umoins", [_expr]) )
# 7743 "parser.ml"
             in
            _menhir_goto_expr_avec_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv500)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168) : 'freshtv502)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv505 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv503 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 7768 "parser.ml"
             in
            
# 154 "parser.mly"
                    ( ExprCall("@non", [_expr]) )
# 7773 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv504)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170) : 'freshtv506)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv517 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv509 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState172 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv507 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _bloc =
              let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 7819 "parser.ml"
               in
              
# 240 "parser.mly"
               ( [_expr] )
# 7824 "parser.ml"
              
            in
            
# 115 "parser.mly"
                              ( ExprListe(_bloc) )
# 7830 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv508)) : 'freshtv510)
        | PARD_IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv513 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState172 in
            let (_v : (
# 11 "parser.mly"
       (string)
# 7840 "parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv511 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((_ident : (
# 11 "parser.mly"
       (string)
# 7849 "parser.ml"
            )) : (
# 11 "parser.mly"
       (string)
# 7853 "parser.ml"
            )) = _v in
            ((let ((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 7860 "parser.ml"
             in
            
# 116 "parser.mly"
                                          ( 
   ExprCall("@fois", [_expr; ExprAssignement(LvalueVar(_ident), None)])
)
# 7867 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv512)) : 'freshtv514)
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv515 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState172 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | SEMI ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | PARD ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173) : 'freshtv516)
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172) : 'freshtv518)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv521 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | BOOLEEN _ | CHAINE _ | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | FOR | IDENT _ | IDENT_PARG _ | IF | NON | PARD | PARD_IDENT _ | PARG | RETURN | SEMI | TO | VIRGULE | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv519 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr_sans_prefixe = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 7970 "parser.ml"
             in
            
# 135 "parser.mly"
                                   ( ExprReturn(Some _expr) )
# 7975 "parser.ml"
             in
            _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv520)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184) : 'freshtv522)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv523 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | SEMI ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | END ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186) : 'freshtv524)
    | MenhirState236 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv529 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIFF ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | EGAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | ET ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | FOIS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | INF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | INFEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | MOD ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | MOINS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | OU ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | POINT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | PUIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv527 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState232 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv525 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (_expr : 'tv_expr_sans_prefixe)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_decl = let _expr = 
# 174 "parser.mly"
                            ( _expr )
# 8093 "parser.ml"
             in
            
# 53 "parser.mly"
                     ( DeclExpr(_expr) )
# 8098 "parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv526)) : 'freshtv528)
        | SUP ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | SUPEGAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState232) : 'freshtv530)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState236 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState234 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState232 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState220 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv53 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 8135 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8139 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState218 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv55 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 8148 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_VIRGULE_param__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState215 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * 'tv_param)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState212 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 8162 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState207 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv61 * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 8171 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState200 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_param)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState194 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState193 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8190 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState189 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState186 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState184 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv77 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv81 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState171 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv93 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv97 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState157 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv103 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv109 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state * 'tv_bloc_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv121 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 8329 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv123 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 8338 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv125 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8347 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv127 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8356 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv129 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8365 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv131 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8374 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv133 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8383 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv141 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8407 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv143 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8416 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv145 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8425 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv147 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 8434 "parser.ml"
        )) * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv149 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 8443 "parser.ml"
        )) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 8477 "parser.ml"
        )) * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv163 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv165 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv167 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv169 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv171 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv173 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8511 "parser.ml"
        ))) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv175 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv177 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv179 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv181 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv183 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv185 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv189 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv191 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv195 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv197 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv199 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv201 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv203 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv207 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv209 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv211 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv213 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv215 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv217 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv219 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv221 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv223 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv225 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv227 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv229 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv231 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv233 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv235 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv239 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv241 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv243 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv245 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv247 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv249 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv250)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv253 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv255 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv259 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv261 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv265 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv269 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv270)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv271 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv273 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv275 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv277 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv279 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv281 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv283 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv285 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv287 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv289 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv291 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv292)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv293 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv294)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv295 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv297 * _menhir_state * 'tv_expr_avec_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv298)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv299 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_avec_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv301 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv303 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv305 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv309 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv310)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv311 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv313 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8865 "parser.ml"
        ))) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv315 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8874 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv319 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv321 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv323 * _menhir_state * 'tv_expr_sans_prefixe) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv325 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 8903 "parser.ml"
        )) * _menhir_state * 'tv_expr_sans_prefixe) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv327 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 8912 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv329 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 8921 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv331 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 8930 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv333 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 8939 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv334)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv335 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv337 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv338)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv339 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv341 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv342)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv343 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv344)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv345 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv346)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv348)

and _menhir_reduce110 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_decl_ = 
# 211 "<standard.mly>"
    ( [] )
# 8983 "parser.ml"
     in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run192 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 7 "parser.mly"
       (string)
# 9038 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
        | SEMI ->
            _menhir_run194 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | END ->
            _menhir_reduce118 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193) : 'freshtv44)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | DIFF | EGAL | ELSE | ELSEIF | END | ET | FOIS | INF | INFEGAL | MOD | OU | PARD | PARD_IDENT _ | PLUS | POINT | PUIS | SEMI | SUP | SUPEGAL | TO | VIRGULE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_expr_sans_prefixe = 
# 136 "parser.mly"
                     ( ExprReturn(None) )
# 9104 "parser.ml"
         in
        _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv42)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run205 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STRUCT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv33 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 7 "parser.mly"
       (string)
# 9208 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
            | SEMI ->
                _menhir_run194 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | END ->
                _menhir_reduce118 _menhir_env (Obj.magic _menhir_stack) MenhirState207
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState207) : 'freshtv34)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv35 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)) : 'freshtv38)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (string)
# 9320 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | PARD ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState7 in
        ((let _v : 'tv_loption_separated_nonempty_list_VIRGULE_expr__ = 
# 142 "<standard.mly>"
    ( [] )
# 9362 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_VIRGULE_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv32)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 9373 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EG_SIGNE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 9385 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | CHAINE _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | ENTIER _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | ENTIER_IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | ENTIER_PARG _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | FOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | IDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | IDENT_PARG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | IF ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | MOINS ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | NON ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | PARG ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | RETURN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv26)
    | BOOLEEN _ | CHAINE _ | DIFF | EGAL | ELSE | ELSEIF | END | ENTIER _ | ENTIER_IDENT _ | ENTIER_PARG _ | ET | FOIS | FOR | IDENT _ | IDENT_PARG _ | IF | INF | INFEGAL | MOD | MOINS | NON | OU | PARD | PARD_IDENT _ | PARG | PLUS | POINT | PUIS | RETURN | SEMI | SUP | SUPEGAL | TO | VIRGULE | WHILE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 9427 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_ident : (
# 7 "parser.mly"
       (string)
# 9432 "parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_expr_sans_prefixe = let _lvalue = 
# 181 "parser.mly"
                 ( LvalueVar(_ident) )
# 9437 "parser.ml"
         in
        
# 129 "parser.mly"
                   ( ExprAssignement(_lvalue, None) )
# 9442 "parser.ml"
         in
        _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 9452 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)

and _menhir_run211 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT_PARG _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "parser.mly"
       (string)
# 9469 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _v
        | PARD ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState212 in
            ((let _v : 'tv_loption_separated_nonempty_list_VIRGULE_param__ = 
# 142 "<standard.mly>"
    ( [] )
# 9484 "parser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_VIRGULE_param__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv20)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState212) : 'freshtv22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 7 "parser.mly"
       (string)
# 9511 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EG_SIGNE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv11 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 9522 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | CHAINE _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | ENTIER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | ENTIER_IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | ENTIER_PARG _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | FOR ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | IDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | IDENT_PARG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | IF ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | MOINS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | NON ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | PARG ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | RETURN ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv12)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 9566 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)) : 'freshtv16)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (string)
# 9581 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "parser.mly"
       (string * string)
# 9624 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_entier_ident : (
# 10 "parser.mly"
       (string * string)
# 9634 "parser.ml"
    )) : (
# 10 "parser.mly"
       (string * string)
# 9638 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr_sans_prefixe = 
# 109 "parser.mly"
                               ( 
   ExprCall("@fois", [ExprCst(CInt (fst _entier_ident)); ExprAssignement(LvalueVar(snd _entier_ident), None)])
)
# 9645 "parser.ml"
     in
    _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv10)

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "parser.mly"
       (string)
# 9652 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_entier : (
# 16 "parser.mly"
       (string)
# 9662 "parser.ml"
    )) : (
# 16 "parser.mly"
       (string)
# 9666 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr_sans_prefixe = 
# 104 "parser.mly"
                   ( ExprCst(CInt _entier) )
# 9671 "parser.ml"
     in
    _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "parser.mly"
       (string)
# 9678 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_chaine : (
# 17 "parser.mly"
       (string)
# 9688 "parser.ml"
    )) : (
# 17 "parser.mly"
       (string)
# 9692 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr_sans_prefixe = 
# 105 "parser.mly"
                   ( ExprCst(CString _chaine) )
# 9697 "parser.ml"
     in
    _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "parser.mly"
       (bool)
# 9704 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_booleen : (
# 18 "parser.mly"
       (bool)
# 9714 "parser.ml"
    )) : (
# 18 "parser.mly"
       (bool)
# 9718 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr_sans_prefixe = 
# 106 "parser.mly"
                     ( ExprCst(CBool _booleen) )
# 9723 "parser.ml"
     in
    _menhir_goto_expr_sans_prefixe _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and fichier : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 38 "parser.mly"
      (Ast.fichier)
# 9742 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | CHAINE _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ENTIER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ENTIER_IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ENTIER_PARG _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FOR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUNCTION ->
        _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IDENT_PARG _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MOINS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MUTABLE ->
        _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NON ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PARG ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RETURN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRUCT ->
        _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce110 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 9805 "parser.ml"
