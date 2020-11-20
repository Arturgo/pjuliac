open Ast

let rec typageExp context =function
|ExprCst(CInt n) -> ExprCstTyp (CInt n, Int64)
