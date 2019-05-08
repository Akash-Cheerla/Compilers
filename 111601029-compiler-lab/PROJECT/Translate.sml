structure Translate =
struct

fun Oper oper = case oper of
                      Ast.Plus   => "+"
                    | Ast.Minus  => "-"
                    | Ast.Mul    => "*"
                    | Ast.Div    => "/"
                    | Ast.Mod    => "%"
                    | Ast.Not_Op => "!"
                    | Ast.Lt_Op  => "<"
                    | Ast.Gt_Op  => ">"
                    | Ast.NE_Op  => "!="
                    | Ast.LE_Op  => "<="
                    | Ast.GE_Op  => ">="
                    | Ast.Eq_Op  => "=="
                    | Ast.And_Op => "&&"
                    | Ast.Or_Op  => "||"

fun AssignStr assign = case assign of
                          Ast.Assign       => "="
                        | Ast.Plus_Assign  => "+="
                        | Ast.Minus_Assign => "-="
                        | Ast.Mul_Assign   => "*="
                        | Ast.Div_Assign   => "/="
                        | Ast.Mod_Assign   => "%="

fun ExprStr (Ast.Const (c)) = Int.toString c
|   ExprStr (Ast.ID(Ast.Var(x))) = x
|   ExprStr (Ast.Binop (a,oper,b)) = (ExprStr a) ^" "^ (Oper oper) ^" "^ (ExprStr b)




fun compile []        = []
  | compile (x :: xs) = let 
                    fun compileStmt (Ast.DeclC(x_type, Ast.Var(x), exp)) = x ^ " = " ^ (ExprStr exp) ^ "\n" :: []

           |   compileStmt (Ast.AssignC(Ast.Var(x), assign, exp)) = x ^" "^ (AssignStr assign) ^" "^ (ExprStr exp) ^" \n" :: [] 

           |   compileStmt (Ast.IF(exp, stmts)) =  "if (" ^ (ExprStr exp) ^ "):\n" :: (map (fn x=>"    "^x) (compile stmts))

           |   compileStmt (Ast.IF_ELSE(exp, stmts_if, stmts_else)) = let
                                             val if_stmt = "if (" ^ (ExprStr exp) ^ "):\n" :: (map (fn x=>"    "^x) (compile stmts_if))
                                             val else_stmt = "else:\n" :: (map (fn x=>"    "^x) (compile stmts_else))
                                           in
                                             if_stmt @ else_stmt
                                           end

| compileStmt (Ast.WHILE(exp, stmts)) = "while (" ^ (ExprStr exp) ^ "):\n" :: (map (fn x=>"    "^x) (compile stmts))

                  in
                    (compileStmt x) @ (compile xs)
                  end
  
  
  
end
