structure Ast =
struct
    datatype var = Var of string;
    datatype types = INT;
    datatype binOpType = Plus | Minus | Mul | Div | Mod
                       | Not_Op | Lt_Op | Gt_Op
                       | NE_Op  | LE_Op | GE_Op | Eq_Op
                       | And_Op | Or_Op;
    
    datatype assignment = Assign | Plus_Assign | Minus_Assign | Mul_Assign | Div_Assign | Mod_Assign;
    
    datatype expr = Const of int
                  | ID of var
                  | Binop of expr * binOpType * expr;
    
    datatype stmt = DeclC of types * var * expr
                  | AssignC of var * assignment * expr
                  | IF of expr * stmt list
                  | IF_ELSE of expr * stmt list * stmt list
                  | WHILE of expr * stmt list;
    
    type program = stmt list;
end







                                      
