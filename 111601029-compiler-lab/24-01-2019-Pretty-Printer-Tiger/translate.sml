structure Translate =
struct
fun printEmpty 0 =()
	| printEmpty (intend) =(print(" ");printEmpty(intend-1);())
fun compileExpr (Ast.Const x)         = print(Int.toString(x))
  | compileExpr (Ast.Variable x)     = print(x)
  | compileExpr (Ast.Op (x, oper, y)) = ( compileExpr x;
  											print (Ast.binOpToString oper);
  											compileExpr y; ())

fun compileStament (Ast.Ex x) = (compileExpr x;print(";\n");())

fun compileStaments (x::xs,intend) = (printEmpty(intend);compileStament x; compileStaments (xs,intend);())
	|compileStaments ([],intend)		= ()
    
fun compileCmpExpr (Ast.Cmp_Expr (x, oper , y))=(compileExpr x;
												print (Ast.cmpOpToString oper);
												compileExpr y;())
(*fun compileIf(x,Ast.Ifprt y::z,intend)=(printEmpty(intend);print("if (");
		compileCmpExpr x;print(")\n");printEmpty(intend);print("{\n");
		compileIf(y,intend+4);compileIf(z,intend+4);printEmpty(intend);print("}\n");())
	| compileIf(x,Ast.Stmnts x::z,intend)=(printEmpty(intend);print("if (");
		compileCmpExpr x;print(")\n");printEmpty(intend);print("{\n");
		compileStaments(x,intend+4);compileIf(z,intend+4);printEmpty(intend);print("}\n");())
	| compileIf(x,Ast.Stmnts x,intend)=(printEmpty(intend);print("if (");
		compileCmpExpr x;print(")\n");printEmpty(intend);print("{\n");
		compileStaments(x,intend+4);printEmpty(intend);print("}\n");())*)
(*val compilePart : Ast.Part * int -> unit*)
(*fun compilePartsIf (y::ys,intend)=(compilePart(y,intend);compilePartsIf (ys,intend);())
	| compilePartsIf ([],intend) = ()*)

fun compilePart (Ast.Stmnts x,intend) = (printEmpty(intend);print("{\n");compileStaments (x,intend+4);printEmpty(intend);print("}\n");())
	| compilePart (Ast.Ifprt (x,y),intend)=
	let fun compilePartsIf (y::ys,intend)=(compilePart(y,intend);compilePartsIf (ys,intend);())
		| compilePartsIf ([],intend) = ()
	in
		(printEmpty(intend);print("if (");
		compileCmpExpr x;print(")\n");printEmpty(intend);print("{\n");
		compilePartsIf (y,intend+4);printEmpty(intend);print("}\n");())
	end
	(*| compilePart (Ast.Ifprt (x,y),intend) = compileIf(y,intend)*)


fun compileParts (x::xs) =(compilePart (x,0); compileParts xs;())
	| compileParts [] = ()

fun compile (Ast.Prts x) = compileParts x

end
