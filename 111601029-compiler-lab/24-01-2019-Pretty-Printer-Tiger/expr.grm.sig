signature Expr_TOKENS =
sig
type ('a,'b) token
type svalue
val IF:  'a * 'a -> (svalue,'a) token
val RCUR:  'a * 'a -> (svalue,'a) token
val LCUR:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val NEWLINE:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val EQUALITY:  'a * 'a -> (svalue,'a) token
val GREATERTHAN:  'a * 'a -> (svalue,'a) token
val LESSTHAN:  'a * 'a -> (svalue,'a) token
val EQUAL:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val CONST: (int) *  'a * 'a -> (svalue,'a) token
end
signature Expr_LRVALS=
sig
structure Tokens : Expr_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
