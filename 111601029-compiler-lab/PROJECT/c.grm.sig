signature C_TOKENS =
sig
type ('a,'b) token
type svalue
val RETURN:  'a * 'a -> (svalue,'a) token
val CONTINUE:  'a * 'a -> (svalue,'a) token
val BREAK:  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val OR_OP:  'a * 'a -> (svalue,'a) token
val AND_OP:  'a * 'a -> (svalue,'a) token
val EQ_OP:  'a * 'a -> (svalue,'a) token
val GE_OP:  'a * 'a -> (svalue,'a) token
val LE_OP:  'a * 'a -> (svalue,'a) token
val NE_OP:  'a * 'a -> (svalue,'a) token
val GT_OP:  'a * 'a -> (svalue,'a) token
val LT_OP:  'a * 'a -> (svalue,'a) token
val NOT_OP:  'a * 'a -> (svalue,'a) token
val MOD_ASSIGN:  'a * 'a -> (svalue,'a) token
val DIV_ASSIGN:  'a * 'a -> (svalue,'a) token
val MUL_ASSIGN:  'a * 'a -> (svalue,'a) token
val MINUS_ASSIGN:  'a * 'a -> (svalue,'a) token
val PLUS_ASSIGN:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val R_BRACE:  'a * 'a -> (svalue,'a) token
val L_BRACE:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val R_PAREN:  'a * 'a -> (svalue,'a) token
val L_PAREN:  'a * 'a -> (svalue,'a) token
val SCOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val CONST: (int) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val VOID:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature C_LRVALS=
sig
structure Tokens : C_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
