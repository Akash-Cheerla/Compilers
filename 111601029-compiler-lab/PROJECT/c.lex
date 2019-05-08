
type pos = int             (* Position in file *)
type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token


val  lineRef : pos ref = ref 0
fun lineRange l r = "line " ^ l
fun updateLine n      = lineRef := !(lineRef) + n


fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt        = toSigned o String.explode


fun eof   ()      = Tokens.EOF (!lineRef,!lineRef)
fun error (e,l,r) = TextIO.output(TextIO.stdErr, lineRange l r ^ ":" ^ e ^ "\n")




%%
D  = [0-9];
L  = [a-zA-Z];
ws = [\ \t];

%header (functor CLexFun(structure Tokens : C_TOKENS));

%%

"//".*\n		=> ( continue() );

\n				=> ( updateLine 1; continue() );
{ws}+			=> ( continue() );

"-"				=> ( Tokens.MINUS (!lineRef, yypos) );
"+"				=> ( Tokens.PLUS (!lineRef, yypos) );
"*"				=> ( Tokens.MUL (!lineRef, yypos) );
"/"				=> ( Tokens.DIV (!lineRef, yypos) );
"%"				=> ( Tokens.MOD (!lineRef, yypos) );

"="				=> ( Tokens.ASSIGN (!lineRef, yypos) );
"-="			=> ( Tokens.MINUS_ASSIGN (!lineRef, yypos) );
"+="			=> ( Tokens.PLUS_ASSIGN (!lineRef, yypos) );
"*="			=> ( Tokens.MUL_ASSIGN (!lineRef, yypos) );
"/="			=> ( Tokens.DIV_ASSIGN (!lineRef, yypos) );
"%="			=> ( Tokens.MOD_ASSIGN (!lineRef, yypos) );

"!"				=> ( Tokens.NOT_OP (!lineRef, yypos) );
"<"				=> ( Tokens.LT_OP (!lineRef, yypos) );
">"				=> ( Tokens.GT_OP (!lineRef, yypos) );

"!="			=> ( Tokens.NE_OP (!lineRef, yypos) );
"<="			=> ( Tokens.LE_OP (!lineRef, yypos) );
">="			=> ( Tokens.GE_OP (!lineRef, yypos) );
"=="			=> ( Tokens.EQ_OP (!lineRef, yypos) );

"&&"			=> ( Tokens.AND_OP (!lineRef, yypos) );
"||"			=> ( Tokens.OR_OP (!lineRef, yypos) );

";"				=> ( Tokens.SCOLON (!lineRef, yypos) );
","				=> ( Tokens.COMMA (!lineRef, yypos) );
":"				=> ( Tokens.COLON (!lineRef, yypos) );

"{"				=> ( Tokens.L_BRACE (!lineRef, yypos) );
"}"				=> ( Tokens.R_BRACE (!lineRef, yypos) );
"("				=> ( Tokens.L_PAREN (!lineRef, yypos) );
")"				=> ( Tokens.R_PAREN (!lineRef, yypos) );

"int"			=> ( Tokens.INT (!lineRef, yypos) );
"void"			=> ( Tokens.VOID (!lineRef, yypos) );

"if"			=> ( Tokens.IF (!lineRef, yypos) );
"else"			=> ( Tokens.ELSE (!lineRef, yypos) );

"for"			=> ( Tokens.FOR (!lineRef, yypos) );
"while"			=> ( Tokens.WHILE (!lineRef, yypos) );

"continue"		=> ( Tokens.CONTINUE (!lineRef, yypos) );
"break"			=> ( Tokens.BREAK (!lineRef, yypos) );
"return"		=> ( Tokens.RETURN (!lineRef, yypos) );

{L}({L}|{D})*	=> ( Tokens.ID (yytext, !lineRef, yypos) );

{D}+			=> ( Tokens.CONST (toInt yytext, !lineRef, yypos) );





