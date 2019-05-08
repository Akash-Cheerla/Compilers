(*type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error 
*)
type lexresult     = Tokens.token
fun eof   ()      = Tokens.EOF ()
val  lineNum  = ref 0   (* reference variable to keep track of position.
				    Typing not necessary just for clarity *)
val prevLineCol = ref 0
(*fun updateLine n      = lineRef := !(lineRef) + n*)

%%
whitespace=[\ \r\t];
digit=[0-9];
keyword="array"|"if"|"then"|"else"|"while"|"for"|"to"|"do"|"let"|"in"|"end"|"of"|
 		"break"|"nil"|"function"|"var"|"type"|"import"|"primitive";
symbol=","|":"|";"|"("|")"|"["|"]"|"{"|"}"|"."|"+"|"-"|"*"|"/"|"="|"<>"|"<"|
		"<="|">"|">="|"&"|"|"|":=";


%%
\n					=> (lineNum := !lineNum+1;prevLineCol := yypos;
					    Tokens.NEWLINE(yytext,!lineNum,yypos-(!prevLineCol)));				
{whitespace}+       => (Tokens.WHITESPACE(yytext,!lineNum,yypos-(!prevLineCol)));
{digit}+    		=> (Tokens.DIGIT(yytext,!lineNum,yypos-(!prevLineCol)));
{symbol}		    => (Tokens.SYMBOL(yytext,!lineNum,yypos-(!prevLineCol)));
{keyword}	  		=> (Tokens.KEYWORD(yytext,!lineNum,yypos-(!prevLineCol)));
[a-zA-Z]*           => (Tokens.ID(yytext,!lineNum,yypos-(!prevLineCol)));
.					=> (continue());