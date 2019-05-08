(*#line 54.10 "expr.lex"*)functor ExprLexFun(structure Tokens : Expr_TOKENS)(*#line 1.1 "expr.lex.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "expr.lex"*)(* Internal datatypes and functions required by the lexer *)
(* Keeping track of position in source                    *)

type lineNo            = int
type pos               = lineNo  (* The type of Should match with expr.yacc *)
val  lineRef : pos ref = ref 0   (* reference variable to keep track of position.
				    Typing not necessary just for clarity *)

fun updateLine n      = lineRef := !(lineRef) + n

(* Stuff done to make use of the Tokens module generated by expr.grm *)

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token


fun lineRange l r = "line " ^ l
				  (*else ("line " ^ Int.toString l ^ "-" ^ Int.toString r)*)
fun error (e,l,r) = TextIO.output(TextIO.stdErr, lineRange l r ^ ":" ^ e ^ "\n")

(*
   What to return at the end of the file. Note the extra (!pos,!pos). If you have
   the clause

   %term FOO of int  | BAR

   The token module will have two functions which are

   Token.FOO : int * pos * pos
   Token.BAR : pos * pos

   Here we give the eof function for the lexer which should return the
   EOF terminal to the parser.

*)
fun eof   ()      = Tokens.EOF (!lineRef,!lineRef)

(* Some helper functions during lexing *)

fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt        = toSigned o String.explode

val newlineCount = List.length o List.filter (fn x => x = #"\n") o String.explode

(*#line 58.1 "expr.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\000\000\000\000\000\000\000\000\000\013\014\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\013\000\000\011\000\000\000\000\010\009\008\007\000\006\000\005\
\\003\003\003\003\003\003\003\003\003\003\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (3, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (11, 
"\011\011\011\011\011\011\011\011\011\011\012\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\013\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\015\014\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\015\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 15)], trans = 3},
{fin = [(N 15)], trans = 3},
{fin = [(N 27)], trans = 0},
{fin = [(N 19)], trans = 0},
{fin = [(N 17)], trans = 0},
{fin = [(N 21)], trans = 0},
{fin = [(N 25)], trans = 0},
{fin = [(N 23)], trans = 0},
{fin = [], trans = 11},
{fin = [(N 3)], trans = 0},
{fin = [(N 6)], trans = 13},
{fin = [(N 10)], trans = 14},
{fin = [], trans = 14}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  10 => let val yytext=yymktext() in (*#line 60.19 "expr.lex"*) let val old = !lineRef
		   in updateLine (newlineCount yytext); Tokens.NEWLINE (old, !lineRef)
		   end
		 (*#line 204.1 "expr.lex.sml"*)
 end
| 15 => let val yytext=yymktext() in (*#line 64.19 "expr.lex"*) Tokens.CONST (toInt yytext, !lineRef, !lineRef) (*#line 206.1 "expr.lex.sml"*)
 end
| 17 => ((*#line 65.19 "expr.lex"*) Tokens.PLUS  (!lineRef,!lineRef) (*#line 208.1 "expr.lex.sml"*)
)
| 19 => ((*#line 66.19 "expr.lex"*) Tokens.MINUS  (!lineRef,!lineRef) (*#line 210.1 "expr.lex.sml"*)
)
| 21 => ((*#line 67.19 "expr.lex"*) Tokens.MUL (!lineRef,!lineRef) (*#line 212.1 "expr.lex.sml"*)
)
| 23 => ((*#line 68.15 "expr.lex"*) Tokens.LPAR (!lineRef,!lineRef) (*#line 214.1 "expr.lex.sml"*)
)
| 25 => ((*#line 69.15 "expr.lex"*) Tokens.RPAR (!lineRef,!lineRef) (*#line 216.1 "expr.lex.sml"*)
)
| 27 => ((*#line 70.10 "expr.lex"*) Tokens.DIV (!lineRef,!lineRef) (*#line 218.1 "expr.lex.sml"*)
)
| 3 => ((*#line 58.19 "expr.lex"*) updateLine 1; lex ()(*#line 220.1 "expr.lex.sml"*)
)
| 6 => ((*#line 59.19 "expr.lex"*) lex() (*#line 222.1 "expr.lex.sml"*)
)
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
