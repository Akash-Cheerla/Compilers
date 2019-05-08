functor ExprLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Expr_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "expr.grm"*)(* This is the preamble where you can have arbitrary sml code. For us
it is empty *)


(*#line 15.1 "expr.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\014\000\002\000\013\000\012\000\012\000\000\000\
\\001\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\009\000\020\000\014\000\019\000\000\000\
\\001\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\013\000\039\000\014\000\019\000\000\000\
\\001\000\010\000\000\000\000\000\
\\001\000\015\000\029\000\000\000\
\\001\000\016\000\008\000\018\000\007\000\000\000\
\\001\000\016\000\018\000\000\000\
\\001\000\017\000\028\000\000\000\
\\001\000\017\000\041\000\000\000\
\\043\000\000\000\
\\044\000\016\000\008\000\018\000\007\000\000\000\
\\045\000\000\000\
\\046\000\000\000\
\\047\000\000\000\
\\048\000\000\000\
\\049\000\000\000\
\\050\000\001\000\014\000\002\000\013\000\012\000\012\000\000\000\
\\051\000\000\000\
\\052\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\014\000\019\000\000\000\
\\053\000\000\000\
\\054\000\000\000\
\\055\000\005\000\024\000\006\000\023\000\014\000\019\000\000\000\
\\056\000\005\000\024\000\006\000\023\000\014\000\019\000\000\000\
\\057\000\006\000\023\000\000\000\
\\058\000\000\000\
\\059\000\006\000\023\000\000\000\
\\060\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\014\000\019\000\000\000\
\\061\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\014\000\019\000\000\000\
\\062\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\014\000\019\000\000\000\
\\063\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\014\000\019\000\000\000\
\"
val actionRowNumbers =
"\005\000\013\000\009\000\010\000\
\\012\000\000\000\000\000\011\000\
\\006\000\001\000\000\000\020\000\
\\019\000\007\000\004\000\018\000\
\\005\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\002\000\015\000\016\000\
\\008\000\025\000\029\000\028\000\
\\027\000\026\000\023\000\022\000\
\\021\000\024\000\017\000\014\000\
\\003\000"
val gotoT =
"\
\\003\000\004\000\006\000\040\000\007\000\003\000\008\000\002\000\
\\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\004\000\007\000\003\000\008\000\007\000\009\000\001\000\000\000\
\\000\000\
\\001\000\009\000\010\000\008\000\000\000\
\\001\000\015\000\004\000\014\000\005\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\025\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\004\000\007\000\003\000\008\000\028\000\009\000\001\000\000\000\
\\001\000\029\000\000\000\
\\001\000\030\000\000\000\
\\001\000\031\000\000\000\
\\001\000\032\000\000\000\
\\001\000\033\000\000\000\
\\001\000\034\000\000\000\
\\001\000\035\000\000\000\
\\001\000\036\000\000\000\
\\000\000\
\\000\000\
\\001\000\015\000\004\000\014\000\005\000\038\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 41
val numrules = 21
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | ID of  (string) | CONST of  (int) | CMP_EXPR of  (Ast.CmpExpr) | IF_PART of  (Ast.Part) | PARTS of  (Ast.Part list) | PART of  (Ast.Part) | PROGRAM of  (Ast.Program) | STATEMENTS of  (Ast.Statement list) | STATEMENT of  (Ast.Statement) | SIMPLE_PART of  (Ast.Part) | EXPS of  (Ast.Expr list) | EXP of  (Ast.Expr)
end
type svalue = MlyValue.svalue
type result = Ast.Program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 9) => true | _ => false
val showTerminal =
fn (T 0) => "CONST"
  | (T 1) => "ID"
  | (T 2) => "PLUS"
  | (T 3) => "MINUS"
  | (T 4) => "MUL"
  | (T 5) => "EQUAL"
  | (T 6) => "LESSTHAN"
  | (T 7) => "GREATERTHAN"
  | (T 8) => "EQUALITY"
  | (T 9) => "EOF"
  | (T 10) => "NEWLINE"
  | (T 11) => "LPAR"
  | (T 12) => "RPAR"
  | (T 13) => "DIV"
  | (T 14) => "SEMICOLON"
  | (T 15) => "LCUR"
  | (T 16) => "RCUR"
  | (T 17) => "IF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PARTS PARTS, PARTS1left, PARTS1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 76.32 "expr.grm"*)Ast.Prts PARTS(*#line 230.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, PARTS1left, PARTS1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.PART PART, PART1left, PART1right)) :: rest671)) => let val  result = MlyValue.PARTS ((*#line 78.18 "expr.grm"*)[PART](*#line 234.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, PART1left, PART1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.PARTS PARTS, _, PARTS1right)) :: ( _, ( MlyValue.PART PART, PART1left, _)) :: rest671)) => let val  result = MlyValue.PARTS ((*#line 79.23 "expr.grm"*)PART :: PARTS(*#line 238.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, PART1left, PARTS1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.SIMPLE_PART SIMPLE_PART, SIMPLE_PART1left, SIMPLE_PART1right)) :: rest671)) => let val  result = MlyValue.PART ((*#line 81.22 "expr.grm"*)SIMPLE_PART:Ast.Part(*#line 242.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, SIMPLE_PART1left, SIMPLE_PART1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.IF_PART IF_PART, IF_PART1left, IF_PART1right)) :: rest671)) => let val  result = MlyValue.PART ((*#line 82.22 "expr.grm"*)IF_PART:Ast.Part(*#line 246.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, IF_PART1left, IF_PART1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RCUR1right)) :: ( _, ( MlyValue.PARTS PARTS, _, _)) :: _ :: ( _, ( MlyValue.CMP_EXPR CMP_EXPR, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.IF_PART ((*#line 84.39 "expr.grm"*)Ast.Ifprt(CMP_EXPR,PARTS)(*#line 250.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, IF1left, RCUR1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RCUR1right)) :: ( _, ( MlyValue.STATEMENTS STATEMENTS, _, _)) :: ( _, ( _, LCUR1left, _)) :: rest671)) => let val  result = MlyValue.SIMPLE_PART ((*#line 86.38 "expr.grm"*)Ast.Stmnts STATEMENTS(*#line 254.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, LCUR1left, RCUR1right), rest671)
end
|  ( 7, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.STATEMENT STATEMENT, STATEMENT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENTS ((*#line 88.46 "expr.grm"*)[STATEMENT](*#line 258.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, STATEMENT1left, SEMICOLON1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.STATEMENTS STATEMENTS, _, STATEMENTS1right)) :: _ :: ( _, ( MlyValue.STATEMENT STATEMENT, STATEMENT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENTS ((*#line 89.48 "expr.grm"*)STATEMENT :: STATEMENTS(*#line 262.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, STATEMENT1left, STATEMENTS1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 91.38 "expr.grm"*)Ast.Ex EXP(*#line 266.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, EXP1left, EXP1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 94.40 "expr.grm"*)Ast.Const CONST(*#line 270.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 95.17 "expr.grm"*)Ast.Variable ID(*#line 274.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 96.38 "expr.grm"*)Ast.plus EXP1 EXP2(*#line 278.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 97.38 "expr.grm"*)Ast.minus EXP1 EXP2(*#line 282.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 98.38 "expr.grm"*)Ast.mul EXP1 EXP2(*#line 286.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 99.38 "expr.grm"*)EXP(*#line 290.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 100.38 "expr.grm"*)Ast.div EXP1 EXP2(*#line 294.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 101.27 "expr.grm"*)Ast.equal EXP1 EXP2(*#line 298.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.CMP_EXPR ((*#line 103.30 "expr.grm"*)Ast.lessthan EXP1 EXP2(*#line 302.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.CMP_EXPR ((*#line 104.28 "expr.grm"*)Ast.greaterthan EXP1 EXP2(*#line 306.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.CMP_EXPR ((*#line 105.25 "expr.grm"*)Ast.equality EXP1 EXP2(*#line 310.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, EXP1left, EXP2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Expr_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.CONST i,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.ID i,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun EQUALITY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun NEWLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun LCUR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun RCUR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
end
end
