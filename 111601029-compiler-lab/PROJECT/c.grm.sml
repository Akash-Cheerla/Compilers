functor CLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : C_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "c.grm"*)
(*#line 12.1 "c.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\011\000\004\000\010\000\036\000\009\000\038\000\008\000\000\000\
\\001\000\004\000\012\000\000\000\
\\001\000\004\000\023\000\005\000\022\000\009\000\021\000\000\000\
\\001\000\008\000\013\000\000\000\
\\001\000\008\000\019\000\021\000\018\000\000\000\
\\001\000\008\000\044\000\016\000\040\000\017\000\039\000\018\000\038\000\
\\019\000\037\000\020\000\036\000\027\000\035\000\028\000\034\000\
\\029\000\033\000\030\000\032\000\031\000\031\000\032\000\030\000\
\\033\000\029\000\034\000\028\000\035\000\027\000\000\000\
\\001\000\009\000\015\000\000\000\
\\001\000\009\000\016\000\000\000\
\\001\000\010\000\041\000\016\000\040\000\017\000\039\000\018\000\038\000\
\\019\000\037\000\020\000\036\000\027\000\035\000\028\000\034\000\
\\029\000\033\000\030\000\032\000\031\000\031\000\032\000\030\000\
\\033\000\029\000\034\000\028\000\035\000\027\000\000\000\
\\001\000\010\000\043\000\016\000\040\000\017\000\039\000\018\000\038\000\
\\019\000\037\000\020\000\036\000\027\000\035\000\028\000\034\000\
\\029\000\033\000\030\000\032\000\031\000\031\000\032\000\030\000\
\\033\000\029\000\034\000\028\000\035\000\027\000\000\000\
\\001\000\010\000\060\000\016\000\040\000\017\000\039\000\018\000\038\000\
\\019\000\037\000\020\000\036\000\027\000\035\000\028\000\034\000\
\\029\000\033\000\030\000\032\000\031\000\031\000\032\000\030\000\
\\033\000\029\000\034\000\028\000\035\000\027\000\000\000\
\\001\000\013\000\059\000\000\000\
\\001\000\013\000\061\000\000\000\
\\001\000\013\000\067\000\000\000\
\\001\000\014\000\064\000\000\000\
\\001\000\014\000\065\000\000\000\
\\001\000\014\000\069\000\000\000\
\\001\000\021\000\017\000\000\000\
\\071\000\000\000\
\\072\000\002\000\011\000\004\000\010\000\036\000\009\000\038\000\008\000\000\000\
\\073\000\000\000\
\\074\000\016\000\040\000\017\000\039\000\018\000\038\000\019\000\037\000\
\\020\000\036\000\027\000\035\000\028\000\034\000\029\000\033\000\
\\030\000\032\000\031\000\031\000\032\000\030\000\033\000\029\000\
\\034\000\028\000\035\000\027\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\018\000\038\000\019\000\037\000\020\000\036\000\000\000\
\\082\000\018\000\038\000\019\000\037\000\020\000\036\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\016\000\040\000\017\000\039\000\018\000\038\000\019\000\037\000\
\\020\000\036\000\028\000\034\000\029\000\033\000\030\000\032\000\
\\031\000\031\000\032\000\030\000\033\000\029\000\034\000\028\000\
\\035\000\027\000\000\000\
\\087\000\016\000\040\000\017\000\039\000\018\000\038\000\019\000\037\000\
\\020\000\036\000\000\000\
\\088\000\016\000\040\000\017\000\039\000\018\000\038\000\019\000\037\000\
\\020\000\036\000\000\000\
\\089\000\016\000\040\000\017\000\039\000\018\000\038\000\019\000\037\000\
\\020\000\036\000\028\000\034\000\029\000\033\000\031\000\031\000\
\\032\000\030\000\000\000\
\\090\000\016\000\040\000\017\000\039\000\018\000\038\000\019\000\037\000\
\\020\000\036\000\000\000\
\\091\000\016\000\040\000\017\000\039\000\018\000\038\000\019\000\037\000\
\\020\000\036\000\000\000\
\\092\000\016\000\040\000\017\000\039\000\018\000\038\000\019\000\037\000\
\\020\000\036\000\028\000\034\000\029\000\033\000\031\000\031\000\
\\032\000\030\000\000\000\
\\093\000\016\000\040\000\017\000\039\000\018\000\038\000\019\000\037\000\
\\020\000\036\000\028\000\034\000\029\000\033\000\030\000\032\000\
\\031\000\031\000\032\000\030\000\033\000\029\000\000\000\
\\094\000\016\000\040\000\017\000\039\000\018\000\038\000\019\000\037\000\
\\020\000\036\000\028\000\034\000\029\000\033\000\030\000\032\000\
\\031\000\031\000\032\000\030\000\033\000\029\000\034\000\028\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\037\000\066\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\"
val actionRowNumbers =
"\001\000\043\000\002\000\004\000\
\\020\000\019\000\007\000\008\000\
\\018\000\023\000\005\000\044\000\
\\021\000\003\000\003\000\003\000\
\\003\000\024\000\009\000\003\000\
\\027\000\026\000\010\000\022\000\
\\006\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\012\000\
\\011\000\013\000\025\000\042\000\
\\041\000\040\000\039\000\038\000\
\\037\000\036\000\035\000\034\000\
\\033\000\032\000\031\000\030\000\
\\029\000\001\000\028\000\001\000\
\\015\000\016\000\047\000\045\000\
\\014\000\001\000\017\000\046\000\
\\000\000"
val gotoT =
"\
\\002\000\068\000\003\000\005\000\004\000\004\000\005\000\003\000\
\\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\012\000\004\000\004\000\005\000\003\000\006\000\002\000\
\\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\018\000\000\000\
\\001\000\022\000\000\000\
\\001\000\023\000\000\000\
\\001\000\024\000\000\000\
\\000\000\
\\000\000\
\\001\000\040\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\043\000\000\000\
\\001\000\044\000\000\000\
\\001\000\045\000\000\000\
\\001\000\046\000\000\000\
\\001\000\047\000\000\000\
\\001\000\048\000\000\000\
\\001\000\049\000\000\000\
\\001\000\050\000\000\000\
\\001\000\051\000\000\000\
\\001\000\052\000\000\000\
\\001\000\053\000\000\000\
\\001\000\054\000\000\000\
\\001\000\055\000\000\000\
\\001\000\056\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\060\000\004\000\004\000\005\000\003\000\006\000\002\000\
\\007\000\001\000\000\000\
\\000\000\
\\003\000\061\000\004\000\004\000\005\000\003\000\006\000\002\000\
\\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\066\000\004\000\004\000\005\000\003\000\006\000\002\000\
\\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 69
val numrules = 29
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
datatype svalue = VOID' | ntVOID of unit ->  unit | CONST of unit ->  (int) | ID of unit ->  (string) | decs of unit ->  (Ast.stmt) | types of unit ->  (Ast.types) | ass_exp of unit ->  (Ast.stmt) | stmt of unit ->  (Ast.stmt) | stmts of unit ->  (Ast.stmt list) | program of unit ->  (Ast.stmt list) | exp of unit ->  (Ast.expr)
end
type svalue = MlyValue.svalue
type result = Ast.stmt list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 1) => true | (T 2) => true | (T 35) => true | (T 36) => true | (T 37) => true | (T 38) => true | (T 39) => true | (T 40) => true | (T 41) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 36))::
(nil
,nil
 $$ (T 8))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "INT"
  | (T 2) => "VOID"
  | (T 3) => "ID"
  | (T 4) => "CONST"
  | (T 5) => "COMMA"
  | (T 6) => "COLON"
  | (T 7) => "SCOLON"
  | (T 8) => "L_PAREN"
  | (T 9) => "R_PAREN"
  | (T 10) => "LBRACK"
  | (T 11) => "RBRACK"
  | (T 12) => "L_BRACE"
  | (T 13) => "R_BRACE"
  | (T 14) => "DOT"
  | (T 15) => "PLUS"
  | (T 16) => "MINUS"
  | (T 17) => "MUL"
  | (T 18) => "DIV"
  | (T 19) => "MOD"
  | (T 20) => "ASSIGN"
  | (T 21) => "PLUS_ASSIGN"
  | (T 22) => "MINUS_ASSIGN"
  | (T 23) => "MUL_ASSIGN"
  | (T 24) => "DIV_ASSIGN"
  | (T 25) => "MOD_ASSIGN"
  | (T 26) => "NOT_OP"
  | (T 27) => "LT_OP"
  | (T 28) => "GT_OP"
  | (T 29) => "NE_OP"
  | (T 30) => "LE_OP"
  | (T 31) => "GE_OP"
  | (T 32) => "EQ_OP"
  | (T 33) => "AND_OP"
  | (T 34) => "OR_OP"
  | (T 35) => "IF"
  | (T 36) => "ELSE"
  | (T 37) => "WHILE"
  | (T 38) => "FOR"
  | (T 39) => "BREAK"
  | (T 40) => "CONTINUE"
  | (T 41) => "RETURN"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 4) => MlyValue.CONST(fn () => (1)) | 
_ => MlyValue.VOID'
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.stmts stmts1, stmts1left, stmts1right)) :: rest671)) => let val  result = MlyValue.program (fn _ => let val  (stmts as stmts1) = stmts1 ()
 in ((*#line 60.32 "c.grm"*) stmts (*#line 338.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, stmts1left, stmts1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.stmt stmt1, stmt1left, stmt1right)) :: rest671)) => let val  result = MlyValue.stmts (fn _ => let val  (stmt as stmt1) = stmt1 ()
 in ((*#line 62.32 "c.grm"*) [stmt] (*#line 344.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, stmt1left, stmt1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.stmts stmts1, _, stmts1right)) :: ( _, ( MlyValue.stmt stmt1, stmt1left, _)) :: rest671)) => let val  result = MlyValue.stmts (fn _ => let val  (stmt as stmt1) = stmt1 ()
 val  (stmts as stmts1) = stmts1 ()
 in ((*#line 63.32 "c.grm"*) stmt :: stmts (*#line 350.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, stmt1left, stmts1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ass_exp (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 65.33 "c.grm"*) Ast.AssignC (Ast.Var ID, Ast.Assign, exp) (*#line 357.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, ID1left, exp1right), rest671)
end
|  ( 4, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.types (fn _ => ((*#line 67.32 "c.grm"*) Ast.INT (*#line 364.1 "c.grm.sml"*)
))
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 5, ( ( _, ( _, _, SCOLON1right)) :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( MlyValue.types types1, types1left, _)) :: rest671)) => let val  result = MlyValue.decs (fn _ => let val  (types as types1) = types1 ()
 val  (ID as ID1) = ID1 ()
 in ((*#line 69.38 "c.grm"*) Ast.DeclC (types, Ast.Var ID, Ast.Const(0)) (*#line 368.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, types1left, SCOLON1right), rest671)
end
|  ( 6, ( ( _, ( _, _, SCOLON1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( MlyValue.types types1, types1left, _)) :: rest671)) => let val  result = MlyValue.decs (fn _ => let val  (types as types1) = types1 ()
 val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 70.46 "c.grm"*) Ast.DeclC (types, Ast.Var ID, exp) (*#line 375.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, types1left, SCOLON1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 72.39 "c.grm"*) Ast.ID (Ast.Var ID) (*#line 383.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (CONST as CONST1) = CONST1 ()
 in ((*#line 73.39 "c.grm"*) Ast.Const CONST (*#line 389.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 9, ( ( _, ( _, _, R_PAREN1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, L_PAREN1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in ((*#line 74.39 "c.grm"*) exp (*#line 395.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, L_PAREN1left, R_PAREN1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 75.39 "c.grm"*) Ast.Binop (exp1, Ast.Plus, exp2) (*#line 401.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 76.39 "c.grm"*) Ast.Binop (exp1, Ast.Minus, exp2) (*#line 408.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 77.39 "c.grm"*) Ast.Binop (exp1, Ast.Mul, exp2) (*#line 415.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 78.39 "c.grm"*) Ast.Binop (exp1, Ast.Div, exp2) (*#line 422.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 79.39 "c.grm"*) Ast.Binop (exp1, Ast.Mod, exp2) (*#line 429.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 80.39 "c.grm"*) Ast.Binop (exp1, Ast.Not_Op, exp2) (*#line 436.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 81.39 "c.grm"*) Ast.Binop (exp1, Ast.Lt_Op, exp2) (*#line 443.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 82.39 "c.grm"*) Ast.Binop (exp1, Ast.Gt_Op, exp2) (*#line 450.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 83.39 "c.grm"*) Ast.Binop (exp1, Ast.NE_Op, exp2) (*#line 457.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 84.39 "c.grm"*) Ast.Binop (exp1, Ast.LE_Op, exp2) (*#line 464.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 85.38 "c.grm"*) Ast.Binop (exp1, Ast.GE_Op, exp2) (*#line 471.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 86.39 "c.grm"*) Ast.Binop (exp1, Ast.Eq_Op, exp2) (*#line 478.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 87.39 "c.grm"*) Ast.Binop (exp1, Ast.And_Op, exp2) (*#line 485.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 88.39 "c.grm"*) Ast.Binop (exp1, Ast.Or_Op, exp2) (*#line 492.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.decs decs1, decs1left, decs1right)) :: rest671)) => let val  result = MlyValue.stmt (fn _ => let val  (decs as decs1) = decs1 ()
 in ((*#line 90.35 "c.grm"*) decs (*#line 499.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, decs1left, decs1right), rest671)
end
|  ( 25, ( ( _, ( _, _, SCOLON1right)) :: ( _, ( MlyValue.ass_exp ass_exp1, ass_exp1left, _)) :: rest671)) => let val  result = MlyValue.stmt (fn _ => let val  (ass_exp as ass_exp1) = ass_exp1 ()
 in ((*#line 91.35 "c.grm"*) ass_exp (*#line 505.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, ass_exp1left, SCOLON1right), rest671)
end
|  ( 26, ( ( _, ( _, _, R_BRACE1right)) :: ( _, ( MlyValue.stmts stmts1, _, _)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.stmt (fn _ => let val  (exp as exp1) = exp1 ()
 val  (stmts as stmts1) = stmts1 ()
 in ((*#line 92.67 "c.grm"*) Ast.IF (exp, stmts) (*#line 511.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, IF1left, R_BRACE1right), rest671)
end
|  ( 27, ( ( _, ( _, _, R_BRACE2right)) :: ( _, ( MlyValue.stmts stmts2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.stmts stmts1, _, _)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.stmt (fn _ => let val  (exp as exp1) = exp1 ()
 val  stmts1 = stmts1 ()
 val  stmts2 = stmts2 ()
 in ((*#line 93.92 "c.grm"*) Ast.IF_ELSE (exp, stmts1, stmts2) (*#line 518.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, IF1left, R_BRACE2right), rest671)
end
|  ( 28, ( ( _, ( _, _, R_BRACE1right)) :: ( _, ( MlyValue.stmts stmts1, _, _)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.stmt (fn _ => let val  (exp as exp1) = exp1 ()
 val  (stmts as stmts1) = stmts1 ()
 in ((*#line 94.67 "c.grm"*) Ast.WHILE (exp, stmts) (*#line 526.1 "c.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, WHILE1left, R_BRACE1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID'
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : C_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID',p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID',p1,p2))
fun VOID (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID',p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID',p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID',p1,p2))
fun SCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID',p1,p2))
fun L_PAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID',p1,p2))
fun R_PAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID',p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID',p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID',p1,p2))
fun L_BRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID',p1,p2))
fun R_BRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID',p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID',p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID',p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID',p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID',p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID',p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID',p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID',p1,p2))
fun PLUS_ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID',p1,p2))
fun MINUS_ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID',p1,p2))
fun MUL_ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID',p1,p2))
fun DIV_ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID',p1,p2))
fun MOD_ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID',p1,p2))
fun NOT_OP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID',p1,p2))
fun LT_OP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID',p1,p2))
fun GT_OP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID',p1,p2))
fun NE_OP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID',p1,p2))
fun LE_OP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID',p1,p2))
fun GE_OP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID',p1,p2))
fun EQ_OP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID',p1,p2))
fun AND_OP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID',p1,p2))
fun OR_OP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID',p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID',p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.VOID',p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.VOID',p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(ParserData.MlyValue.VOID',p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(ParserData.MlyValue.VOID',p1,p2))
fun CONTINUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(ParserData.MlyValue.VOID',p1,p2))
fun RETURN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(ParserData.MlyValue.VOID',p1,p2))
end
end
