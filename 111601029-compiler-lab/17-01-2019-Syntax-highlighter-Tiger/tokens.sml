structure Tokens : Tiger_TOKENS =
struct
type linenum = int
type token = string*int*int*string
(*type color = string*)
fun NEWLINE(s,i,j)=(s,i,j,"white")
fun WHITESPACE(s,i,j)=(s,i,j,"white")
fun KEYWORD(s,i,j)=(s,i,j,"red")
fun SYMBOL(s,i,j)=(s,i,j,"blue")
fun DIGIT(s,i,j) = (s,i,j,"green")
fun ID(s,i,j) = (s,i,j,"yellow")
fun OTHER(s,i,j) =(s,i,j,"blue")
fun EOF() =("EOF",0,0," ")

end