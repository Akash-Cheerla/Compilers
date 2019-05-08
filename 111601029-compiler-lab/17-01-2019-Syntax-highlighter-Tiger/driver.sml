structure Parse =
struct 
  fun parse filename =
      let val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
	  val lexer = Mlex.makeLexer get
	  fun print_red x = print ("\027[31m"^x^"\027[0m")
	  fun print_white x = print ("\027[37m"^x^"\027[0m")
	  fun print_green x = print ("\027[32m"^x^"\027[0m")
	  fun print_yellow x = print ("\027[33m"^x^"\027[0m")
	  fun print_blue x = print ("\027[34m"^x^"\027[0m")
	  fun printToken (x,c) = (case c of
						"red" => (print_red x)
				      |  "green" => (print_green x)
				      |  "white" => (print_white x)
				      |  "blue" => (print_blue x)
				      |  "yellow" => (print_yellow x)
				      |  _ => (print ("")))
	  fun do_it() =
	      let val (x,i,j,c) = lexer()
	       in printToken (x,c);
		   if x="EOF" andalso j=0 then print ("\n") else do_it()
	      end
       in do_it();
	  TextIO.closeIn file
      end

end

