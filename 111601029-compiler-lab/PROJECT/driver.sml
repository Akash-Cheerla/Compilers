structure Driver =
struct 
  
  fun writeFile filename output =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, output)
        handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end

  
  
  
  val drive =
      let val inputFile = case CommandLine.arguments() of
              [x]  => x
            |  _   => (TextIO.output(TextIO.stdErr, "no file specified\n"); OS.Process.exit OS.Process.failure)
          val ast = Parser.parse inputFile
	      val py_code = Translate.compile ast
	      val _ = writeFile "out.py" (String.concat(py_code))
       in 
          print ("output printed successfully in out.py\n")
      end

end

