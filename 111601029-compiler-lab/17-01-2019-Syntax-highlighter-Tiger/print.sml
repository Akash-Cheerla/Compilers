structure Print =
struct
	fun print_red x = print ("\u001b[31m"^x)
	fun print_white x = print ("\u001b[37m"^x)
	fun print_green x = print ("\u001b[32m"^x)
	fun print_yellow x = print ("\u001b[33m"^x)
	fun print_black x = print ("\u001b[30m"^x)
	fun printToken (x,i,j,c) = case c of
						"red" => (print_red x;[])
				      |  "green" => (print_green x;[])
				      |  "white" => (print_white x;[])
				      |  "black" => (print_black x;[])
				      |  "yellow" => (print_yellow x;[]))
end