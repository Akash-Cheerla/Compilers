ctopy: ast.sml c.grm c.lex parser.sml driver.sml Translate.sml ctopy.mlb
	mllex c.lex
	mlyacc c.grm
	mlton ctopy.mlb

clean:
	rm -f *.lex.sml *.grm.* ctopy

