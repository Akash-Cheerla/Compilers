(*this is the type of each production *)
type RHS = Atom.atom list

fun len (x::xs,l) = len(xs,l+1)
	| len ([],l) = l

structure RHS_KEY : ORD_KEY = struct
	type ord_key = RHS
	fun compare (x::xs,y::ys) = (case Atom.compare(x,y) of
							GREATER => GREATER
							| LESS => LESS
							| EQUAL => (compare (xs,ys)))
		| compare ([],[]) = EQUAL
		| compare ([],x::xs) = LESS
		| compare (x::xs,[]) = GREATER

	fun conv(x:ord_key):RHS = x
end

(*creating a set of RHS*)(**)
structure RHSSet = RedBlackSetFn (RHS_KEY)

(*productions is ORD_SET.set == RHSSet.set type*)
type Productions = RHSSet.set

(*Rules are a Productions map = a' map*)
type Rules = Productions AtomMap.map


type Grammar    = { symbols : AtomSet.set, tokens : AtomSet.set, rules : Rules }


(*===============================functions to insert productions into map==============================*)
fun modify x = (Atom.atom x)
fun makeRule (ps,rs) = 
	(let
		val modified_rs = map modify rs
	in
		RHSSet.add(ps,modified_rs)
	end)

fun makeRules(g,l,rss) = 
	(let
		val ps = RHSSet.empty
		fun untilOnePsIsDone (ps,rs::rss) = untilOnePsIsDone(makeRule(ps,rs),rss) 
			| untilOnePsIsDone (ps,[]) = ps
	in
		AtomMap.insert(g,(modify l),untilOnePsIsDone(ps,rss))
	end)
fun makeSet (r::rs) = (AtomSet.add(makeSet(rs),(modify r)))
	| makeSet [] = AtomSet.empty
(*====================make grammar========================*)
val tokens = makeSet(["a","c","d"])
val symbols = makeSet(["Z","X","Y","P","M"])
val rules = AtomMap.empty
val rules = makeRules(rules,"Z",[["d"],["X","Y","Z"]])
val rules = makeRules(rules,"Y",[[],["c"]])
val rules = makeRules(rules,"X",[["Y"],["a"]])
val rules = makeRules(rules,"P",[[],["d"]])
val rules = makeRules(rules,"M",[["P","Y"]])

(*============================Helper Functions===============================*)
fun prnt_atom_list ls= map (fn k=> print((Atom.toString k)^" ")) ls
fun prnt_atom_set set= map (fn k=> print((Atom.toString k)^" ")) (AtomSet.listItems(set))
fun prnt_lines _ = print("\n------------------\n")
(*=====================print grammar======================*)
fun printGrammar(key, elements) = 
	let 
		val _ = (print(Atom.toString key);print("-->"))
		val prdcnList = RHSSet.listItems(elements)
		fun printElement xs = (map (fn k => (
											print(Atom.toString k))) xs;
											if len(xs,0) = 0 then print("_|") else print("|");())

		val _ = map printElement prdcnList
		val _ = print "\n"
	in
		()
	end
val _ = (print("\nTokens:{ ");prnt_atom_set(tokens);print(" }\n"))
val _ = (print("\nSymbols:{ ");prnt_atom_set(symbols);print(" }\n"))
val _ = (print("\n\nRules : ");prnt_lines())
val _ = (AtomMap.appi printGrammar rules;prnt_lines())

(*==================================nullable==================================*)
val nullable_set = ref AtomSet.empty

fun calc_nullable g =
	(let
		val symbls = (AtomMap.listKeys(g))
		fun is_prd_nullable (r::rs) = AtomSet.member(!nullable_set,r) andalso is_prd_nullable(rs)
			| is_prd_nullable ([]) = true
		fun is_sym_nullable(k) = foldl (fn (rs,b) => ((is_prd_nullable(rs)) orelse b)) false (RHSSet.listItems(AtomMap.lookup(g,k)))

		fun make_next_nullable (k::ks) = if is_sym_nullable(k) then ((nullable_set := AtomSet.add(!nullable_set,k));make_next_nullable(ks))
										else make_next_nullable(ks)
			| make_next_nullable ([]) = ()

		fun rec_nullable _ = 
			(let
				val old_set = !nullable_set
				val _ = make_next_nullable symbls
			in
				case AtomSet.equal(!nullable_set,old_set) of
					false => rec_nullable()
					| true => ()
			end)
 	in
		rec_nullable ()
	end)

val _ = calc_nullable(rules)

(*=================colors=================*)
(*val red:string = "\u001b[31m"
val blue:string = "\u001b[34m"
val green:string ="\u001b[32m"
val white:string = "\u001b[37m"
val cyan:string = "\u001b[36m"
val reset:string = "\u001b[0m"*)
(*val _ = print("\u001b[34m"^"Nullable Set : {"^"\u001b[0m")*)
val _ = print("\n\nNullable Set : {")
val _ = prnt_atom_set (!nullable_set)
val _ = print("}\n\n")
(*val _ = print("\u001b[34m"^"}"^"\u001b[0m")*)

(*===================helper func=====================*)

fun in_list (x,(r::rs)) = if Atom.compare(x,r)=EQUAL then true else in_list(x,rs)
			| in_list(x,[]) = false
fun gets(m,k) = ( case AtomMap.find(m,k) of
					SOME x => x
					|NONE => AtomSet.empty
					)
fun gets_g(m,k) = ( case AtomMap.find(m,k) of
					SOME x => x
					|NONE => RHSSet.empty
					)

fun map_equality (map1,map2,(k::ks)) = 
            (let
                 val m1_val = AtomMap.find(map1,k)
                 val m2_val = AtomMap.find(map2,k)
             in
                 case (m1_val,m2_val) of
                     (SOME v1,SOME v2)       =>AtomSet.equal(v1,v2) andalso (map_equality(map1,map2,ks))
                     |(NONE,NONE)          =>true andalso (map_equality(map1,map2,ks))
                     |_                      =>false
             end)
             |map_equality(map1,map2,[]) = true

fun printFirst(key, elements) = 
	let 
		val _ = (print(Atom.toString key);print("-->{"))
		val prdcnList = AtomSet.listItems(elements)
		fun printElement xs = (map (fn k => (print(Atom.toString k);print(" "))) xs;())
		val _ = printElement prdcnList
		val _ = print "}\n"
	in
		()
	end

(*======================================first===============================*)

fun calc_first g = 
	(let
		val first_map = ref AtomMap.empty
		val symbls = (AtomMap.listKeys(g))
		fun add_token_into_first (k,r) = (first_map := AtomMap.insert(!first_map,k,AtomSet.union(gets(!first_map,k),AtomSet.singleton(r)));())
		fun add_symbol_into_first (k,r) = (first_map := AtomMap.insert(!first_map,k,AtomSet.union(gets(!first_map,k),gets(!first_map,r)));())
		fun prd_first (k,(r::rs)) = (case in_list(r,symbls) of
									false => add_token_into_first(k,r)
									| true => (case AtomSet.member(!nullable_set,r) of
												true => (add_symbol_into_first(k,r);prd_first(k,rs))
												| false => add_symbol_into_first(k,r)))
			| prd_first(k,[]) = ()
		fun sym_first (k,(rs::rss)) = (prd_first(k,rs);sym_first(k,rss))
			|sym_first (k,[]) = ()
		fun all_sym_first(k::ks) = (sym_first(k,(  RHSSet.listItems(gets_g(g,k)))   );all_sym_first(ks);())
			| all_sym_first([]) = ()
		fun rec_first _ = 
			(let
				val old_map = !first_map
				val _ = all_sym_first(symbls)
			in
				case map_equality(old_map,!first_map,symbls) of
					false => rec_first()
					| true => ()
			end)
	in
		(rec_first();!first_map)
	end)

val first_map_final = calc_first(rules)
val _ = print("\nFirst :\n-----------\n")
val _ = (AtomMap.appi printFirst first_map_final;print("-----------\n"))

(*===========================================FOllow===========================*)

fun calc_follow g =
	(let
		val follow_map = ref AtomMap.empty
		val symbls = (AtomMap.listKeys(g))
		fun add_token_into_first(t,set) = AtomSet.union(AtomSet.singleton(t),set)
		fun add_symbol_into_first(k,set) = AtomSet.union(gets(first_map_final,k),set)
		fun list_first ((r::rs),set) = (case in_list(r,symbls) of
									false => add_token_into_first(r,set)
									|true => (case AtomSet.member(!nullable_set,r) of
												true => list_first(rs,(add_symbol_into_first(r,set)))
												|false => add_symbol_into_first(r,set)))
			|list_first ([],set) = set
		fun add_set_to_follow(k,s) = follow_map := AtomMap.insert(!follow_map,k,AtomSet.union(gets(!follow_map,k),s))
		fun update_follow_map(fk,k,r::rs) = if(AtomSet.isEmpty(list_first((r::rs),AtomSet.empty))) 
										then (if(AtomSet.isEmpty(gets(!follow_map,fk)))
												then ()
											  else (add_set_to_follow(k,gets(!follow_map,fk)))) 
										else (add_set_to_follow(k,list_first((r::rs),AtomSet.empty)))
			|update_follow_map(fk,k,[]) = if(AtomSet.isEmpty(gets(!follow_map,fk)))
												then ()
										  else (add_set_to_follow(k,gets(!follow_map,fk)))
		fun prpgn_one_prd(fk,k,(r::rs)) = if(Atom.compare(k,r) = EQUAL) then update_follow_map(fk,k,rs) 
										else prpgn_one_prd(fk,k,rs)
			|prpgn_one_prd(fk,k,[]) = ()
		fun prpgn_one_symbol_prdns(fk,k,(rs::rss)) = (prpgn_one_prd(fk,k,rs);prpgn_one_symbol_prdns(fk,k,rss))
			|prpgn_one_symbol_prdns(fk,k,[])= ()
		fun sym_follow k fk = prpgn_one_symbol_prdns(fk,k,RHSSet.listItems(gets_g(g,fk)))
		(*fun next_follow _ = map (map sym_follow symbls) [symbls]*)
		fun one_k_all_fk k = map (sym_follow k) symbls 
		fun rec_follow _ = 
			(let
				val old_map = !follow_map
				val _ = map one_k_all_fk symbls 
			in
				case map_equality(old_map,!follow_map,symbls) of
					false => rec_follow()
					| true => ()
			end)
		(*val temp = list_first([Atom.atom "X",Atom.atom "P"],AtomSet.empty)*)
	in
		(rec_follow();!follow_map)
	end)

val follow_map_final = calc_follow(rules)

val _ = (print("\n\nFollow:");prnt_lines())
val _ = AtomMap.appi printFirst follow_map_final
val _ = prnt_lines()

(*=====================LL(1)============================*)

fun compute_ll1 g =
	(let
		val token_list = AtomSet.listItems(tokens)
		val symbls = (AtomMap.listKeys(g))
		fun add_token_into_first(t,set) = AtomSet.union(AtomSet.singleton(t),set)	
		fun add_symbol_into_first(k,set) = AtomSet.union(gets(first_map_final,k),set)
		
		fun list_first ((r::rs),set) = (case in_list(r,symbls) of
									false => add_token_into_first(r,set)
									|true => (case AtomSet.member(!nullable_set,r) of
												true => list_first(rs,(add_symbol_into_first(r,set)))
												|false => add_symbol_into_first(r,set)))
			|list_first ([],set) = set
		fun prnt_one_cell (s,t,r::rs) = 
			(print("(");print(Atom.toString(s));print(",");print(Atom.toString(t));print(") : ");print(Atom.toString(s));print(" --> ");prnt_atom_list(r::rs);print("\n");())
		fun prpgn_one_prd(s,t,(r::rs)) = 
			(let
				val _ = if(AtomSet.member(list_first((r::rs),AtomSet.empty),t)) then prnt_one_cell(s,t,r::rs) else ()
				val _ = if(AtomSet.member(!nullable_set,t) 
							andalso AtomSet.member(gets(follow_map_final,s),t)) then prnt_one_cell(s,t,r::rs)
						else ()
			in
				()
			end)
			|prpgn_one_prd(s,t,[]) = ()
		fun prpgn_one_symbol_prdns(s,t,(rs::rss)) = (prpgn_one_prd(s,t,rs);prpgn_one_symbol_prdns(s,t,rss))
			|prpgn_one_symbol_prdns(s,t,[])= ()
		
		fun sym_follow t s = prpgn_one_symbol_prdns(s,t,RHSSet.listItems(gets_g(g,s)))
		fun one_t_all_s t = map (sym_follow t) symbls 
	in
		map one_t_all_s token_list 		
	end)

val _ = (print("\n\nLL(1) Parsing Table :");prnt_lines())
val _ = compute_ll1(rules)
val _ = prnt_lines();