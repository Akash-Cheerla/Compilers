
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
end

(*creating a set of RHS*)(**)
structure RHSSet = RedBlackSetFn (RHS_KEY)

(*productions is ORD_SET.set == RHSSet.set type*)
type Productions = RHSSet.set

(*Rules are a Productions map = a' map*)
type Rules = Productions AtomMap.map


type Grammar    = { symbols : AtomSet.set, tokens : AtomSet.set, rules : Rules }


(*===================entering symbols,tokens and rules of the grammar =============*)


(*val lhs = Atom.atom "S"*)
(*val prdcn = [Atom.atom "E",Atom.atom "$"]*)
val prdcnset1 = RHSSet.empty
val prdcnset1 = RHSSet.add(prdcnset1,[Atom.atom "d"])
val prdcnset1 = RHSSet.add(prdcnset1,[Atom.atom "X",Atom.atom "Y",Atom.atom "Z"])

val prdcnset2 = RHSSet.empty
val prdcnset2 = RHSSet.add(prdcnset2,[])
val prdcnset2 = RHSSet.add(prdcnset2,[Atom.atom "c"])

val prdcnset3 = RHSSet.empty
val prdcnset3 = RHSSet.add(prdcnset3,[Atom.atom "Y"])
val prdcnset3 = RHSSet.add(prdcnset3,[Atom.atom "a"])

val prdcnset4 = RHSSet.empty
val prdcnset4 = RHSSet.add(prdcnset4,[])
val prdcnset4 = RHSSet.add(prdcnset4,[Atom.atom "d"])


val prdcnset5 = RHSSet.empty
val prdcnset5 = RHSSet.add(prdcnset5,[Atom.atom "P",Atom.atom "Y"])

val m = AtomMap.empty
val m = AtomMap.insert(m,Atom.atom "Z",prdcnset1)
val m = AtomMap.insert(m,Atom.atom "Y",prdcnset2)
val m = AtomMap.insert(m,Atom.atom "X",prdcnset3)
val m = AtomMap.insert(m,Atom.atom "P",prdcnset4)
val m = AtomMap.insert(m,Atom.atom "M",prdcnset5)



(*val m = AtomMap.singleton(Atom.atom "S",prdcnset)*)

(*=============printing grammar================*)

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

val _ = AtomMap.appi printGrammar m;



(*==========================Nullable=============================*)
		(*list of tuples (key,value)pair*)

(*fun make_nullable m = 
	let
		val nlbl_set = AtomSet.empty;
		val n = AtomMap.insert(AtomMap.empty,Atom.atom "Z",false);
		val n = AtomMap.insert(m,Atom.atom "Y",false);
		val n = AtomMap.insert(m,Atom.atom "X",false);
		val n = AtomMap.insert(m,Atom.atom "a",false);
		val n = AtomMap.insert(m,Atom.atom "b",false);
		val n = AtomMap.insert(m,Atom.atom "c",false);
		val n = AtomMap.insert(m,Atom.atom "d",false);

		val key_val_list = AtomMap.listItemsi m;

		fun first_nullable_set ((x::xs):(Atom.atom*RHSSet.set) list) = 
			(let
				val key = (#1 x)
				val prodn_list = RHSSet.listItems (#2 x)
				fun nlbl (z::zs) = AtomMap.lookup(n,z) andalso nlbl(zs)
					| nlbl [] = true
				fun nlbl_exists (y::ys) = nlbl(y) oralso nlbl(ys)
				fun epsilon_exists (y::ys) = ( case y of
								[] => true
								|_ => epsilon_exists ys
								)
					| epsilon_exists [] = false

				val new_value = nlbl_exists(prodn_list)

			in
				case new_value of
					AtomMap.lookup(n,key) =>  
					|not AtomMap.lookup(n,key)=> ()
				case (epsilon_exists prodn_list) of 
					true => AtomSet.add((first_nullable_set xs),key)
					| false => (first_nullable_set xs)
			end)
			| first_nullable_set []  = AtomSet.empty;

	in
		first_nullable_set key_val_list
	end;

val nullable_set = make_nullable m;*)

(*============================Helper Functions===============================*)
fun prnt_atom_list ls= map (fn k=> print((Atom.toString k)^" ")) ls
fun prnt_atom_set set= map (fn k=> print((Atom.toString k)^" ")) (AtomSet.listItems(set))


(*==================================Nullable =================================*)
fun calc_nullable (m:(RHSSet.set AtomRedBlackMap.map)) =
	(let
		fun make_next_nullable_set set =
			(let
				val symbls = (AtomMap.listKeys (m))
				(*val _ = prnt_atom_list symbls;*)
				fun for_each_symbol((k::ks),key_set)=
					(let
						val productions =RHSSet.listItems(AtomMap.lookup(m,k))

						fun for_each_prdcn ((prd::prds),prdn_set) = 
											(case prd of 
												(*[] => for_each_prdcn(prds,AtomSet.add(prdn_set,k))*)
												[] => AtomSet.add((for_each_prdcn (prds,prdn_set)),k)
												|x::xs => 
													(let
														fun one_prdn (x::xs) =
														((AtomSet.member(prdn_set,x) andalso (one_prdn xs)))
														| one_prdn [] = true
													in
														case (one_prdn prd) of
															(*true => for_each_prdcn(prds,(AtomSet.add(prdn_set,k)))*)
															true => AtomSet.add((for_each_prdcn (prds,key_set)),k )
															|false => prdn_set
													end	
													)
											)
								| for_each_prdcn ([],key_set) = key_set

						val updated_set = for_each_prdcn(productions,key_set)
					in
						AtomSet.union(updated_set,(for_each_symbol(ks,updated_set)))
					end)
					| for_each_symbol([],key_set) = key_set
			in
				for_each_symbol(symbls,set)
			end)

		fun rec_nullable set =
			(let
				val new_set = make_next_nullable_set set
			in
				case AtomSet.equal(set,new_set) of
					false => (rec_nullable new_set)
					| true => new_set
			end)
	in
		rec_nullable AtomSet.empty
	end)

val nullable_set = calc_nullable m
(*val _ = print(Int.toString(AtomSet.numItems(nullable_set)))*)
val _ = prnt_atom_set nullable_set

(*====================================First====================================*)


(*======================================temp=====================================*)
(*fun pe ls = map (fn k=> print (Int.toString k)) ls
val _ = map pe [[1,2,3],[4,5,6]]
*)





(*fun p [x] = print x | p (x::xs) = (print (x^"|");p xs) | p [] = ();*)

