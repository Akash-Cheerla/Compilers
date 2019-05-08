type RHS = Atom.atom list

structure RHS_KEY :ORD_KEY= struct
    type ord_key =RHS
    fun  compare (x::xs,y::ys)=(case Atom.compare(x,y) of
        							EQUAL => (compare (xs,ys))
        							| GREATER => GREATER
        							| LESS => LESS)
    	| compare ([],[]) = EQUAL
    	| compare (x::xs,[]) = GREATER
    	| compare ([],y::ys) = LESS
    fun get(x:ord_key):RHS=x
end
structure RHSSet = RedBlackSetFn (RHS_KEY)
type Productions = RHSSet.set

type Item= Atom.atom*Atom.atom list*Atom.atom list;

fun compare_list ((x::xs),(y::ys)) = (case Atom.compare (x,y) of
	        								  GREATER => GREATER
	        								| LESS	  => LESS
	        								| EQUAL	  => compare_list (xs,ys))
	    	|compare_list ((x::xs),[])	  = GREATER
	    	|compare_list ([]	,(y::ys)) = LESS
	    	|compare_list ([],	[])		  = EQUAL
fun compare_item ((x:Item),(y:Item)) = 
	    	let
	    		val xlhs = #1 x
	    		val xbef = #2 x
	    		val xaft = #3 x

	    		val ylhs = #1 y
	    		val ybef = #2 y
	    		val yaft = #3 y
	    	in
	    		case Atom.compare(xlhs,ylhs) of
	    			EQUAL => (case compare_list(xbef,ybef) of
	    						EQUAL => compare_list(xaft,yaft)
	    						|y 	  => y
	    				)
	    			|x 	  => x
	    	end
fun compare_state(x::xs,y::ys)=compare_item(x,y)
structure ITEM_KEY :ORD_KEY= struct
    type ord_key =Item
    val compare=compare_item
end
structure ItemSet = RedBlackSetFn (ITEM_KEY)
type itemtype=ItemSet.set
(*signature PROXY = sig
   type proxy
   type count
   val proxy  : actual -> proxy
   val actual : proxy -> actual
end*)

type jump=int AtomRedBlackMap.map ref
type STATE=itemtype* Atom.atom list AtomRedBlackMap.map ref*jump*jump

structure StateKey :ORD_KEY= struct
    	type ord_key =STATE
    	fun compare((x,_,_,_):STATE,(a,_,_,_):STATE)=compare_state(ItemSet.listItems x,ItemSet.listItems a)
	end
    structure STATEMap = RedBlackMapFn (StateKey)
    val itemMap = STATEMap.map
(*val itemMap=STATEMap.map*)
(*val p:STATE=(!result,AtomMap.singleton(Atom.atom "A",1)
	,AtomMap.singleton(Atom.atom "A",1),AtomMap.singleton(Atom.atom "A",1));*)
signature temp = 
sig
end


fun makeRule (r,productionlist,rule) =
	let
		fun modify x = map (fn k=>Atom.atom k) x
		val s=map modify productionlist
		val s2=RHSSet.fromList(s)
	in
		AtomMap.insert(rule,Atom.atom r,s2)
	end

val rules=AtomMap.empty;
val rules=makeRule("S",[["E","$"]],rules)
val rules=makeRule("E",[["T","+","E"],["T"]],rules)
(*val rules=makeRule("EP",[["+","T","EP"],[]],rules)*)
val rules=makeRule("T",[["x"]],rules)
(*val rules=makeRule("TP",[[],["*","N","TP"]],rules)*)

fun makeSymbols(symbolslist)=
	let
		val s = map(fn k=>Atom.atom k)symbolslist
	in
		AtomSet.fromList(s)
	end
val symbols=AtomSet.empty;
val symbols=makeSymbols(["S","E","T"])

val tokens=AtomSet.empty;
val tokens=makeSymbols(["+","x","$"])

fun printMap (key,element) = 
	let
		val _ = print (Atom.toString key)
		val production_list = RHSSet.listItems(element)
		fun printProduction symtok_list =( print "{";map (fn k=>print((Atom.toString k)^" ")) symtok_list ; print "}" )
		val _ = (print " : ";map printProduction production_list;print "\n")
	in
		()
		
	end

val _ =print ("Grammar is as follows\n")
val t1 = AtomMap.appi printMap rules

fun printList(x::xs)=(print (Atom.toString x);printList(xs))
	| printList([])=()
fun Print((a,b,c)::xs)=(print (Atom.toString a);print "->";printList b;print ".";
	printList c;print "\n";Print(xs))
	| Print([])=()

val result=ref (ItemSet.empty);
fun makeLR0(item,result) =
	let
		val P=AtomMap.listItemsi rules
		val y = #3 item
		fun addElements(x::xs,s)=if ItemSet.member(!result,(s,[],x)) then (addElements(xs,s);()) 
		else (makeLR0((s,[],x),result);addElements(xs,s);())
			| addElements([],s)=()
		fun checkElements((key,element)::xs,s)= if Atom.same(key,s) then addElements(RHSSet.listItems(element),s)
		else checkElements(xs,s)
			| checkElements([],s)=() 
		fun addToResult(x)= checkElements(P,x) 
		fun verify(x::xs)=if AtomSet.member(symbols,x) then addToResult(x)
		else ()
			| verify([])=()
	in
		(result := ItemSet.add(!result,item);verify(y))
	end

fun makeLR0s(item,result) =
	let
		val P=AtomMap.listItemsi rules
		(*val y = #3 item*)
		fun addElements(x::xs,s)=if ItemSet.member(!result,(s,[],x)) then (addElements(xs,s);()) 
		else (makeLR0((s,[],x),result);addElements(xs,s);())
			| addElements([],s)=()
		fun checkElements((key,element)::xs,s)= if Atom.same(key,s) then addElements(RHSSet.listItems(element),s)
		else checkElements(xs,s)
			| checkElements([],s)=() 
		fun addToResult(x)= checkElements(P,x) 
		fun verify(x::xs)=if AtomSet.member(symbols,x) then addToResult(x)
		else ()
			| verify([])=()
		fun verifys((lhs,_,x)::xs)=(verify(x);verifys(xs);())
			| verifys ([])=()
	in
		(result := ItemSet.addList(!result,item);verifys(item))
	end

functor Proxy(A : temp)  = struct
(*structure Proxy=struct*)
    (*type proxy = int*)
    val count = ref 0
    val x=RHSSet.empty
    
    (*type proxyMap = ref (map from A.ord_key to int)*)
    val pMap: (int STATEMap.map) ref = ref STATEMap.empty 
    (*type reverseMap = ref (map from int -> A.ord_key)*)
    val rMap: (STATE IntRedBlackMap.map) ref=ref IntRedBlackMap.empty
    fun addState(x:STATE)=(
    			pMap := STATEMap.insert(!pMap,x,!count);
    			rMap := IntRedBlackMap.insert(!rMap,!count,x);
    			count := !count+1)
    fun findState(x)=IntRedBlackMap.lookup(!rMap,x)
    fun getCount()= !count
    fun checkItem((x,_,_,_),y)=ItemSet.equal(x,y)
    fun check(item,c:int)= if c<0 then ~1 
    else if checkItem(findState(c),item) then c 
    	else check(item,c-1) 
    fun checkStates(item)=
    	let
    		val result=ref (ItemSet.empty);
			val _ = makeLR0s(item,result);
			val x=check(!result,getCount()-1)
    	in
    		x
    	end
    fun printReduce((key,element)::xs)=(print "Reduce ";
    	print (Atom.toString key);print "->";
    	printList element;print "\n";printReduce (xs);() )
    | printReduce([])=()

    fun printShift((key,element)::xs)=(print "Shift on ";print (Atom.toString key);
    	print " to state ";print (Int.toString element);print "\n";
    	printShift(xs);())
    | printShift ([])=()

    fun printGoto((key,element)::xs)=(print "Goto on ";print (Atom.toString key);
    	print " to state ";print (Int.toString element);print "\n";	
    	printGoto(xs);())
    | printGoto ([])=()

    fun printStateLast((x,r,s,g))=(
    	print ("\027[34m");Print(ItemSet.listItems x);print ("\027[0m");
    	print ("\027[31m");printReduce(AtomMap.listItemsi(!r));print ("\027[0m");
    	print ("\027[32m");printShift(AtomMap.listItemsi(!s));print ("\027[0m");
    	print ("\027[33m");printGoto(AtomMap.listItemsi(!g));print ("\027[0m");())

    fun printStateItem(x)=(print ("\027[35m");print "LRO of state ";print (Int.toString x);
    	print ("\027[0m");print "\n";
    	printStateLast(findState(x)))
    fun prinState(l,c)=if c<=l then (printStateItem(c);prinState(l,c+1);())
    else ()
    fun printStates()=(prinState(getCount()-1,0))

    (*val proxy :
    val actual : proxy -> A.ord_key*)
end

val x:Item=(Atom.atom "S",[],[Atom.atom "E",Atom.atom "$"])
val y:Item=(Atom.atom "T",[],[Atom.atom "x"])
val z:Item=(Atom.atom "E",[Atom.atom "T",Atom.atom "+"],[Atom.atom "E"])
val _ = makeLR0(z,result);



val _=print "LR0 is as follows\n"
val _ =Print(ItemSet.listItems(!result))
structure b= struct end
structure s =Proxy(b)
fun Generate(item)=
	let
		val result=ref (ItemSet.empty)
		val _ = makeLR0s(item,result)
		(*val x=s.check(!result,s.getCount-1)*)
		val i = !result
		val shift=ref AtomMap.empty
		val reduce: Atom.atom list AtomRedBlackMap.map ref=ref AtomMap.empty
		val temp:Item list ref=ref []
		val goto=ref AtomMap.empty
		val p:STATE=(i,reduce,shift,goto)
		val return =s.getCount();
		val _=s.addState(p)
		val stateNum=s.getCount()-1
		(*val (i,shift,reduce,goto)=!result*)
		fun verify(item)=if s.checkStates(item)<1 then (Generate(item))
		else s.checkStates(item) 
		(*fun updateMapsShift((lhs,b,y::ys)::xs,x)=if AtomSet.member(tokens,y) andalso Atom.same(y,x)
			then((shift := AtomMap.insert(!shift,y,verify(lhs,b@[y],ys)));updateMapsShift(xs,x);())
			else (updateMapsShift(xs,x);())
			| updateMapsShift((lhs,b,[])::xs)=()
			| updateMapsShift([])=()*)
		fun checkempty [x]=false
			|checkempty (x::xs)=false
			| checkempty []=true
		fun updateMapsShift((lhs,b,y::ys)::xs,x)=if AtomSet.member(tokens,y) andalso Atom.same(y,x)
			then(temp := (!temp)@[(lhs,b@[y],ys)];updateMapsShift(xs,x);())
			else (updateMapsShift(xs,x);())
			| updateMapsShift((lhs,b,[])::xs,x)=(updateMapsShift(xs,x);())
			| updateMapsShift([],x)= if checkempty(!temp) then ()
			else ((shift := AtomMap.insert(!shift,x,verify(!temp)));())

		fun updateMapsGoto((lhs,b,y::ys)::xs,x)=if AtomSet.member(symbols,y) andalso Atom.same(y,x)
			then(temp := (!temp)@[(lhs,b@[y],ys)];updateMapsGoto(xs,x);())
			else (updateMapsGoto(xs,x);())
			| updateMapsGoto((lhs,b,[])::xs,x)=(updateMapsGoto(xs,x);())
			| updateMapsGoto([],x)= if checkempty(!temp) then ()
			else ((goto := AtomMap.insert(!goto,x,verify(!temp)));())

		(*fun updateMapsGoto((lhs,b,y::ys)::xs,x)=if AtomSet.member(symbols,y) andalso Atom.same(y,x)
			then((shift := AtomMap.insert(!goto,y,verify(lhs,b@[y],ys)));updateMapsGoto(xs,x);())
			else (updateMapsGoto(xs,x);())
			| updateMapsGoto((lhs,b,[])::xs)=()
			| updateMapsGoto([])=()*)

		fun updateMapsReduce((lhs,b,[])::xs)=(reduce := AtomMap.insert(!reduce,lhs,b);updateMapsReduce(xs);())
			| updateMapsReduce((lhs,b,x)::xs)=(updateMapsReduce(xs);())
			| updateMapsReduce([])=()

		fun updatesMapsShift(x::xs)=(temp :=[];updateMapsShift(ItemSet.listItems(i),x);
									updatesMapsShift(xs);())
			| updatesMapsShift ([])=()

		fun updatesMapsGoto(x::xs)=(temp :=[];updateMapsGoto(ItemSet.listItems(i),x);
									updatesMapsGoto(xs);())
			| updatesMapsGoto ([])=()

		(*fun update(i,x::xs)=(updateMaps(i,x);update)*)
		val _=(updatesMapsShift(AtomSet.listItems(tokens));
			updatesMapsGoto(AtomSet.listItems(symbols));
			updateMapsReduce(ItemSet.listItems(i)))
		(*val (iTemp,reduceTemp,shiftTemp,gotoTemp)=s.findState(stateNum)*)

	in
		return
	end
val x:Item list=[(Atom.atom "S",[],[Atom.atom "E",Atom.atom "$"])]

(*val x:Item list=[(Atom.atom "E",[Atom.atom "T"],[Atom.atom "+",Atom.atom "E"]),
(Atom.atom "E",[Atom.atom "T"],[])]*)
val _ =Generate(x)
val _=s.printStates()
(*val p:STATE=(!result,AtomMap.singleton(Atom.atom "A",1)
	,AtomMap.singleton(Atom.atom "A",1),AtomMap.singleton(Atom.atom "A",1));*)
(*structure b= struct end
structure s =Proxy(b)*)
(*val _=s.addState(p)
val (x,_,_,_)=s.findState(0)
val _=print("State LR0 item")
val _=Print(ItemSet.listItems(x))*)

