

datatype token = Identifier of string*int | NUM of int*int 
  | IFCond of int | THENCond of int | ELSECond of int  |Prog of int| TRUE of int | FALSE of int
  |VARDec of int|TYPEINT of int |TYPEBOOL of int |TakeInput of int 
  |ShowOutput of int |EndingIF of int
  |LOOPStart of int| TILL of int |LOOPEnd of int|BlockInitiator of int|AssignOp of int
  |TypeInitiator of int | EndStatement of int | NOTOfBool of int
  |Plusop of int|Minusop of int|Mulop of int|Divop of int|Remainderop of int|CompEqual of int
  |CompLessEqual of int|CompGreatEqual of int |NotEqual of int |CompGreat of int
  |CompLess of int |NegSign of int| BoolAnd of int |BoolOr of int| COMMA of int
  | EQUAL of int | LPAREN of int| RPAREN of int |StartCommands of int|EndCommands of int| EOF 


(*signature LEXER =
sig
    val SourceFile : string -> token list
end

structure lexer: LEXER =
struct*)
local
  val counter : int ref = ref 1;
  open TextIO

  (*  nexttoken recognizes the various reserved words of PCF, along with
      identifiers, integer literals, and the symbols =, =>, (, and ).
      Also, # begins a comment that extends to the end of the current line.
  *)
  fun nexttoken strm =
    case input1 strm of
        NONE   => EOF
      | SOME c =>
	  if Char.isSpace c then(*for ignoring space and new line character*)
	    if (c = #"\n") then
		(counter := (!counter) +1;(*print(Int.toString(!counter))*)nexttoken strm) (*for ignoring newline character*)	
		 else
		nexttoken strm 
	  else if Char.isAlpha c then
	    let
	      fun getid id =
		case lookahead strm of
		    NONE   => id
		  | SOME d =>
		      if Char.isAlpha d orelse Char.isDigit d then
			(input1 strm; getid (id ^ str d))
		      else
			id
	      val ident = getid (str c)
	    in case ident of
	           "if"     => IFCond (!counter)
		 | "then"   => THENCond (!counter)
		 | "else"   => ELSECond (!counter)
		 |"endif"   => EndingIF (!counter)
		 |"while"   => LOOPStart (!counter)
		 |"do"      => TILL (!counter)
		 | "endwh"  => LOOPEnd (!counter)
		 |"program" => Prog (!counter)
		 | "tt"   => TRUE (!counter)
		 | "ff"  => FALSE (!counter)
		 | "var" => VARDec (!counter)
		 | "int" => TYPEINT (!counter)
		 | "bool" => TYPEBOOL (!counter)
		 | "read" => TakeInput (!counter)
		 | "write" => ShowOutput (!counter)
		 | _        => Identifier (ident,(!counter))
	    end
	  else if Char.isDigit c then
	    let
	      fun getnum num =
		case lookahead strm of
		    NONE   => num
		  | SOME d =>
		      if Char.isDigit d then
			(input1 strm; getnum (10*num + ord d - ord #"0"))
		      else
			num
	    in
	      NUM (getnum (ord c - ord #"0"),(!counter))
	    end
	  else if c = #"&" andalso lookahead strm = SOME #"&" then (input1 strm;BoolAnd (!counter))
	  else if c = #"|" andalso lookahead strm = SOME #"|" then (input1 strm;BoolOr (!counter))
	  else
	    case c of
(*	        #"=" => (case lookahead strm of
			     SOME #">" => (input1 strm; FNARROW)
			   | _         => EQUAL)                              *)
	        #"(" => LPAREN (!counter)
	      | #")" => RPAREN (!counter)
	      | #"{" => StartCommands (!counter)
	      | #"}" => EndCommands (!counter)
	      | #":" => (case lookahead strm of
			     SOME #":" => (input1 strm; BlockInitiator (!counter))
			    |SOME #"=" => (input1 strm; AssignOp (!counter))
			    |_	       => TypeInitiator (!counter))
	      | #";" => EndStatement (!counter)
	      | #"," => COMMA (!counter)
	      | #"!" => NOTOfBool (!counter)
	      | #"+" => Plusop (!counter)
	      | #"-" => Minusop (!counter)
	      | #"~" => NegSign (!counter)
	      |	#"*" => Mulop (!counter)
	      | #"/" => Divop (!counter)
              | #"%" => Remainderop (!counter)
	      | #"=" => CompEqual (!counter)
	      | #"<" => (case lookahead strm of 
			      SOME #"=" => (input1 strm; CompLessEqual (!counter))
			     |SOME #">" => (input1 strm; NotEqual (!counter))
	                     |_		=> CompLess (!counter))
	      | #">" => (case lookahead strm of
			      SOME #"=" => (input1 strm; CompGreatEqual (!counter))
			     |_		=> CompGreat (!counter))
(*	      | #"#" =>
		    
		  let fun eatline () =
			case input1 strm of
			    NONE       => EOF
			  | SOME #"\n" => nexttoken strm
			  | SOME _     => eatline ()
		  in
		    eatline ()
		  end                                                                  *)
(*	      | #"&" => (case lookahead strm of 
			    SOME #"&" => (input1 strm; BoolAnd))
	      | #"|" => (case lookahead strm of 
			    SOME #"|" => (input1 strm; BoolOr))
*)	      
	      (*| #"\n"=> (counter := !counter +1;print(Int.toString(!counter));nexttoken strm) *)(*for ignoring newline character*)
	     (* | #"\n" => (print("fuck u");nexttoken strm) *)
	      | _    => (print ("Skipping illegal character ->" ^ str c ^" in line no."^ Int.toString((!counter)) ^ ".\n");
			 nexttoken strm)

  fun gettokens strm =
    let
      fun gettokens_aux toks =
        let val tok = nexttoken strm
        in
	  if tok = EOF then
	    (closeIn strm; rev (EOF::toks))
          else
	    gettokens_aux (tok::toks)
        end
    in
      gettokens_aux []
    end

in
  fun SourceFile file = ((counter:=1);gettokens (openIn file))
  (*  Note that we could instead have called
        explode (input (openIn file))
      to get a list of all the characters in the file.
  *)
end



(*$$$$$$$$$$$$$$$$$$$$$$$$$$$     Parser         $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*)

val tempVarList = ref []; 
val tempVar = ref "";	


val list_of_tokens = SourceFile "testcase1.txt";		(*---------INPUT FILE---------*)

val eLine = ref 1;

val head = ref (Prog 1);

val Tlist = ref list_of_tokens;

fun getToken () = 
	(head:=hd(!Tlist);Tlist:=tl(!Tlist);!head)
fun error() = print("Syntactic Error In Your Program\n")

val storeType = ref "int";

val tok = ref (getToken())

fun advance() = tok := getToken ()

fun eat(t) = if (!tok=t) then 
	if (t = EOF) then print("parsed Successfully\n") else advance() 
	 else error()

fun Line() = case !tok of
		 Identifier(r,k)	=>k
		|NUM (r,k)		=>k
  | AssignOp(k)				=>k
  | BlockInitiator(k)			=>k
  | BoolAnd(k)				=>k
  | BoolOr(k)				=>k
  | COMMA(k)				=>k
  | CompEqual(k)			=>k
  | CompGreat(k)			=>k
  | CompGreatEqual(k)			=>k
  | CompLess(k)				=>k
  | CompLessEqual(k)			=>k
  | Divop(k)				=>k
  | ELSECond(k)				=>k
 
  | EQUAL(k)				=>k
  | EndCommands(k)			=>k
  | EndStatement(k)			=>k
  | EndingIF(k)				=>k
  | FALSE(k)				=>k
  | IFCond(k)				=>k
 
  | LOOPEnd(k)				=>k
  | LOOPStart(k)			=>k
  | LPAREN(k)				=>k
  | Minusop(k)				=>k
  | Mulop(k)				=>k
  | NOTOfBool(k)			=>k

  | NegSign(k)				=>k
  | NotEqual(k)				=>k
  | Plusop(k)				=>k
  | Prog(k)				=>k
  | RPAREN(k)				=>k
  | Remainderop(k)			=>k
  | ShowOutput(k)			=>k
  | StartCommands(k)			=>k	
  | THENCond(k)				=>k
  | TILL(k)				=>k
  | TRUE(k)				=>k
  | TYPEBOOL(k)				=>k
  | TYPEINT(k)				=>k
  | TakeInput(k)			=>k
  | TypeInitiator(k)			=>k	
  | VARDec(k)				=>k

fun lexeme() = case !tok of
 		 Identifier(l,k)=>l
		|NUM(l,k)	=>(Int.toString(l))

fun error1() = print("Syntax Error in Line Number :"^Int.toString(Line())^"\n")


(*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*)
(*--This point onward is an example of how to implement parser + do semantic analysis, to generte 3-Adress Code for While Programming Language Using SML--*)

(*
fun Start() = (Program();eat(EOF))

and Program() = (eat(Prog(Line()));IdentifierP();eat(BlockInitiator(Line()));Block())(*Prog*)

and Block() = case !tok of
		 StartCommands(k)	=> (DeclarationSeq();CommandSeq())		
		|VARDec(k)		=> (DeclarationSeq();CommandSeq())
		|_			=> (error1();print("Expected tokens StartCommands or VarDec\n"))

and DeclarationSeq() = case !tok of 
		 StartCommands(k)	=> ()
		|VARDec(k)		=> (Declaration();DeclarationSeq())
		|_			=> (error1();print("Expected tokens StartCommands or VarDec\n"))


(*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*)

and Declaration() = (eat(VARDec(Line()));VariableList;eat(TypeInitiator(Line()));
			storeType := Type();
			eat(EndStatement(Line()));tempVarList:=[])

and Type() = case !tok of
		 TYPEBOOL(k)		=>(eat(TYPEBOOL(Line()));"bool")
		|TYPEINT(k)		=>(eat(TYPEINT(Line()));"int")
		|_			=>(error1();print("Expected tokens TYPEBOOL or TYPEINT\n");"")

and VariableList() = (tempVar:=Variable();[tempVar]@tempVarList;M())

and M() = case !tok of
		 COMMA(k)		=>(eat(COMMA(Line()));VariableList())
		|TypeInitiator(k)	=>()
		|_			=>(error1();print("Expected tokens COMMA or TypeInitiator\n"))

and CommandSeq() = (eat(StartCommands(Line()));K();eat(EndCommands(Line())))

and K() = case !tok of
		 TakeInput(k)		=>(Command();eat(EndStatement(Line()));K())
		|ShowOutput(k)		=>(Command();eat(EndStatement(Line()));K())
		|IFCond(k)		=>(Command();eat(EndStatement(Line()));K())
		|LOOPStart(k)		=>(Command();eat(EndStatement(Line()));K())
		|Identifier(r,k)	=>(Command();eat(EndStatement(Line()));K())
		|EndCommands(k)		=>()
		|_			=>(error1();print("Expected tokens TakeInput or ShowOutput or IFCond or LoopStart or Identifier or EndCommands\n"))

and Command() = case !tok of
		 Identifier(r,k)	=>(Variable();eat(AssignOp(Line()));Expression())
		|TakeInput(k)		=>(eat(TakeInput(Line()));Variable())
		|ShowOutput(k)		=>(eat(ShowOutput(Line()));IntExpression())
		|IFCond(k)		=>(eat(IFCond(Line()));BoolExpression;eat(THENCond(Line()));
					CommandSeq();eat(ELSECond(Line()));
					CommandSeq();eat(EndingIF(Line())))
		|LOOPStart(k)		=>(eat(LOOPStart(Line()));BoolExpression();eat(TILL(Line()));
					CommandSeq();eat(LOOPEnd(Line())))
		|_			=>(error1();print("Expected tokens Identifier or TakeInput or ShowOutput or IFCond or LoopStart\n"))

*)










		 
		
























































































