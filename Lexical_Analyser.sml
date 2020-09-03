

datatype token = Identifier of string | NUM of int 
  | IFCond | THENCond | ELSECond |Prog| TRUE | FALSE|VARDec|TYPEINT|TYPEBOOL|TakeInput|ShowOutput|EndingIF
  |LOOPStart| TILL |LOOPEnd |BlockInitiator|AssignOp|TypeInitiator | EndStatement | NOTOfBool
  |Plusop|Minusop|Mulop|Divop|Remainderop|CompEqual|CompLessEqual|CompGreatEqual|NotEqual|CompGreat
  |CompLess|NegSign|BoolAnd |BoolOr | COMMA
  | EQUAL | LPAREN | RPAREN |StartCommands|EndCommands| EOF


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
		(counter := !counter +1;(*print(Int.toString(!counter))*)nexttoken strm) (*for ignoring newline character*)	
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
	           "if"     => IFCond
		 | "then"   => THENCond
		 | "else"   => ELSECond
		 |"endif"   => EndingIF
		 |"while"   => LOOPStart
		 |"do"      => TILL
		 | "endwh"  => LOOPEnd
		 |"program" => Prog
		 | "tt"   => TRUE
		 | "ff"  => FALSE
		 | "var" => VARDec
		 | "int" => TYPEINT
		 | "bool" => TYPEBOOL
		 | "read" => TakeInput
		 | "write" => ShowOutput
		 | _        => Identifier ident
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
	      NUM (getnum (ord c - ord #"0"))
	    end
	  else if c = #"&" andalso lookahead strm = SOME #"&" then (input1 strm;BoolAnd)
	  else if c = #"|" andalso lookahead strm = SOME #"|" then (input1 strm;BoolOr)
	  else
	    case c of
(*	        #"=" => (case lookahead strm of
			     SOME #">" => (input1 strm; FNARROW)
			   | _         => EQUAL)                              *)
	        #"(" => LPAREN
	      | #")" => RPAREN
	      | #"{" => StartCommands
	      | #"}" => EndCommands
	      | #":" => (case lookahead strm of
			     SOME #":" => (input1 strm; BlockInitiator)
			    |SOME #"=" => (input1 strm; AssignOp)
			    |_	       => TypeInitiator)
	      | #";" => EndStatement
	      | #"," => COMMA
	      | #"!" => NOTOfBool
	      | #"+" => Plusop
	      | #"-" => Minusop
	      | #"~" => NegSign
	      |	#"*" => Mulop
	      | #"/" => Divop
              | #"%" => Remainderop
	      | #"=" => CompEqual
	      | #"<" => (case lookahead strm of 
			      SOME #"=" => (input1 strm; CompLessEqual)
			     |SOME #">" => (input1 strm; NotEqual)
	                     |_		=> CompLess)
	      | #">" => (case lookahead strm of
			      SOME #"=" => (input1 strm; CompGreatEqual)
			     |_		=> CompGreat)
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
	      | _    => (print ("Skipping illegal character ->" ^ str c ^" in line no."^ Int.toString(!counter) ^ " in your input file.\n");
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
  fun SourceFile file = (counter:=1;gettokens (openIn file))
  (*  Note that we could instead have called
        explode (input (openIn file))
      to get a list of all the characters in the file.
  *)
end

(*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$       Parser      $$$$$$$$$$$$$$$$$$$$$$$$$$$$*)



val list_of_tokens = SourceFile "testcase1.txt"; (*---------INPUT FILE---------*)

val Tlist = ref list_of_tokens;

fun getToken () = 
	(Tlist:=tl(!Tlist);hd(!Tlist))
fun error() = print("Syntactic Error In Your Program\n")

val tok = ref (getToken())

fun advance() = tok := getToken ()

fun eat(t) = if (!tok=t) then 
	if (t = EOF) then print("parsed Successfully\n") else advance() 
	 else error()
(*
fun Start() = (Program();eat(EOF))

and Program() = (eat(Prog);Identifier();eat(BlockInitiator);Block())(*Prog*)

and Block() = (DeclarationSeq();CommandSeq())

and DeclarationSeq() = case !tok

		of 


*)













































