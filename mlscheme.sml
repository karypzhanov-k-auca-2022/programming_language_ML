(* mlscheme.sml S379 *)


(*****************************************************************)
(*                                                               *)
(*   \FOOTNOTESIZE SHARED: NAMES, ENVIRONMENTS, STRINGS, ERRORS, PRINTING, INTERACTION, STREAMS, \&\ INITIALIZATION *)
(*                                                               *)
(*****************************************************************)

(* \footnotesize shared: names, environments, strings, errors, printing, interaction, streams, \&\ initialization S213a *)
(* for working with curried functions: [[id]], [[fst]], [[snd]], [[pair]], [[curry]], and [[curry3]] S249b *)
fun id x = x
fun fst (x, y) = x
fun snd (x, y) = y
fun pair x y = (x, y)
fun curry  f x y   = f (x, y)
fun curry3 f x y z = f (x, y, z)
(* type declarations for consistency checking *)
val _ = op fst    : ('a * 'b) -> 'a
val _ = op snd    : ('a * 'b) -> 'b
val _ = op pair   : 'a -> 'b -> 'a * 'b
val _ = op curry  : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
val _ = op curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
(* support for names and environments 303 *)
type name = string
(* support for names and environments 304 *)
type 'a env = (name * 'a) list
(* support for names and environments 305a *)
val emptyEnv = []
(* support for names and environments 305b *)
exception NotFound of name
fun find (name, []) = raise NotFound name
  | find (name, (x, v)::tail) = if name = x then v else find (name, tail)
(* support for names and environments 305c *)
fun isbound (name, []) = false
  | isbound (name, (x, v)::tail) = name = x orelse isbound (name, tail)
(* support for names and environments 305d *)
fun bind (name, v, rho) =
  (name, v) :: rho
(* support for names and environments 305e *)
exception BindListLength
fun bindList (x::vars, v::vals, rho) = bindList (vars, vals, bind (x, v, rho))
  | bindList ([], [], rho) = rho
  | bindList _ = raise BindListLength

fun mkEnv (xs, vs) = bindList (xs, vs, emptyEnv)
(* support for names and environments 305f *)
(* composition *)
infix 6 <+>
fun pairs <+> pairs' = pairs' @ pairs
(* type declarations for consistency checking *)
val _ = op emptyEnv : 'a env
(* type declarations for consistency checking *)
val _ = op find : name * 'a env -> 'a
(* type declarations for consistency checking *)
val _ = op bind : name * 'a * 'a env -> 'a env
(* type declarations for consistency checking *)
val _ = op bindList : name list * 'a list * 'a env -> 'a env
val _ = op mkEnv    : name list * 'a list -> 'a env
(* type declarations for consistency checking *)
val _ = op <+> : 'a env * 'a env -> 'a env
(* support for names and environments S219d *)
fun duplicatename [] = NONE
  | duplicatename (x::xs) =
      if List.exists (fn x' => x' = x) xs then
        SOME x
      else
        duplicatename xs
(* type declarations for consistency checking *)
val _ = op duplicatename : name list -> name option
(* exceptions used in every interpreter S213b *)
exception RuntimeError of string (* error message *)
exception LeftAsExercise of string (* string identifying code *)
(* support for detecting and signaling errors detected at run time S219e *)
exception InternalError of string (* bug in the interpreter *)
(* list functions not provided by \sml's initial basis S219a *)
fun unzip3 [] = ([], [], [])
  | unzip3 (trip::trips) =
      let val (x,  y,  z)  = trip
          val (xs, ys, zs) = unzip3 trips
      in  (x::xs, y::ys, z::zs)
      end

fun zip3 ([], [], []) = []
  | zip3 (x::xs, y::ys, z::zs) = (x, y, z) :: zip3 (xs, ys, zs)
  | zip3 _ = raise ListPair.UnequalLengths
(* type declarations for consistency checking *)
val _ = op unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val _ = op zip3   : 'a list * 'b list * 'c list -> ('a * 'b * 'c) list
(* list functions not provided by \sml's initial basis S219b *)
val reverse = rev
(* list functions not provided by \sml's initial basis S219c *)
fun optionList [] = SOME []
  | optionList (NONE :: _) = NONE
  | optionList (SOME x :: rest) =
      (case optionList rest
         of SOME xs => SOME (x :: xs)
          | NONE    => NONE)
(* type declarations for consistency checking *)
val _ = op optionList : 'a option list -> 'a list option
(* utility functions for string manipulation and printing S214c *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)
(* type declarations for consistency checking *)
val _ = op intString : int -> string
(* utility functions for string manipulation and printing S214d *)
fun plural what [x] = what
  | plural what _   = what ^ "s"

fun countString xs what =
  intString (length xs) ^ " " ^ plural what xs
(* utility functions for string manipulation and printing S214e *)
val spaceSep = String.concatWith " "   (* list separated by spaces *)
val commaSep = String.concatWith ", "  (* list separated by commas *)
(* type declarations for consistency checking *)
val _ = op spaceSep : string list -> string
val _ = op commaSep : string list -> string
(* utility functions for string manipulation and printing S214f *)
fun nullOrCommaSep empty [] = empty
  | nullOrCommaSep _     ss = commaSep ss                   
(* type declarations for consistency checking *)
val _ = op nullOrCommaSep : string -> string list -> string
(* utility functions for string manipulation and printing S215a *)
fun fnvHash s =
  let val offset_basis = 0wx011C9DC5 : Word.word  (* trim the high bit *)
      val fnv_prime    = 0w16777619  : Word.word
      fun update (c, hash) = Word.xorb (hash, Word.fromInt (ord c)) * fnv_prime
      fun int w =
        Word.toIntX w handle Overflow => Word.toInt (Word.andb (w, 0wxffffff))
  in  int (foldl update offset_basis (explode s))
  end
(* type declarations for consistency checking *)
val _ = op fnvHash : string -> int
(* utility functions for string manipulation and printing S215b *)
fun println  s = (print s; print "\n")
fun eprint   s = TextIO.output (TextIO.stdErr, s)
fun eprintln s = (eprint s; eprint "\n")
(* utility functions for string manipulation and printing S215c *)
fun predefinedFunctionError s =
  eprintln ("while reading predefined functions, " ^ s)
(* utility functions for string manipulation and printing S215d *)
val xprinter = ref print
fun xprint   s = !xprinter s
fun xprintln s = (xprint s; xprint "\n")
(* utility functions for string manipulation and printing S216a *)
fun tryFinally f x post =
  (f x handle e => (post (); raise e)) before post ()

fun withXprinter xp f x =
  let val oxp = !xprinter
      val ()  = xprinter := xp
  in  tryFinally f x (fn () => xprinter := oxp)
  end
(* type declarations for consistency checking *)
val _ = op withXprinter : (string -> unit) -> ('a -> 'b) -> ('a -> 'b)
val _ = op tryFinally   : ('a -> 'b) -> 'a -> (unit -> unit) -> 'b
(* utility functions for string manipulation and printing S216b *)
fun bprinter () =
  let val buffer = ref []
      fun bprint s = buffer := s :: !buffer
      fun contents () = concat (rev (!buffer))
  in  (bprint, contents)
  end
(* utility functions for string manipulation and printing S216c *)
fun printUTF8 code =
  let val w = Word.fromInt code
      val (&, >>) = (Word.andb, Word.>>)
      infix 6 & >>
      val _ = if (w & 0wx1fffff) <> w then
                raise RuntimeError (intString code ^
                                    " does not represent a Unicode code point")
              else
                 ()
      val printbyte = xprint o str o chr o Word.toInt
      fun prefix byte byte' = Word.orb (byte, byte')
  in  if w > 0wxffff then
        app printbyte [ prefix 0wxf0  (w >> 0w18)
                      , prefix 0wx80 ((w >> 0w12) & 0wx3f)
                      , prefix 0wx80 ((w >>  0w6) & 0wx3f)
                      , prefix 0wx80 ((w      ) & 0wx3f)
                      ]
      else if w > 0wx7ff then
        app printbyte [ prefix 0wxe0  (w >> 0w12)
                      , prefix 0wx80 ((w >>  0w6) & 0wx3f)
                      , prefix 0wx80 ((w        ) & 0wx3f)
                      ]
      else if w > 0wx7f then
        app printbyte [ prefix 0wxc0  (w >>  0w6)
                      , prefix 0wx80 ((w        ) & 0wx3f)
                      ]
      else
        printbyte w
  end
(* utility functions for string manipulation and printing S217a *)
fun stripNumericSuffix s =
      let fun stripPrefix []         = s   (* don't let things get empty *)
            | stripPrefix (#"-"::[]) = s
            | stripPrefix (#"-"::cs) = implode (reverse cs)
            | stripPrefix (c   ::cs) = if Char.isDigit c then stripPrefix cs
                                       else implode (reverse (c::cs))
      in  stripPrefix (reverse (explode s))
      end
(* support for representing errors as \ml\ values S221b *)
datatype 'a error = OK of 'a | ERROR of string
(* support for representing errors as \ml\ values S222a *)
infix 1 >>=
fun (OK x)      >>= k  =  k x
  | (ERROR msg) >>= k  =  ERROR msg
(* type declarations for consistency checking *)
val _ = op >>= : 'a error * ('a -> 'b error) -> 'b error
(* support for representing errors as \ml\ values S222b *)
infix 1 >>=+
fun e >>=+ k'  =  e >>= (OK o k')
(* type declarations for consistency checking *)
val _ = op >>=+ : 'a error * ('a -> 'b) -> 'b error
(* support for representing errors as \ml\ values S223a *)
fun errorList es =
  let fun cons (OK x, OK xs) = OK (x :: xs)
        | cons (ERROR m1, ERROR m2) = ERROR (m1 ^ "; " ^ m2)
        | cons (ERROR m, OK _) = ERROR m
        | cons (OK _, ERROR m) = ERROR m
  in  foldr cons (OK []) es
  end
(* type declarations for consistency checking *)
val _ = op errorList : 'a error list -> 'a list error
(* support for representing errors as \ml\ values S223b *)
fun errorLabel s (OK x) = OK x
  | errorLabel s (ERROR msg) = ERROR (s ^ msg)
(* type [[interactivity]] plus related functions and value S236a *)
datatype input_interactivity = PROMPTING | NOT_PROMPTING
(* type [[interactivity]] plus related functions and value S236b *)
datatype output_interactivity = ECHOING | NOT_ECHOING
(* type [[interactivity]] plus related functions and value S236c *)
type interactivity = 
  input_interactivity * output_interactivity
val noninteractive = 
  (NOT_PROMPTING, NOT_ECHOING)
fun prompts (PROMPTING,     _) = true
  | prompts (NOT_PROMPTING, _) = false
fun echoes (_, ECHOING)     = true
  | echoes (_, NOT_ECHOING) = false
(* type declarations for consistency checking *)
type interactivity = interactivity
val _ = op noninteractive : interactivity
val _ = op prompts : interactivity -> bool
val _ = op echoes  : interactivity -> bool
(* simple implementations of set operations S217b *)
type 'a set = 'a list
val emptyset = []
fun member x = 
  List.exists (fn y => y = x)
fun insert (x, ys) = 
  if member x ys then ys else x::ys
fun union (xs, ys) = foldl insert ys xs
fun inter (xs, ys) =
  List.filter (fn x => member x ys) xs
fun diff  (xs, ys) = 
  List.filter (fn x => not (member x ys)) xs
(* type declarations for consistency checking *)
type 'a set = 'a set
val _ = op emptyset : 'a set
val _ = op member   : ''a -> ''a set -> bool
val _ = op insert   : ''a     * ''a set  -> ''a set
val _ = op union    : ''a set * ''a set  -> ''a set
val _ = op inter    : ''a set * ''a set  -> ''a set
val _ = op diff     : ''a set * ''a set  -> ''a set
(* ability to interrogate environment variable [[BPCOPTIONS]] S220a *)
local
  val split = String.tokens (fn c => c = #",")
  val optionTokens =
    getOpt (Option.map split (OS.Process.getEnv "BPCOPTIONS"), [])
in
  fun hasOption s = member s optionTokens
(* type declarations for consistency checking *)
val _ = op hasOption : string -> bool
end
(* collections with mapping and combining functions S218a *)
datatype 'a collection = C of 'a set
fun elemsC (C xs) = xs
fun singleC x     = C [x]
val emptyC        = C []
(* type declarations for consistency checking *)
type 'a collection = 'a collection
val _ = op elemsC  : 'a collection -> 'a set
val _ = op singleC : 'a -> 'a collection
val _ = op emptyC  :       'a collection
(* collections with mapping and combining functions S218b *)
fun joinC     (C xs) = C (List.concat (map elemsC xs))
fun mapC  f   (C xs) = C (map f xs)
fun filterC p (C xs) = C (List.filter p xs)
fun mapC2 f (xc, yc) = joinC (mapC (fn x => mapC (fn y => f (x, y)) yc) xc)
(* type declarations for consistency checking *)
val _ = op joinC   : 'a collection collection -> 'a collection
val _ = op mapC    : ('a -> 'b)      -> ('a collection -> 'b collection)
val _ = op filterC : ('a -> bool)    -> ('a collection -> 'a collection)
val _ = op mapC2   : ('a * 'b -> 'c) -> ('a collection * 'b collection -> 'c
                                                                     collection)
(* suspensions S228a *)
datatype 'a action
  = PENDING  of unit -> 'a
  | PRODUCED of 'a

type 'a susp = 'a action ref
(* type declarations for consistency checking *)
type 'a susp = 'a susp
(* suspensions S228b *)
fun delay f = ref (PENDING f)
fun demand cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(* type declarations for consistency checking *)
val _ = op delay  : (unit -> 'a) -> 'a susp
val _ = op demand : 'a susp -> 'a
(* streams S228c *)
datatype 'a stream 
  = EOS
  | :::       of 'a * 'a stream
  | SUSPENDED of 'a stream susp
infixr 3 :::
(* streams S229a *)
fun streamGet EOS = NONE
  | streamGet (x ::: xs)    = SOME (x, xs)
  | streamGet (SUSPENDED s) = streamGet (demand s)
(* streams S229b *)
fun streamOfList xs = 
  foldr (op :::) EOS xs
(* type declarations for consistency checking *)
val _ = op streamGet : 'a stream -> ('a * 'a stream) option
(* type declarations for consistency checking *)
val _ = op streamOfList : 'a list -> 'a stream
(* streams S229c *)
fun listOfStream xs =
  case streamGet xs
    of NONE => []
     | SOME (x, xs) => x :: listOfStream xs
(* streams S229d *)
fun delayedStream action = 
  SUSPENDED (delay action)
(* type declarations for consistency checking *)
val _ = op listOfStream : 'a stream -> 'a list
(* type declarations for consistency checking *)
val _ = op delayedStream : (unit -> 'a stream) -> 'a stream
(* streams S229e *)
fun streamOfEffects action =
  delayedStream (fn () => case action ()
                            of NONE   => EOS
                             | SOME a => a ::: streamOfEffects action)
(* type declarations for consistency checking *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* streams S229f *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* type declarations for consistency checking *)
type line = line
val _ = op filelines : TextIO.instream -> line stream
(* streams S230a *)
fun streamRepeat x =
  delayedStream (fn () => x ::: streamRepeat x)
(* type declarations for consistency checking *)
val _ = op streamRepeat : 'a -> 'a stream
(* streams S230b *)
fun streamOfUnfold next state =
  delayedStream
    (fn () => case next state
                of NONE => EOS
                 | SOME (a, state') => a ::: streamOfUnfold next state')
(* type declarations for consistency checking *)
val _ = op streamOfUnfold : ('b -> ('a * 'b) option) -> 'b -> 'a stream
(* streams S230c *)
val naturals = 
  streamOfUnfold (fn n => SOME (n, n+1)) 0   (* 0 to infinity *)
(* type declarations for consistency checking *)
val _ = op naturals : int stream
(* streams S231a *)
fun preStream (pre, xs) = 
  streamOfUnfold (fn xs => (pre (); streamGet xs)) xs
(* streams S231b *)
fun postStream (xs, post) =
  streamOfUnfold (fn xs => case streamGet xs
                             of NONE => NONE
                              | head as SOME (x, _) => (post x; head)) xs
(* type declarations for consistency checking *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* type declarations for consistency checking *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* streams S231c *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* type declarations for consistency checking *)
val _ = op streamMap : ('a -> 'b) -> 'a stream -> 'b stream
(* streams S231d *)
fun streamFilter p xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => if p x then x ::: streamFilter p
                                                                              xs
                                               else streamFilter p xs)
(* type declarations for consistency checking *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* streams S231e *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* type declarations for consistency checking *)
val _ = op streamFold : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
(* streams S231f *)
fun streamZip (xs, ys) =
  delayedStream
  (fn () => case (streamGet xs, streamGet ys)
              of (SOME (x, xs), SOME (y, ys)) => (x, y) ::: streamZip (xs, ys)
               | _ => EOS)
(* streams S232a *)
fun streamConcat xss =
  let fun get (xs, xss) =
        case streamGet xs
          of SOME (x, xs) => SOME (x, (xs, xss))
           | NONE => case streamGet xss
                       of SOME (xs, xss) => get (xs, xss)
                        | NONE => NONE
  in  streamOfUnfold get (EOS, xss)
  end
(* type declarations for consistency checking *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* type declarations for consistency checking *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* streams S232b *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* type declarations for consistency checking *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* streams S232c *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* type declarations for consistency checking *)
val _ = op @@@ : 'a stream * 'a stream -> 'a stream
(* streams S232d *)
fun streamTake (0, xs) = []
  | streamTake (n, xs) =
      case streamGet xs
        of SOME (x, xs) => x :: streamTake (n-1, xs)
         | NONE => []
(* type declarations for consistency checking *)
val _ = op streamTake : int * 'a stream -> 'a list
(* streams S232e *)
fun streamDrop (0, xs) = xs
  | streamDrop (n, xs) =
      case streamGet xs
        of SOME (_, xs) => streamDrop (n-1, xs)
         | NONE => EOS
(* type declarations for consistency checking *)
val _ = op streamDrop : int * 'a stream -> 'a stream
(* stream transformers and their combinators S246b *)
type ('a, 'b) xformer = 
  'a stream -> ('b error * 'a stream) option
(* type declarations for consistency checking *)
type ('a, 'b) xformer = ('a, 'b) xformer
(* stream transformers and their combinators S247a *)
fun pure y = fn xs => SOME (OK y, xs)
(* type declarations for consistency checking *)
val _ = op pure : 'b -> ('a, 'b) xformer
(* stream transformers and their combinators S247b *)
infix 3 <*>
fun tx_f <*> tx_b =
  fn xs => case tx_f xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK f, xs) =>
                  case tx_b xs
                    of NONE => NONE
                     | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
                     | SOME (OK y, xs) => SOME (OK (f y), xs)
(* type declarations for consistency checking *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S249a *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* type declarations for consistency checking *)
val _ = op <$> : ('b -> 'c) * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S249c *)
infix 1 <|>
fun t1 <|> t2 = (fn xs => case t1 xs of SOME y => SOME y | NONE => t2 xs) 
(* type declarations for consistency checking *)
val _ = op <|> : ('a, 'b) xformer * ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S249d *)
fun pzero _ = NONE
(* stream transformers and their combinators S249e *)
fun anyParser ts = 
  foldr op <|> pzero ts
(* type declarations for consistency checking *)
val _ = op pzero : ('a, 'b) xformer
(* type declarations for consistency checking *)
val _ = op anyParser : ('a, 'b) xformer list -> ('a, 'b) xformer
(* stream transformers and their combinators S250a *)
infix 6 <* *>
fun p1 <*  p2 = curry fst <$> p1 <*> p2
fun p1  *> p2 = curry snd <$> p1 <*> p2

infixr 4 <$
fun v <$ p = (fn _ => v) <$> p
(* type declarations for consistency checking *)
val _ = op <*  : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'b) xformer
val _ = op  *> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
val _ = op <$  : 'b               * ('a, 'c) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S250b *)
fun one xs = case streamGet xs
               of NONE => NONE
                | SOME (x, xs) => SOME (OK x, xs)
(* type declarations for consistency checking *)
val _ = op one : ('a, 'a) xformer
(* stream transformers and their combinators S250c *)
fun eos xs = case streamGet xs
               of NONE => SOME (OK (), EOS)
                | SOME _ => NONE
(* type declarations for consistency checking *)
val _ = op eos : ('a, unit) xformer
(* stream transformers and their combinators S251a *)
fun peek tx xs =
  case tx xs of SOME (OK y, _) => SOME y
              | _ => NONE
(* type declarations for consistency checking *)
val _ = op peek : ('a, 'b) xformer -> 'a stream -> 'b option
(* stream transformers and their combinators S251b *)
fun rewind tx xs =
  case tx xs of SOME (ey, _) => SOME (ey, xs)
              | NONE => NONE
(* type declarations for consistency checking *)
val _ = op rewind : ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S251c *)
fun sat p tx xs =
  case tx xs
    of answer as SOME (OK y, xs) => if p y then answer else NONE
     | answer => answer
(* type declarations for consistency checking *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S252a *)
fun eqx y = 
  sat (fn y' => y = y') 
(* type declarations for consistency checking *)
val _ = op eqx : ''b -> ('a, ''b) xformer -> ('a, ''b) xformer
(* stream transformers and their combinators S252b *)
infixr 4 <$>?
fun f <$>? tx =
  fn xs => case tx xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK y, xs) =>
                  case f y
                    of NONE => NONE
                     | SOME z => SOME (OK z, xs)
(* type declarations for consistency checking *)
val _ = op <$>? : ('b -> 'c option) * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S252c *)
infix 3 <&>
fun t1 <&> t2 = fn xs =>
  case t1 xs
    of SOME (OK _, _) => t2 xs
     | SOME (ERROR _, _) => NONE    
     | NONE => NONE
(* type declarations for consistency checking *)
val _ = op <&> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S252d *)
fun notFollowedBy t xs =
  case t xs
    of NONE => SOME (OK (), xs)
     | SOME _ => NONE
(* type declarations for consistency checking *)
val _ = op notFollowedBy : ('a, 'b) xformer -> ('a, unit) xformer
(* stream transformers and their combinators S253a *)
fun many t = 
  curry (op ::) <$> t <*> (fn xs => many t xs) <|> pure []
(* type declarations for consistency checking *)
val _ = op many  : ('a, 'b) xformer -> ('a, 'b list) xformer
(* stream transformers and their combinators S253b *)
fun many1 t = 
  curry (op ::) <$> t <*> many t
(* type declarations for consistency checking *)
val _ = op many1 : ('a, 'b) xformer -> ('a, 'b list) xformer
(* stream transformers and their combinators S253c *)
fun optional t = 
  SOME <$> t <|> pure NONE
(* type declarations for consistency checking *)
val _ = op optional : ('a, 'b) xformer -> ('a, 'b option) xformer
(* stream transformers and their combinators S254a *)
infix 2 <*>!
fun tx_ef <*>! tx_x =
  fn xs => case (tx_ef <*> tx_x) xs
             of NONE => NONE
              | SOME (OK (OK y),      xs) => SOME (OK y,      xs)
              | SOME (OK (ERROR msg), xs) => SOME (ERROR msg, xs)
              | SOME (ERROR msg,      xs) => SOME (ERROR msg, xs)
infixr 4 <$>!
fun ef <$>! tx_x = pure ef <*>! tx_x
(* type declarations for consistency checking *)
val _ = op <*>! : ('a, 'b -> 'c error) xformer * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
val _ = op <$>! : ('b -> 'c error)                 * ('a, 'b) xformer -> ('a, 'c
                                                                       ) xformer
(* support for source-code locations and located streams S233b *)
type srcloc = string * int
fun srclocString (source, line) =
  source ^ ", line " ^ intString line
(* support for source-code locations and located streams S233c *)
datatype error_format = WITH_LOCATIONS | WITHOUT_LOCATIONS
val toplevel_error_format = ref WITH_LOCATIONS
(* support for source-code locations and located streams S233d *)
fun synerrormsg (source, line) strings =
  if !toplevel_error_format = WITHOUT_LOCATIONS
  andalso source = "standard input"
  then
    concat ("syntax error: " :: strings)
  else    
    concat ("syntax error in " :: srclocString (source, line) :: ": " :: strings
                                                                               )

(* support for source-code locations and located streams S234a *)
fun warnAt (source, line) strings =
  ( app eprint
      (if !toplevel_error_format = WITHOUT_LOCATIONS
       andalso source = "standard input"
       then
         "warning: " :: strings
       else
         "warning in " :: srclocString (source, line) :: ": " :: strings)
  ; eprint "\n"
  )
(* support for source-code locations and located streams S234b *)
exception Located of srcloc * exn
(* support for source-code locations and located streams S234c *)
type 'a located = srcloc * 'a
(* type declarations for consistency checking *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* type declarations for consistency checking *)
type 'a located = 'a located
(* support for source-code locations and located streams S234d *)
fun atLoc loc f a =
  f a handle e as RuntimeError _ => raise Located (loc, e)
           | e as NotFound _     => raise Located (loc, e)
           (* more handlers for [[atLoc]] S234e *)
           | e as IO.Io _   => raise Located (loc, e)
           | e as Div       => raise Located (loc, e)
           | e as Overflow  => raise Located (loc, e)
           | e as Subscript => raise Located (loc, e)
           | e as Size      => raise Located (loc, e)
(* type declarations for consistency checking *)
val _ = op atLoc : srcloc -> ('a -> 'b) -> ('a -> 'b)
(* support for source-code locations and located streams S234f *)
fun located f (loc, a) = atLoc loc f a
fun leftLocated f ((loc, a), b) = atLoc loc f (a, b)
(* type declarations for consistency checking *)
val _ = op located : ('a -> 'b) -> ('a located -> 'b)
val _ = op leftLocated : ('a * 'b -> 'c) -> ('a located * 'b -> 'c)
(* support for source-code locations and located streams S235a *)
fun fillComplaintTemplate (s, maybeLoc) =
  let val string_to_fill = " <at loc>"
      val (prefix, atloc) =
        Substring.position string_to_fill (Substring.full s)
      val suffix = Substring.triml (size string_to_fill) atloc
      val splice_in =
        Substring.full
          (case maybeLoc
             of NONE => ""
              | SOME (loc as (file, line)) =>
                  if !toplevel_error_format = WITHOUT_LOCATIONS
                  andalso file = "standard input"
                  then
                    ""
                  else
                    " in " ^ srclocString loc)
  in  if Substring.size atloc = 0 then (* <at loc> is not present *)
        s
      else
        Substring.concat [prefix, splice_in, suffix]
  end
fun fillAtLoc (s, loc) = fillComplaintTemplate (s, SOME loc)
fun stripAtLoc s = fillComplaintTemplate (s, NONE)
(* type declarations for consistency checking *)
val _ = op fillComplaintTemplate : string * srcloc option -> string
(* support for source-code locations and located streams S235b *)
fun synerrorAt msg loc = 
  ERROR (synerrormsg loc [msg])
(* support for source-code locations and located streams S235c *)
fun locatedStream (streamname, inputs) =
  let val locations =
        streamZip (streamRepeat streamname, streamDrop (1, naturals))
  in  streamZip (locations, inputs)
  end
(* type declarations for consistency checking *)
val _ = op synerrorAt : string -> srcloc -> 'a error
(* type declarations for consistency checking *)
val _ = op locatedStream : string * line stream -> line located stream
(* streams that track line boundaries S258 *)
datatype 'a eol_marked
  = EOL of int (* number of the line that ends here *)
  | INLINE of 'a

fun drainLine EOS = EOS
  | drainLine (SUSPENDED s)     = drainLine (demand s)
  | drainLine (EOL _    ::: xs) = xs
  | drainLine (INLINE _ ::: xs) = drainLine xs
(* streams that track line boundaries S259a *)
local 
  fun asEol (EOL n) = SOME n
    | asEol (INLINE _) = NONE
  fun asInline (INLINE x) = SOME x
    | asInline (EOL _)    = NONE
in
  fun eol    xs = (asEol    <$>? one) xs
  fun inline xs = (asInline <$>? many eol *> one) xs
  fun srcloc xs = rewind (fst <$> inline) xs
end
(* type declarations for consistency checking *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* type declarations for consistency checking *)
val _ = op eol      : ('a eol_marked, int) xformer
val _ = op inline   : ('a eol_marked, 'a)  xformer
val _ = op srcloc   : ('a located eol_marked, srcloc) xformer
(* support for lexical analysis S254b *)
type 'a lexer = (char, 'a) xformer
(* type declarations for consistency checking *)
type 'a lexer = 'a lexer
(* support for lexical analysis S254c *)
fun isDelim c =
  Char.isSpace c orelse Char.contains "()[]{};" c
(* type declarations for consistency checking *)
val _ = op isDelim : char -> bool
(* support for lexical analysis S254d *)
val whitespace = many (sat Char.isSpace one)
(* type declarations for consistency checking *)
val _ = op whitespace : char list lexer
(* support for lexical analysis S256a *)
fun intChars isDelim = 
  (curry (op ::) <$> eqx #"-" one <|> pure id) <*>
  many1 (sat Char.isDigit one) <* 
  notFollowedBy (sat (not o isDelim) one)
(* type declarations for consistency checking *)
val _ = op intChars : (char -> bool) -> char list lexer
(* support for lexical analysis S256b *)
fun intFromChars (#"-" :: cs) = 
      intFromChars cs >>=+ Int.~
  | intFromChars cs =
      (OK o valOf o Int.fromString o implode) cs
      handle Overflow =>
        ERROR "this interpreter can't read arbitrarily large integers"
(* type declarations for consistency checking *)
val _ = op intFromChars : char list -> int error
(* support for lexical analysis S256c *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* type declarations for consistency checking *)
val _ = op intToken : (char -> bool) -> int lexer
(* support for lexical analysis S256d *)
datatype bracket_shape = ROUND | SQUARE | CURLY
(* support for lexical analysis S257a *)
datatype 'a plus_brackets
  = LEFT  of bracket_shape
  | RIGHT of bracket_shape
  | PRETOKEN of 'a

fun bracketLexer pretoken
  =  LEFT  ROUND  <$ eqx #"(" one
 <|> LEFT  SQUARE <$ eqx #"[" one
 <|> LEFT  CURLY  <$ eqx #"{" one
 <|> RIGHT ROUND  <$ eqx #")" one
 <|> RIGHT SQUARE <$ eqx #"]" one
 <|> RIGHT CURLY  <$ eqx #"}" one
 <|> PRETOKEN <$> pretoken
(* type declarations for consistency checking *)
type 'a plus_brackets = 'a plus_brackets
val _ = op bracketLexer : 'a lexer -> 'a plus_brackets lexer
(* support for lexical analysis S257b *)
fun leftString ROUND  = "("
  | leftString SQUARE = "["
  | leftString CURLY  = "{"
fun rightString ROUND  = ")"
  | rightString SQUARE = "]"
  | rightString CURLY = "}"

fun plusBracketsString _   (LEFT shape)  = leftString shape
  | plusBracketsString _   (RIGHT shape) = rightString shape
  | plusBracketsString pts (PRETOKEN pt)  = pts pt
(* type declarations for consistency checking *)
val _ = op plusBracketsString : ('a -> string) -> ('a plus_brackets -> string)
(* common parsing code S246a *)
(* combinators and utilities for parsing located streams S259b *)
type ('t, 'a) polyparser = ('t located eol_marked, 'a) xformer
(* combinators and utilities for parsing located streams S259c *)
fun token    stream = (snd <$> inline)      stream
fun noTokens stream = (notFollowedBy token) stream
(* type declarations for consistency checking *)
val _ = op token    : ('t, 't)   polyparser
val _ = op noTokens : ('t, unit) polyparser
(* combinators and utilities for parsing located streams S260a *)
fun @@ p = pair <$> srcloc <*> p
(* type declarations for consistency checking *)
val _ = op @@ : ('t, 'a) polyparser -> ('t, 'a located) polyparser
(* combinators and utilities for parsing located streams S260b *)
fun asAscii p =
  let fun good c = Char.isPrint c andalso Char.isAscii c
      fun warn (loc, s) =
        case List.find (not o good) (explode s)
          of NONE => OK s
           | SOME c => 
               let val msg =
                     String.concat ["name \"", s, "\" contains the ",
                                    "non-ASCII or non-printing byte \"",
                                    Char.toCString c, "\""]
               in  synerrorAt msg loc
               end
  in  warn <$>! @@ p
  end
(* combinators and utilities for parsing located streams S260c *)
infix 0 <?>
fun p <?> what = p <|> synerrorAt ("expected " ^ what) <$>! srcloc
(* type declarations for consistency checking *)
val _ = op asAscii : ('t, string) polyparser -> ('t, string) polyparser
(* type declarations for consistency checking *)
val _ = op <?> : ('t, 'a) polyparser * string -> ('t, 'a) polyparser
(* combinators and utilities for parsing located streams S261a *)
infix 4 <!>
fun p <!> msg =
  fn tokens => (case p tokens
                  of SOME (OK _, unread) =>
                       let val outcome =
                             case peek srcloc tokens
                              of SOME loc => synerrorAt msg loc
                               | NONE => ERROR msg
                                             
                       in  SOME (outcome, unread)
                       end
                   | _ => NONE)
(* type declarations for consistency checking *)
val _ = op <!> : ('t, 'a) polyparser * string -> ('t, 'b) polyparser
(* combinators and utilities for parsing located streams S265a *)
fun nodups (what, context) (loc, names) =
  let fun dup [] = OK names
        | dup (x::xs) =
            if List.exists (fn y => y = x) xs then
              synerrorAt (what ^ " " ^ x ^ " appears twice in " ^ context) loc
            else
              dup xs
  in  dup names
  end
(* type declarations for consistency checking *)
val _ = op nodups : string * string -> srcloc * name list -> name list error
(* combinators and utilities for parsing located streams S265b *)
fun rejectReserved reserved x =
  if member x reserved then
    ERROR ("syntax error: " ^ x ^ " is a reserved word and " ^
           "may not be used to name a variable or function")
  else
    OK x
(* type declarations for consistency checking *)
val _ = op rejectReserved : name list -> name -> name error
(* transformers for interchangeable brackets S261c *)
fun left  tokens = ((fn (loc, LEFT  s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun right tokens = ((fn (loc, RIGHT s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun pretoken stream = ((fn PRETOKEN t => SOME t | _ => NONE) <$>? token) stream
(* type declarations for consistency checking *)
val _ = op left  : ('t plus_brackets, bracket_shape located) polyparser
val _ = op right : ('t plus_brackets, bracket_shape located) polyparser
val _ = op pretoken : ('t plus_brackets, 't) polyparser
(* transformers for interchangeable brackets S262a *)
fun badRight msg =
  (fn (loc, shape) => synerrorAt (msg ^ " " ^ rightString shape) loc) <$>! right
(* type declarations for consistency checking *)
val _ = op badRight : string -> ('t plus_brackets, 'a) polyparser
(* transformers for interchangeable brackets S262b *)
fun notCurly (_, CURLY) = false
  | notCurly _          = true
fun leftCurly tokens = sat (not o notCurly) left tokens
(* transformers for interchangeable brackets S262c *)
(* definition of function [[errorAtEnd]] S261b *)
infix 4 errorAtEnd
fun p errorAtEnd mkMsg =
  fn tokens => 
    (case (p <* rewind right) tokens
       of SOME (OK s, unread) =>
            let val outcome =
                  case peek srcloc tokens
                    of SOME loc => synerrorAt ((concat o mkMsg) s) loc
                     | NONE => ERROR ((concat o mkMsg) s)
            in  SOME (outcome, unread)
            end
        | _ => NONE)
(* type declarations for consistency checking *)
val _ = op errorAtEnd : ('t plus_brackets, 'a) polyparser * ('a -> string list)
                                            -> ('t plus_brackets, 'b) polyparser
(* transformers for interchangeable brackets S262d *)
datatype right_result
  = FOUND_RIGHT      of bracket_shape located
  | SCANNED_TO_RIGHT of srcloc  (* location where scanning started *)
  | NO_RIGHT
(* type declarations for consistency checking *)
val _ = op notCurly  : bracket_shape located -> bool
val _ = op leftCurly : ('t plus_brackets, bracket_shape located) polyparser
(* type declarations for consistency checking *)
type right_result = right_result
(* transformers for interchangeable brackets S263a *)
type ('t, 'a) pb_parser = ('t plus_brackets, 'a) polyparser
fun matchingRight tokens =
  let fun scanToClose tokens = 
        let val loc = getOpt (peek srcloc tokens, ("end of stream", 9999))
            fun scan nlp tokens =
              (* nlp is the number of unmatched left parentheses *)
              case tokens
                of EOL _                  ::: tokens => scan nlp tokens
                 | INLINE (_, PRETOKEN _) ::: tokens => scan nlp tokens
                 | INLINE (_, LEFT  _)    ::: tokens => scan (nlp+1) tokens
                 | INLINE (_, RIGHT _)    ::: tokens =>
                     if nlp = 0 then
                       pure (SCANNED_TO_RIGHT loc) tokens
                     else
                       scan (nlp-1) tokens
                 | EOS         => pure NO_RIGHT tokens
                 | SUSPENDED s => scan nlp (demand s)
        in  scan 0 tokens
        end
  in  (FOUND_RIGHT <$> right <|> scanToClose) tokens
  end
(* type declarations for consistency checking *)
val _ = op matchingRight : ('t, right_result) pb_parser
(* transformers for interchangeable brackets S263b *)
fun matchBrackets _ (loc, left) a (FOUND_RIGHT (loc', right)) =
      if left = right then
        OK a
      else
        synerrorAt (rightString right ^ " does not match " ^ leftString left ^
                 (if loc <> loc' then " at " ^ srclocString loc else "")) loc'
  | matchBrackets _ (loc, left) _ NO_RIGHT =
      synerrorAt ("unmatched " ^ leftString left) loc
  | matchBrackets e (loc, left) _ (SCANNED_TO_RIGHT loc') =
      synerrorAt ("expected " ^ e) loc
(* type declarations for consistency checking *)
val _ = op matchBrackets : string -> bracket_shape located -> 'a -> right_result
                                                                     -> 'a error
(* transformers for interchangeable brackets S264a *)

fun liberalBracket (expected, p) =
  matchBrackets expected <$> sat notCurly left <*> p <*>! matchingRight
fun bracket (expected, p) =
  liberalBracket (expected, p <?> expected)
fun curlyBracket (expected, p) =
  matchBrackets expected <$> leftCurly <*> (p <?> expected) <*>! matchingRight
fun bracketKeyword (keyword, expected, p) =
  liberalBracket (expected, keyword *> (p <?> expected))
(* type declarations for consistency checking *)
val _ = op liberalBracket : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op bracket           : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op curlyBracket    : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
(* type declarations for consistency checking *)
val _ = op bracketKeyword : ('t, 'keyword) pb_parser * string * ('t, 'a)
                                                 pb_parser -> ('t, 'a) pb_parser
(* transformers for interchangeable brackets S264b *)
fun usageParser keyword =
  let val left = eqx #"(" one <|> eqx #"[" one
      val getkeyword = left *> (implode <$> many1 (sat (not o isDelim) one))
  in  fn (usage, p) =>
        case getkeyword (streamOfList (explode usage))
          of SOME (OK k, _) => bracketKeyword (keyword k, usage, p)
           | _ => raise InternalError ("malformed usage string: " ^ usage)
  end
(* type declarations for consistency checking *)
val _ = op usageParser : (string -> ('t, string) pb_parser) ->
                               string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
(* code used to debug parsers S265c *)
fun safeTokens stream =
  let fun tokens (seenEol, seenSuspended) =
            let fun get (EOL _         ::: ts) = if seenSuspended then []
                                                 else tokens (true, false) ts
                  | get (INLINE (_, t) ::: ts) = t :: get ts
                  | get  EOS                   = []
                  | get (SUSPENDED (ref (PRODUCED ts))) = get ts
                  | get (SUSPENDED s) = if seenEol then []
                                        else tokens (false, true) (demand s)
            in   get
            end
  in  tokens (false, false) stream
  end
(* type declarations for consistency checking *)
val _ = op safeTokens : 'a located eol_marked stream -> 'a list
(* code used to debug parsers S266a *)
fun showErrorInput asString p tokens =
  case p tokens
    of result as SOME (ERROR msg, rest) =>
         if String.isSubstring " [input: " msg then
           result
         else
           SOME (ERROR (msg ^ " [input: " ^
                        spaceSep (map asString (safeTokens tokens)) ^ "]"),
               rest)
     | result => result
(* type declarations for consistency checking *)
val _ = op showErrorInput : ('t -> string) -> ('t, 'a) polyparser -> ('t, 'a)
                                                                      polyparser
(* code used to debug parsers S266b *)
fun wrapAround tokenString what p tokens =
  let fun t tok = " " ^ tokenString tok
      val _ = app eprint ["Looking for ", what, " at"]
      val _ = app (eprint o t) (safeTokens tokens)
      val _ = eprint "\n"
      val answer = p tokens
      val _ = app eprint [ case answer of NONE => "Didn't find "
                                        | SOME _ => "Found "
                         , what, "\n"
                         ]
  in  answer
  end handle e =>
        ( app eprint ["Search for ", what, " raised ", exnName e, "\n"]
        ; raise e
        )
(* type declarations for consistency checking *)
val _ = op wrapAround : ('t -> string) -> string -> ('t, 'a) polyparser -> ('t,
                                                                  'a) polyparser
(* streams that issue two forms of prompts S267a *)
fun echoTagStream lines = 
  let fun echoIfTagged line =
        if (String.substring (line, 0, 2) = ";#" handle _ => false) then
          print line
        else
          ()
  in  postStream (lines, echoIfTagged)
  end
(* type declarations for consistency checking *)
val _ = op echoTagStream : line stream -> line stream 
(* streams that issue two forms of prompts S267b *)
fun stripAndReportErrors xs =
  let fun next xs =
        case streamGet xs
          of SOME (ERROR msg, xs) => (eprintln msg; next xs)
           | SOME (OK x, xs) => SOME (x, xs)
           | NONE => NONE
  in  streamOfUnfold next xs
  end
(* type declarations for consistency checking *)
val _ = op stripAndReportErrors : 'a error stream -> 'a stream
(* streams that issue two forms of prompts S267c *)
fun lexLineWith lexer =
  stripAndReportErrors o streamOfUnfold lexer o streamOfList o explode
(* type declarations for consistency checking *)
val _ = op lexLineWith : 't lexer -> line -> 't stream
(* streams that issue two forms of prompts S267d *)
fun parseWithErrors parser =
  let fun adjust (SOME (ERROR msg, tokens)) =
            SOME (ERROR msg, drainLine tokens)
        | adjust other = other
  in  streamOfUnfold (adjust o parser)
  end
(* type declarations for consistency checking *)
val _ = op parseWithErrors : ('t, 'a) polyparser ->                     't
                                    located eol_marked stream -> 'a error stream
(* streams that issue two forms of prompts S268 *)
type prompts   = { ps1 : string, ps2 : string }
val stdPrompts = { ps1 = "-> ", ps2 = "   " }
val noPrompts  = { ps1 = "", ps2 = "" }
(* type declarations for consistency checking *)
type prompts = prompts
val _ = op stdPrompts : prompts
val _ = op noPrompts  : prompts
(* streams that issue two forms of prompts S269 *)
fun ('t, 'a) interactiveParsedStream (lexer, parser) (name, lines, prompts) =
  let val { ps1, ps2 } = prompts
      val thePrompt = ref ps1
      fun setPrompt ps = fn _ => thePrompt := ps

      val lines = preStream (fn () => print (!thePrompt), echoTagStream lines)

      fun lexAndDecorate (loc, line) =
        let val tokens = postStream (lexLineWith lexer line, setPrompt ps2)
        in  streamMap INLINE (streamZip (streamRepeat loc, tokens)) @@@
            streamOfList [EOL (snd loc)]
        end

      val xdefs_with_errors : 'a error stream = 
        (parseWithErrors parser o streamConcatMap lexAndDecorate o locatedStream
                                                                               )
        (name, lines)
(* type declarations for consistency checking *)
val _ = op interactiveParsedStream : 't lexer * ('t, 'a) polyparser ->
                                     string * line stream * prompts -> 'a stream
val _ = op lexAndDecorate : srcloc * line -> 't located eol_marked stream
  in  
      stripAndReportErrors (preStream (setPrompt ps1, xdefs_with_errors))
  end 
(* common parsing code ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
fun ('t, 'a) finiteStreamOfLine fail (lexer, parser) line =
  let val lines = streamOfList [line] @@@ streamOfEffects fail
      fun lexAndDecorate (loc, line) =
        let val tokens = lexLineWith lexer line
        in  streamMap INLINE (streamZip (streamRepeat loc, tokens)) @@@
            streamOfList [EOL (snd loc)]
        end

      val things_with_errors : 'a error stream = 
        (parseWithErrors parser o streamConcatMap lexAndDecorate o locatedStream
                                                                               )
        ("command line", lines)
  in  
      stripAndReportErrors things_with_errors
  end 
val _ = finiteStreamOfLine :
          (unit -> string option) -> 't lexer * ('t, 'a) polyparser -> line ->
                                                                       'a stream
(* shared utility functions for initializing interpreters S240a *)
fun override_if_testing () =                           (*OMIT*)
  if isSome (OS.Process.getEnv "NOERRORLOC") then      (*OMIT*)
    toplevel_error_format := WITHOUT_LOCATIONS         (*OMIT*)
  else                                                 (*OMIT*)
    ()                                                 (*OMIT*)
fun setup_error_format interactivity =
  if prompts interactivity then
    toplevel_error_format := WITHOUT_LOCATIONS
    before override_if_testing () (*OMIT*)
  else
    toplevel_error_format := WITH_LOCATIONS
    before override_if_testing () (*OMIT*)
(* function application with overflow checking S220b *)
local
  val defaultRecursionLimit = 6000
  val recursionLimit = ref defaultRecursionLimit
  datatype checkpoint = RECURSION_LIMIT of int

  val evalFuel = ref 1000000
  val throttleCPU = not (hasOption "nothrottle")
in
  (* manipulate recursion limit *)
  fun checkpointLimit () = RECURSION_LIMIT (!recursionLimit)
  fun restoreLimit (RECURSION_LIMIT n) = recursionLimit := n

  (* work with fuel *)
  val defaultEvalFuel = ref (!evalFuel)
  fun fuelRemaining () = !evalFuel
  fun withFuel n f x = 
    let val old = !evalFuel
        val _ = evalFuel := n
    in  (f x before evalFuel := old) handle e => (evalFuel := old; raise e)
    end
(* function application with overflow checking S221a *)
  (* convert function `f` to respect computation limits *)
  fun applyWithLimits f =
    if !recursionLimit <= 0 then
      ( recursionLimit := defaultRecursionLimit
      ; raise RuntimeError "recursion too deep"
      )
    else if throttleCPU andalso !evalFuel <= 0 then
      ( evalFuel := !defaultEvalFuel
      ; raise RuntimeError "CPU time exhausted"
      )
    else
      let val _ = recursionLimit := !recursionLimit - 1
          val _ = evalFuel       := !evalFuel - 1
      in  fn arg => f arg before (recursionLimit := !recursionLimit + 1)
      end
  fun resetComputationLimits () = ( recursionLimit := defaultRecursionLimit
                                  ; evalFuel := !defaultEvalFuel
                                  )
end



(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX AND VALUES FOR \USCHEME                     *)
(*                                                               *)
(*****************************************************************)

(* abstract syntax and values for \uscheme S380a *)
(* definitions of [[exp]] and [[value]] for \uscheme 306 *)
datatype exp   = LITERAL of value
               | VAR     of name
               | SET     of name * exp
               | IFX     of exp * exp * exp
               | WHILEX  of exp * exp
               | BEGIN   of exp list
               | APPLY   of exp * exp list
               | LETX    of let_flavor * (name * exp) list * exp
               | LAMBDA  of lambda
               | OR      of exp * exp
               | AND     of exp * exp
and let_flavor = LET | LETREC | LETSTAR
and      value = SYM       of name
               | NUM       of int
               | BOOLV     of bool   
               | NIL
               | PAIR      of value * value
               | CLOSURE   of lambda * value ref env
               | PRIMITIVE of primitive
withtype primitive = exp * value list -> value (* raises RuntimeError *)
     and lambda    = name list * exp
(* definition of [[def]] for \uscheme 307a *)
datatype def  = VAL    of name * exp
              | EXP    of exp
              | DEFINE of name * lambda
(* definition of [[unit_test]] for untyped languages (shared) S214a *)
datatype unit_test = CHECK_EXPECT of exp * exp
                   | CHECK_ASSERT of exp
                   | CHECK_ERROR  of exp
(* definition of [[xdef]] (shared) S214b *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* definition of [[valueString]] for \uscheme, \tuscheme, and \nml 307b *)
fun valueString (SYM v)   = v
  | valueString (NUM n)   = intString n
  | valueString (BOOLV b) = if b then "#t" else "#f"
  | valueString (NIL)     = "()"
  | valueString (PAIR (car, cdr))  = 
      let fun tail (PAIR (car, cdr)) = " " ^ valueString car ^ tail cdr
            | tail NIL = ")"
            | tail v = " . " ^ valueString v ^ ")"
(* type declarations for consistency checking *)
val _ = op valueString : value -> string
      in  "(" ^ valueString car ^ tail cdr
      end
  | valueString (CLOSURE   _) = "<function>"
  | valueString (PRIMITIVE _) = "<function>"
(* definition of [[expString]] for \uscheme S383b *)
fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun withBindings (keyword, bs, e) =
        bracket (spaceSep [keyword, bindings bs, expString e])
      and bindings bs = bracket (spaceSep (map binding bs))
      and binding (x, e) = bracket (x ^ " " ^ expString e)
      val letkind = fn LET => "let" | LETSTAR => "let*" | LETREC => "letrec"
  in  case e
        of LITERAL (v as NUM   _) => valueString v
         | LITERAL (v as BOOLV _) => valueString v
         | LITERAL v => "'" ^ valueString v
         | VAR name => name
         | SET (x, e) => bracketSpace ["set", x, expString e]
         | IFX (e1, e2, e3) => bracketSpace ("if" :: exps [e1, e2, e3])
         | WHILEX (cond, body) =>
                         bracketSpace ["while", expString cond, expString body]
         | BEGIN es => bracketSpace ("begin" :: exps es)
         | APPLY (e, es) => bracketSpace (exps (e::es))
         | LETX (lk, bs, e) => bracketSpace [letkind lk, bindings bs, expString
                                                                              e]
         | LAMBDA (xs, body) => bracketSpace ["lambda", bracketSpace xs,
                                                                 expString body]
          | OR (e1, e2) => "or(" ^ expString e1 ^ ", " ^ expString e2 ^ ")"
          | AND (e1, e2) => "and(" ^ expString e1 ^ ", " ^ expString e2 ^ ")"
  end
(* type declarations for consistency checking *)
val _ = op valueString      : value -> string
val _ = op expString        : exp   -> string


(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON VALUES ({\FOOTNOTESIZE \USCHEME, \TUSCHEME, \NML}) *)
(*                                                               *)
(*****************************************************************)

(* utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml}) 308a *)
fun embedInt n = NUM n
fun projectInt (NUM n) = n
  | projectInt v =
      raise RuntimeError ("value " ^ valueString v ^ " is not an integer")
(* type declarations for consistency checking *)
val _ = op embedInt   : int   -> value
val _ = op projectInt : value -> int
(* utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml}) 308b *)
fun embedBool b = BOOLV b
fun projectBool (BOOLV false) = false
  | projectBool _             = true
(* type declarations for consistency checking *)
val _ = op embedBool   : bool  -> value
val _ = op projectBool : value -> bool
(* utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml}) 308c *)
fun embedList []     = NIL
  | embedList (h::t) = PAIR (h, embedList t)
(* utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml}) S380c *)
fun equalatoms (NIL,      NIL    )  = true
  | equalatoms (NUM  n1,  NUM  n2)  = (n1 = n2)
  | equalatoms (SYM  v1,  SYM  v2)  = (v1 = v2)
  | equalatoms (BOOLV b1, BOOLV b2) = (b1 = b2)
  | equalatoms  _                   = false
(* type declarations for consistency checking *)
val _ = op equalatoms : value * value -> bool
(* utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml}) S380d *)
fun equalpairs (PAIR (car1, cdr1), PAIR (car2, cdr2)) =
      equalpairs (car1, car2) andalso equalpairs (cdr1, cdr2)
  | equalpairs (v1, v2) = equalatoms (v1, v2)
(* type declarations for consistency checking *)
val _ = op equalpairs : value * value -> bool
(* utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml}) S380e *)
val testEquals = equalpairs
(* type declarations for consistency checking *)
val _ = op testEquals : value * value -> bool
(* utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml}) S389c *)
fun cycleThrough xs =
  let val remaining = ref xs
      fun next () = case !remaining
                      of [] => (remaining := xs; next ())
                       | x :: xs => (remaining := xs; x)
  in  if null xs then
        raise InternalError "empty list given to cycleThrough"
      else
        next
  end
val unspecified =
  cycleThrough [ BOOLV true, NUM 39, SYM "this value is unspecified", NIL
               , PRIMITIVE (fn _ => raise RuntimeError "unspecified primitive")
               ]
(* type declarations for consistency checking *)
val _ = op cycleThrough : 'a list -> (unit -> 'a)
val _ = op unspecified  : unit -> value



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \USCHEME, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* lexical analysis and parsing for \uscheme, providing [[filexdefs]] and [[stringsxdefs]] S383c *)
(* lexical analysis for \uscheme\ and related languages S383d *)
datatype pretoken = QUOTE
                  | INT     of int
                  | SHARP   of bool
                  | NAME    of string
type token = pretoken plus_brackets
(* type declarations for consistency checking *)
type pretoken = pretoken
type token = token
(* lexical analysis for \uscheme\ and related languages S384a *)
fun pretokenString (QUOTE)     = "'"
  | pretokenString (INT  n)    = intString n
  | pretokenString (SHARP b)   = if b then "#t" else "#f"
  | pretokenString (NAME x)    = x
val tokenString = plusBracketsString pretokenString
(* type declarations for consistency checking *)
val _ = op pretokenString : pretoken -> string
val _ = op tokenString    : token    -> string
(* lexical analysis for \uscheme\ and related languages S384b *)
local
  (* functions used in all lexers S384d *)
  fun noneIfLineEnds chars =
    case streamGet chars
      of NONE => NONE (* end of line *)
       | SOME (#";", cs) => NONE (* comment *)
       | SOME (c, cs) => 
           let val msg = "invalid initial character in `" ^
                         implode (c::listOfStream cs) ^ "'"
           in  SOME (ERROR msg, EOS)
           end
  (* type declarations for consistency checking *)
  val _ = op noneIfLineEnds : 'a lexer
  (* functions used in the lexer for \uscheme S384c *)
  fun atom "#t" = SHARP true
    | atom "#f" = SHARP false
    | atom x    = NAME x
in
  val schemeToken =
    whitespace *>
    bracketLexer   (  QUOTE   <$  eqx #"'" one
                  <|> INT     <$> intToken isDelim
                  <|> (atom o implode) <$> many1 (sat (not o isDelim) one)
                  <|> noneIfLineEnds
                   )
(* type declarations for consistency checking *)
val _ = op schemeToken : token lexer
val _ = op atom : string -> pretoken
end
(* parsers for single tokens for \uscheme-like languages S385a *)
type 'a parser = (token, 'a) polyparser
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token : pretoken
                                                                          parser
val quote     = (fn (QUOTE)     => SOME () | _ => NONE) <$>? pretoken
val int       = (fn (INT   n)   => SOME n  | _ => NONE) <$>? pretoken
val booltok   = (fn (SHARP b)   => SOME b  | _ => NONE) <$>? pretoken
val namelike  = (fn (NAME  n)   => SOME n  | _ => NONE) <$>? pretoken
val namelike  = asAscii namelike
(* type declarations for consistency checking *)
val _ = op booltok  : bool parser
val _ = op int      : int  parser
val _ = op namelike : name parser
(* parsers for \uscheme\ tokens S385b *)
val reserved = [ "if", "while", "set", "begin", "lambda", "let"
               , "letrec", "let*", "quote", "val", "define", "use"
               , "check-expect", "check-assert", "check-error", "||", "&&"
               ]
val name = rejectReserved reserved <$>! namelike
(* parsers and parser builders for formal parameters and bindings S385c *)
fun formalsOf what name context = 
  nodups ("formal parameter", context) <$>! @@ (bracket (what, many name))

fun bindingsOf what name exp =
  let val binding = bracket (what, pair <$> name <*> exp)
  in  bracket ("(... " ^ what ^ " ...) in bindings", many binding)
  end
(* parsers and parser builders for formal parameters and bindings S386a *)
fun distinctBsIn bindings context =
  let fun check (loc, bs) =
        nodups ("bound name", context) (loc, map fst bs) >>=+ (fn _ => bs)
  in  check <$>! @@ bindings
  end
(* type declarations for consistency checking *)
val _ = op formalsOf  : string -> name parser -> string -> name list parser
val _ = op bindingsOf : string -> 'x parser -> 'e parser -> ('x * 'e) list
                                                                          parser
(* type declarations for consistency checking *)
val _ = op distinctBsIn : (name * 'e) list parser -> string -> (name * 'e) list
                                                                          parser
(* parsers and parser builders for formal parameters and bindings ((higher-order)) S386b *)
fun asLambda inWhat (loc, e as LAMBDA _) = OK e
  | asLambda inWhat (loc, e) = 
      synerrorAt ("in " ^ inWhat ^ ", expression " ^ expString e ^ 
                  " is not a lambda")
                 loc

val asLambda = fn what => fn eparser => asLambda what <$>! @@ eparser
(* type declarations for consistency checking *)
val _ = op asLambda : string -> exp parser -> exp parser
(* parsers and parser builders for formal parameters and bindings S386c *)
fun recordFieldsOf name =
  nodups ("record fields", "record definition") <$>!
                                    @@ (bracket ("(field ...)", many name))
(* type declarations for consistency checking *)
val _ = op recordFieldsOf : name parser -> name list parser
(* parsers and parser builders for formal parameters and bindings S387a *)
fun kw keyword = 
  eqx keyword namelike
fun usageParsers ps = anyParser (map (usageParser kw) ps)
(* type declarations for consistency checking *)
val _ = op kw : string -> string parser
val _ = op usageParsers : (string * 'a parser) list -> 'a parser
(* parsers and parser builders for \scheme-like syntax S386d *)
fun sexp tokens = (
     SYM       <$> (notDot <$>! @@ namelike)
 <|> NUM       <$> int
 <|> embedBool <$> booltok
 <|> leftCurly <!> "curly brackets may not be used in S-expressions"
 <|> embedList <$> bracket ("list of S-expressions", many sexp)
 <|> (fn v => embedList [SYM "quote", v]) 
               <$> (quote *> sexp)
) tokens
and notDot (loc, ".") =
      synerrorAt "this interpreter cannot handle . in quoted S-expressions" loc
  | notDot (_,   s)   = OK s
(* type declarations for consistency checking *)
val _ = op sexp : value parser
(* parsers and parser builders for \scheme-like syntax S386e *)
fun atomicSchemeExpOf name =  VAR                   <$> name
                          <|> LITERAL <$> NUM       <$> int
                          <|> LITERAL <$> embedBool <$> booltok
(* parsers and parser builders for \scheme-like syntax S388a *)
fun fullSchemeExpOf atomic bracketedOf =
  let val exp = fn tokens => fullSchemeExpOf atomic bracketedOf tokens
  in      atomic
      <|> bracketedOf exp
      <|> quote *> (LITERAL <$> sexp)
      <|> quote *> badRight "quote ' followed by right bracket"
      <|> leftCurly <!> "curly brackets are not supported"
      <|> left *> right <!> "(): unquoted empty parentheses"
      <|> bracket("function application", curry APPLY <$> exp <*> many exp)
  end
(* type declarations for consistency checking *)
val _ = op fullSchemeExpOf : exp parser -> (exp parser -> exp parser) -> exp
                                                                          parser
(* parsers and [[xdef]] streams for \uscheme S387b *)
fun exptable exp =
  let val bindings = bindingsOf "(x e)" name exp
      val formals  = formalsOf "(x1 x2 ...)" name "lambda"
      val dbs      = distinctBsIn bindings
      val letrecbs =
        distinctBsIn
            (bindingsOf "[f (lambda (...) ...)]" name (asLambda "letrec" exp))
            "letrec"
      
(* type declarations for consistency checking *)
val _ = op exptable  : exp parser -> exp parser
val _ = op exp       : exp parser
val _ = op bindings  : (name * exp) list parser
  in usageParsers
     [ ("(if e1 e2 e3)",            curry3 IFX    <$> exp <*> exp <*> exp)
     , ("(while e1 e2)",            curry  WHILEX <$> exp  <*> exp)
     , ("(set x e)",                curry  SET    <$> name <*> exp)
     , ("(begin e1 ...)",                  BEGIN  <$> many exp)
     , ("(lambda (names) body)",    curry  LAMBDA <$> formals <*> exp)
     , ("(let (bindings) body)",    curry3 LETX LET     <$> dbs  "let" <*> exp)
     , ("(letrec (bindings) body)", curry3 LETX LETREC  <$> letrecbs   <*> exp)
     , ("(let* (bindings) body)",   curry3 LETX LETSTAR <$> bindings   <*> exp)
     , ("(quote sexp)",             LITERAL             <$> sexp)
      , ("(or e1 e2)",               curry  OR     <$> exp <*> exp)
      , ("(and e1 e2)",              curry  AND    <$> exp <*> exp)

   (* rows added to ML \uscheme's [[exptable]] in exercises ((prototype)) 322 *)
     , ("(cond ([q a] ...))",
        let fun desugarCond qas = raise LeftAsExercise "desugar cond"
            val qa = bracket ("[question answer]", pair <$> exp <*> exp)
     (* type declarations for consistency checking *)
     val _ = op desugarCond : (exp * exp) list -> exp
        in  desugarCond <$> many qa
        end
       )
     (* rows added to ML \uscheme's [[exptable]] in exercises S387c *)
     (* add syntactic sugar here, each row preceded by a comma *)
     ]
  end
(* parsers and [[xdef]] streams for \uscheme S388b *)
val exp = fullSchemeExpOf (atomicSchemeExpOf name) exptable
(* parsers and [[xdef]] streams for \uscheme S388c *)
val deftable = usageParsers
  [ ("(define f (args) body)",
        let val formals  = formalsOf "(x1 x2 ...)" name "define"
        in  curry DEFINE <$> name <*> (pair <$> formals <*> exp)
        end)
  , ("(val x e)", curry VAL <$> name <*> exp)
  ]
(* type declarations for consistency checking *)
val _ = op deftable : def parser
(* parsers and [[xdef]] streams for \uscheme S388d *)
val testtable = usageParsers
  [ ("(check-expect e1 e2)", curry CHECK_EXPECT <$> exp <*> exp)
  , ("(check-assert e)",           CHECK_ASSERT <$> exp)
  , ("(check-error e)",            CHECK_ERROR  <$> exp)
  ]
(* type declarations for consistency checking *)
val _ = op testtable : unit_test parser
(* parsers and [[xdef]] streams for \uscheme S388e *)
val xdeftable = usageParsers
  [ ("(use filename)", USE <$> name)
  (* rows added to \uscheme\ [[xdeftable]] in exercises S388f *)
  (* add syntactic sugar here, each row preceded by a comma *) 
  ]
(* type declarations for consistency checking *)
val _ = op xdeftable : xdef parser
(* parsers and [[xdef]] streams for \uscheme S389a *)
val xdef =  DEF  <$> deftable
        <|> TEST <$> testtable
        <|>          xdeftable
        <|> badRight "unexpected right bracket"
        <|> DEF <$> EXP <$> exp
        <?> "definition"
(* type declarations for consistency checking *)
val _ = op xdef : xdef parser
(* parsers and [[xdef]] streams for \uscheme S389b *)
val xdefstream = 
  interactiveParsedStream (schemeToken, xdef)
(* type declarations for consistency checking *)
val _ = op xdefstream : string * line stream * prompts -> xdef stream
(* shared definitions of [[filexdefs]] and [[stringsxdefs]] S233a *)
fun filexdefs (filename, fd, prompts) =
      xdefstream (filename, filelines fd, prompts)
fun stringsxdefs (name, strings) =
      xdefstream (name, streamOfList strings, noPrompts)
(* type declarations for consistency checking *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream



(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \USCHEME *)
(*                                                               *)
(*****************************************************************)

(* evaluation, testing, and the read-eval-print loop for \uscheme S380b *)
(* definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
(* definitions of [[eval]] and [[evaldef]] for \uscheme 309a *)
fun eval (e, rho) =
  let val go = applyWithLimits id in go end (* OMIT *)
  let fun ev (LITERAL v) = v
        (* more alternatives for [[ev]] for \uscheme 309b *)
        | ev (VAR x) = !(find (x, rho))
        | ev (SET (x, e)) = 
            let val v = ev e
            in  find (x, rho) := v;
                v
            end
        (* more alternatives for [[ev]] for \uscheme 309c *)
        | ev (IFX (e1, e2, e3)) = ev (if projectBool (ev e1) then e2 else e3)
        | ev (WHILEX (guard, body)) = 
            if projectBool (ev guard) then 
              (ev body; ev (WHILEX (guard, body)))
            else
              BOOLV false
        (* more alternatives for [[ev]] for \uscheme 309d *)
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, BOOLV false)
            end
        (* more alternatives for [[ev]] for \uscheme 310a *)
        | ev (LAMBDA (xs, e)) = CLOSURE ((xs, e), rho)
        (* more alternatives for [[ev]] for \uscheme 310b *)
        | ev (e as APPLY (f, args)) = 
               (case ev f
                  of PRIMITIVE prim => prim (e, map ev args)
                   | CLOSURE clo    =>
                       (* apply closure [[clo]] to [[args]] ((mlscheme)) 310c *)
                                       let val ((formals, body), savedrho) = clo
                                           val actuals = map ev args
                                       in  eval (body, bindList (formals, map
                                                         ref actuals, savedrho))
                                           handle BindListLength => 
                                               raise RuntimeError (
                                      "Wrong number of arguments to closure; " ^
                                                                   "expected ("
                                                       ^ spaceSep formals ^ ")")
                                       end
                   | v => raise RuntimeError
                                  ("Applied non-function " ^ valueString v)
               )
        (* more alternatives for [[ev]] for \uscheme 310d *)
        | ev (LETX (LET, bs, body)) =
            let val (names, rightSides) = ListPair.unzip bs
        (* type declarations for consistency checking *)
        val _ = ListPair.unzip : ('a * 'b) list -> 'a list * 'b list
            in  eval (body, bindList (names, map (ref o ev) rightSides, rho))
            end
        (* more alternatives for [[ev]] for \uscheme 310e *)
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((x, e), rho) = bind (x, ref (eval (e, rho)), rho)
            in  eval (body, foldl step rho bs)
            end
        (* more alternatives for [[ev]] for \uscheme 311a *)
        | ev (LETX (LETREC, bs, body)) =
            let val (names, rightSides) = ListPair.unzip bs
                val rho' =
                  bindList (names, map (fn _ => ref (unspecified())) rightSides,
                                                                            rho)
                val updates = map (fn (x, rightSide) => (x, eval (rightSide,
                                                                      rho'))) bs
        (* type declarations for consistency checking *)
        val _ = List.app : ('a -> unit) -> 'a list -> unit
            in  List.app (fn (x, v) => find (x, rho') := v) updates; 
                eval (body, rho')
            end
          | ev (OR (e1, e2)) =
            if projectBool (ev e1) then BOOLV true else ev e2
          | ev (AND (e1, e2)) =
            if projectBool (ev e1) then ev e2 else BOOLV false
(* type declarations for consistency checking *)
val _ = op embedList : value list -> value
(* type declarations for consistency checking *)
val _ = op eval : exp * value ref env -> value
val _ = op ev   : exp                 -> value
  in  ev e
  end
(* definitions of [[eval]] and [[evaldef]] for \uscheme 311b *)
fun withNameBound (x, rho) =
  (find (x, rho); rho)
  handle NotFound _ => bind (x, ref (unspecified ()), rho)
(* type declarations for consistency checking *)
val _ = op withNameBound : name * value ref env -> value ref env
(* definitions of [[eval]] and [[evaldef]] for \uscheme 311c *)
fun evaldef (VAL (x, e), rho) =
      let val rho = withNameBound (x, rho)
          val v   = eval (e, rho)
          val _   = find (x, rho) := v
          val response = case e of LAMBDA _ => x
                                 | _ => valueString v
(* type declarations for consistency checking *)
val _ = op evaldef : def  * value ref env -> value ref env * string
      in  (rho, response)
      end
(* definitions of [[eval]] and [[evaldef]] for \uscheme 312a *)
  | evaldef (DEFINE (f, lambda), rho) =
      let val (xs, e) = lambda
      in  evaldef (VAL (f, LAMBDA lambda), rho)
      end
(* definitions of [[eval]] and [[evaldef]] for \uscheme 312b *)
  | evaldef (EXP e, rho) =        
      let val v   = eval (e, rho)
          val rho = withNameBound ("it", rho)
          val _   = find ("it", rho) := v
      in  (rho, valueString v)
      end
(* definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme S238c *)
type basis = value ref env
fun processDef (d, rho, interactivity) =
  let val (rho', response) = evaldef (d, rho)
      val _ = if echoes interactivity then println response else ()
  in  rho'
  end
fun dump_names basis = app (println o fst) basis  (*OMIT*)
(* shared unit-testing utilities S225a *)
fun failtest strings = 
  (app eprint strings; eprint "\n"; false)
(* type declarations for consistency checking *)
val _ = op failtest : string list -> bool
(* shared unit-testing utilities S225b *)
fun reportTestResultsOf what (npassed, nthings) =
  case (npassed, nthings)
    of (_, 0) => ()  (* no report *)
     | (0, 1) => println ("The only " ^ what ^ " failed.")
     | (1, 1) => println ("The only " ^ what ^ " passed.")
     | (0, 2) => println ("Both " ^ what ^ "s failed.")
     | (1, 2) => println ("One of two " ^ what ^ "s passed.")
     | (2, 2) => println ("Both " ^ what ^ "s passed.")
     | _ => if npassed = nthings then
              app print ["All ", intString nthings, " " ^ what ^ "s passed.\n"]
            else if npassed = 0 then
              app print ["All ", intString nthings, " " ^ what ^ "s failed.\n"]
            else
              app print [intString npassed, " of ", intString nthings,
                          " " ^ what ^ "s passed.\n"]
val reportTestResults = reportTestResultsOf "test"
(* shared definition of [[withHandlers]] S239a *)
fun withHandlers f a caught =
  f a
  handle RuntimeError msg   => caught ("Run-time error <at loc>: " ^ msg)
       | NotFound x         => caught ("Name " ^ x ^ " not found <at loc>")
       | Located (loc, exn) =>
           withHandlers (fn _ => raise exn)
                        a
                        (fn s => caught (fillAtLoc (s, loc)))

(* other handlers that catch non-fatal exceptions and pass messages to [[caught]] S239b *)
       | Div                => caught ("Division by zero <at loc>")
       | Overflow           => caught ("Arithmetic overflow <at loc>")
       | Subscript          => caught ("Array index out of bounds <at loc>")
       | Size               => caught (
                                "Array length too large (or negative) <at loc>")
       | IO.Io { name, ...} => caught ("I/O error <at loc>: " ^ name)
(* definition of [[testIsGood]] for \uscheme S382c *)
fun testIsGood (test, rho) =
  let fun outcome e =
        withHandlers (fn e => OK (eval (e, rho))) e (ERROR o stripAtLoc)

   (* [[asSyntacticValue]] for \uscheme, \timpcore, \tuscheme, and \nml S383a *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue _           = NONE
      (* type declarations for consistency checking *)
      val _ = op asSyntacticValue : exp -> value option

 (* shared [[check{Expect,Assert,Error}Passes]], which call [[outcome]] S224d *)
      (* shared [[whatWasExpected]] S223c *)
      fun whatWasExpected (e, outcome) =
        case asSyntacticValue e
          of SOME v => valueString v
           | NONE =>
               case outcome
                 of OK v => valueString v ^ " (from evaluating " ^ expString e ^
                                                                             ")"
                  | ERROR _ =>  "the result of evaluating " ^ expString e
      (* type declarations for consistency checking *)
      val _ = op whatWasExpected  : exp * value error -> string
      val _ = op asSyntacticValue : exp -> value option
      (* shared [[checkExpectPassesWith]], which calls [[outcome]] S224a *)
      val cxfailed = "check-expect failed: "
      fun checkExpectPassesWith equals (checkx, expectx) =
        case (outcome checkx, outcome expectx)
          of (OK check, OK expect) => 
               equals (check, expect) orelse
               failtest [cxfailed, " expected ", expString checkx,
                         " to evaluate to ", whatWasExpected (expectx, OK expect
                                                                              ),
                         ", but it's ", valueString check, "."]
           | (ERROR msg, tried) =>
               failtest [cxfailed, " expected ", expString checkx,
                         " to evaluate to ", whatWasExpected (expectx, tried),
                         ", but evaluating ", expString checkx,
                         " caused this error: ", msg]
           | (_, ERROR msg) =>
               failtest [cxfailed, " expected ", expString checkx,
                         " to evaluate to ", whatWasExpected (expectx, ERROR msg
                                                                              ),
                         ", but evaluating ", expString expectx,
                         " caused this error: ", msg]
      (* type declarations for consistency checking *)
      val _ = op checkExpectPassesWith : (value * value -> bool) -> exp * exp ->
                                                                            bool
      val _ = op outcome  : exp -> value error
      val _ = op failtest : string list -> bool

(* shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]] S224b *)
      val cafailed = "check-assert failed: "
      fun checkAssertPasses checkx =
            case outcome checkx
              of OK check =>
                   projectBool check orelse
                   failtest [cafailed, " expected assertion ", expString checkx,
                             " to hold, but it doesn't"]
               | ERROR msg =>
                   failtest [cafailed, " expected assertion ", expString checkx,
                             " to hold, but evaluating it caused this error: ",
                                                                            msg]
      (* type declarations for consistency checking *)
      val _ = op checkAssertPasses : exp -> bool

(* shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]] S224c *)
      val cefailed = "check-error failed: "
      fun checkErrorPasses checkx =
            case outcome checkx
              of ERROR _ => true
               | OK check =>
                   failtest [cefailed, " expected evaluating ", expString checkx
                                                                               ,
                             " to cause an error, but evaluation produced ",
                             valueString check]
      (* type declarations for consistency checking *)
      val _ = op checkErrorPasses : exp -> bool
      fun checkExpectPasses (cx, ex) = checkExpectPassesWith testEquals (cx, ex)
      fun passes (CHECK_EXPECT (c, e)) = checkExpectPasses (c, e)
        | passes (CHECK_ASSERT c)      = checkAssertPasses c
        | passes (CHECK_ERROR c)       = checkErrorPasses  c
(* type declarations for consistency checking *)
val _ = op testIsGood : unit_test * basis -> bool
val _ = op outcome    : exp -> value error
  in  passes test
  end
(* shared definition of [[processTests]] S226 *)
fun processTests (tests, rho) =
      reportTestResults (numberOfGoodTests (tests, rho), length tests)
and numberOfGoodTests (tests, rho) =
      let val testIsGood = fn args => (resetComputationLimits (); testIsGood
                                                               args) in (*OMIT*)
      foldr (fn (t, n) => if testIsGood (t, rho) then n + 1 else n) 0 tests
      end
                                                                                
                                                                        (*OMIT*)
(* type declarations for consistency checking *)
val _ = op processTests : unit_test list * basis -> unit
(* shared read-eval-print loop S237 *)
fun readEvalPrintWith errmsg (xdefs, basis, interactivity) =
  let val unitTests = ref []

(* definition of [[processXDef]], which can modify [[unitTests]] and call [[errmsg]] S238b *)
      fun processXDef (xd, basis) =
        let (* definition of [[useFile]], to read from a file S238a *)
            fun useFile filename =
              let val fd = TextIO.openIn filename
                  val (_, printing) = interactivity
                  val inter' = (NOT_PROMPTING, printing)
              in  readEvalPrintWith errmsg (filexdefs (filename, fd, noPrompts),
                                                                  basis, inter')
                  before TextIO.closeIn fd
              end
            fun try (USE filename) = useFile filename
              | try (TEST t)       = (unitTests := t :: !unitTests; basis)
              | try (DEF def)      = processDef (def, basis, interactivity)
              | try (DEFS ds)      = foldl processXDef basis (map DEF ds)
                                                                        (*OMIT*)
            fun caught msg = (errmsg (stripAtLoc msg); basis)
            val () = resetComputationLimits ()     (* OMIT *)
        in  withHandlers try xd caught
        end 
      (* type declarations for consistency checking *)
      val _ = op errmsg     : string -> unit
      val _ = op processDef : def * basis * interactivity -> basis
      val basis = streamFold processXDef basis xdefs
      val _     = processTests (!unitTests, basis)
(* type declarations for consistency checking *)
type basis = basis
val _ = op processDef   : def * basis * interactivity -> basis
val _ = op testIsGood   : unit_test      * basis -> bool
val _ = op processTests : unit_test list * basis -> unit
(* type declarations for consistency checking *)
val _ = op readEvalPrintWith : (string -> unit) ->                     xdef
                                         stream * basis * interactivity -> basis
val _ = op processXDef       : xdef * basis -> basis
  in  basis
  end



(*****************************************************************)
(*                                                               *)
(*   IMPLEMENTATIONS OF \USCHEME\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* implementations of \uscheme\ primitives and definition of [[initialBasis]] S382a *)
(* utility functions for building primitives in \uscheme 313a *)
fun inExp f = 
  fn (e, vs) => f vs
                handle RuntimeError msg =>
                  raise RuntimeError ("in " ^ expString e ^ ", " ^ msg)
(* type declarations for consistency checking *)
val _ = op inExp : (value list -> value) -> (exp * value list -> value)
(* utility functions for building primitives in \uscheme 313b *)
fun arityError n args =
  raise RuntimeError ("expected " ^ intString n ^
                      " but got " ^ intString (length args) ^ " arguments")
fun unaryOp  f = (fn [a]    => f a      | args => arityError 1 args)
fun binaryOp f = (fn [a, b] => f (a, b) | args => arityError 2 args)
(* type declarations for consistency checking *)
val _ = op unaryOp  : (value         -> value) -> (value list -> value)
val _ = op binaryOp : (value * value -> value) -> (value list -> value)
(* utility functions for building primitives in \uscheme 313c *)
fun arithOp f = binaryOp (fn (NUM n1, NUM n2) => NUM (f (n1, n2)) 
                           | (NUM n, v) =>
                                       (* report [[v]] is not an integer 314b *)
                                           raise RuntimeError (
                                "expected an integer, but got " ^ valueString v)
                           | (v, _)     =>
                                       (* report [[v]] is not an integer 314b *)
                                           raise RuntimeError (
                                "expected an integer, but got " ^ valueString v)
                         )
(* type declarations for consistency checking *)
val _ = op arithOp: (int * int -> int) -> (value list -> value)
(* utility functions for building primitives in \uscheme 314a *)
fun predOp f     = unaryOp  (embedBool o f)
fun comparison f = binaryOp (embedBool o f)
fun intcompare f = comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                                | (NUM n, v) =>
                                       (* report [[v]] is not an integer 314b *)
                                                raise RuntimeError (
                                "expected an integer, but got " ^ valueString v)
                                | (v, _)     =>
                                       (* report [[v]] is not an integer 314b *)
                                                raise RuntimeError (
                                "expected an integer, but got " ^ valueString v)
                              )
(* type declarations for consistency checking *)
val _ = op predOp     : (value         -> bool) -> (value list -> value)
val _ = op comparison : (value * value -> bool) -> (value list -> value)
val _ = op intcompare : (int   * int   -> bool) -> (value list -> value)
(* utility functions for building primitives in \uscheme S381d *)
fun errorPrimitive (_, [v]) = raise RuntimeError (valueString v)
  | errorPrimitive (e, vs)  = inExp (arityError 1) (e, vs)
(* type declarations for consistency checking *)
val _ = op errorPrimitive : exp * value list -> value list
val primitiveBasis =
  let val rho =
        foldl (fn ((name, prim), rho) =>
                 bind (name, ref (PRIMITIVE (inExp prim)), rho))
              emptyEnv
              ((* primitives for \uscheme\ [[::]] 313d *)
               ("+", arithOp op +  ) :: 
               ("-", arithOp op -  ) :: 
               ("*", arithOp op *  ) :: 
               ("/", arithOp op div) ::
               (* primitives for \uscheme\ [[::]] 314c *)
               ("<", intcompare op <) :: 
               (">", intcompare op >) ::
               ("=", comparison equalatoms) ::
               ("null?",    predOp (fn (NIL    ) => true | _ => false)) ::
               ("boolean?", predOp (fn (BOOLV _) => true | _ => false)) ::
               (* primitives for \uscheme\ [[::]] S380f *)
               ("number?",  predOp (fn (NUM   _) => true | _ => false)) ::
               ("symbol?",  predOp (fn (SYM   _) => true | _ => false)) ::
               ("pair?",    predOp (fn (PAIR  _) => true | _ => false)) ::
               ("function?",
                     predOp (fn (PRIMITIVE _) => true
                              | (CLOSURE   _) => true
                              | _ => false)) ::
               (* primitives for \uscheme\ [[::]] S381a *)
               ("cons", binaryOp (fn (a, b) => PAIR (a, b))) ::
               ("car",  unaryOp  (fn (PAIR (car, _)) => car 
                                   | NIL => raise RuntimeError
                                                     "car applied to empty list"
                                   | v => raise RuntimeError
                                            ("car applied to non-list " ^
                                                             valueString v))) ::
               ("cdr",  unaryOp  (fn (PAIR (_, cdr)) => cdr 
                                   | NIL => raise RuntimeError
                                                     "cdr applied to empty list"
                                   | v => raise RuntimeError
                                            ("cdr applied to non-list " ^
                                                             valueString v))) ::
               (* primitives for \uscheme\ [[::]] S381b *)
               ("hash",  unaryOp (fn SYM s => NUM (fnvHash s)
                                   | v => raise RuntimeError
                                             (valueString v ^ " is not a symbol"
                                                                          ))) ::
               (* primitives for \uscheme\ [[::]] S381c *)
               ("println", unaryOp (fn v => (print (valueString v ^ "\n"); v)))
                                                                              ::
               ("print",   unaryOp (fn v => (print (valueString v);        v)))
                                                                              ::
               ("printu",  unaryOp (fn NUM n => (printUTF8 n; NUM n)
                                     | v => raise RuntimeError
                                              (valueString v ^
                                               " is not a Unicode code point")))
                                                                          :: [])
      val rho = bind ("error", ref (PRIMITIVE errorPrimitive), rho)
  in  rho
  end
(* implementations of \uscheme\ primitives and definition of [[initialBasis]] S382b *)
val predefs = 
               [ ";  predefined uScheme functions 96a "
               , "(define caar (xs) (car (car xs)))"
               , "(define cadr (xs) (car (cdr xs)))"
               , "(define cdar (xs) (cdr (car xs)))"
               ,
";  predefined uScheme functions ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) "
               , ";  more predefined combinations of [[car]] and [[cdr]] S319e "
               , "(define cddr  (sx) (cdr (cdr  sx)))"
               , "(define caaar (sx) (car (caar sx)))"
               , "(define caadr (sx) (car (cadr sx)))"
               , "(define cadar (sx) (car (cdar sx)))"
               , "(define caddr (sx) (car (cddr sx)))"
               , "(define cdaar (sx) (cdr (caar sx)))"
               , "(define cdadr (sx) (cdr (cadr sx)))"
               , "(define cddar (sx) (cdr (cdar sx)))"
               , "(define cdddr (sx) (cdr (cddr sx)))"
               , ";  more predefined combinations of [[car]] and [[cdr]] S319f "
               , "(define caaaar (sx) (car (caaar sx)))"
               , "(define caaadr (sx) (car (caadr sx)))"
               , "(define caadar (sx) (car (cadar sx)))"
               , "(define caaddr (sx) (car (caddr sx)))"
               , "(define cadaar (sx) (car (cdaar sx)))"
               , "(define cadadr (sx) (car (cdadr sx)))"
               , "(define caddar (sx) (car (cddar sx)))"
               , "(define cadddr (sx) (car (cdddr sx)))"
               , ";  more predefined combinations of [[car]] and [[cdr]] S320a "
               , "(define cdaaar (sx) (cdr (caaar sx)))"
               , "(define cdaadr (sx) (cdr (caadr sx)))"
               , "(define cdadar (sx) (cdr (cadar sx)))"
               , "(define cdaddr (sx) (cdr (caddr sx)))"
               , "(define cddaar (sx) (cdr (cdaar sx)))"
               , "(define cddadr (sx) (cdr (cdadr sx)))"
               , "(define cdddar (sx) (cdr (cddar sx)))"
               , "(define cddddr (sx) (cdr (cdddr sx)))"
               , ";  predefined uScheme functions 96b "
               , "(define list1 (x)     (cons x '()))"
               , "(define list2 (x y)   (cons x (list1 y)))"
               , "(define list3 (x y z) (cons x (list2 y z)))"
               , ";  predefined uScheme functions 99b "
               , "(define append (xs ys)"
               , "  (if (null? xs)"
               , "     ys"
               , "     (cons (car xs) (append (cdr xs) ys))))"
               , ";  predefined uScheme functions 100b "
               , "(define revapp (xs ys) ; (reverse xs) followed by ys"
               , "  (if (null? xs)"
               , "     ys"
               , "     (revapp (cdr xs) (cons (car xs) ys))))"
               , ";  predefined uScheme functions 100c "
               , "(define reverse (xs) (revapp xs '()))"
               ,
";  predefined uScheme functions ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) "
               ,
";  definitions of predefined uScheme functions [[and]], [[or]], and [[not]] 96c "
               , "(define and (b c) (if b  c  b))"
               , "(define or  (b c) (if b  b  c))"
               , "(define not (b)   (if b #f #t))"
               , ";  predefined uScheme functions 103a "
               , "(define atom? (x)"
               ,
              "  (or (symbol? x) (or (number? x) (or (boolean? x) (null? x)))))"
               , ";  predefined uScheme functions 104a "
               , "(define equal? (sx1 sx2)"
               , "  (if (atom? sx1)"
               , "    (= sx1 sx2)"
               , "    (if (atom? sx2)"
               , "        #f"
               , "        (and (equal? (car sx1) (car sx2))"
               , "             (equal? (cdr sx1) (cdr sx2))))))"
               , ";  predefined uScheme functions 106a "
               , "(define make-alist-pair      (k a)   (list2 k a))"
               , "(define alist-pair-key       (pair)  (car  pair))"
               , "(define alist-pair-attribute (pair)  (cadr pair))"
               , ";  predefined uScheme functions 106b "
               ,
     "(define alist-first-key       (alist) (alist-pair-key       (car alist)))"
               ,
     "(define alist-first-attribute (alist) (alist-pair-attribute (car alist)))"
               , ";  predefined uScheme functions 106c "
               , "(define bind (k a alist)"
               , "  (if (null? alist)"
               , "    (list1 (make-alist-pair k a))"
               , "    (if (equal? k (alist-first-key alist))"
               , "      (cons (make-alist-pair k a) (cdr alist))"
               , "      (cons (car alist) (bind k a (cdr alist))))))"
               , "(define find (k alist)"
               , "  (if (null? alist)"
               , "    '()"
               , "    (if (equal? k (alist-first-key alist))"
               , "      (alist-first-attribute alist)"
               , "      (find k (cdr alist)))))"
               , ";  predefined uScheme functions 125a "
               ,
    "(define o (f g) (lambda (x) (f (g x))))          ; ((o f g) x) = (f (g x))"
               , ";  predefined uScheme functions 126c "
               , "(define curry   (f) (lambda (x) (lambda (y) (f x y))))"
               , "(define uncurry (f) (lambda (x y) ((f x) y)))"
               , ";  predefined uScheme functions 129 "
               , "(define filter (p? xs)"
               , "  (if (null? xs)"
               , "    '()"
               , "    (if (p? (car xs))"
               , "      (cons (car xs) (filter p? (cdr xs)))"
               , "      (filter p? (cdr xs)))))"
               , ";  predefined uScheme functions 130a "
               , "(define map (f xs)"
               , "  (if (null? xs)"
               , "    '()"
               , "    (cons (f (car xs)) (map f (cdr xs)))))"
               , ";  predefined uScheme functions 130b "
               , "(define app (f xs)"
               , "  (if (null? xs)"
               , "    #f"
               , "    (begin (f (car xs)) (app f (cdr xs)))))"
               , ";  predefined uScheme functions 130c "
               , "(define exists? (p? xs)"
               , "  (if (null? xs)"
               , "    #f"
               , "    (if (p? (car xs)) "
               , "      #t"
               , "      (exists? p? (cdr xs)))))"
               , "(define all? (p? xs)"
               , "  (if (null? xs)"
               , "    #t"
               , "    (if (p? (car xs))"
               , "      (all? p? (cdr xs))"
               , "      #f)))"
               , ";  predefined uScheme functions 131b "
               , "(define foldr (combine zero xs)"
               , "  (if (null? xs)"
               , "    zero"
               , "    (combine (car xs) (foldr combine zero (cdr xs)))))"
               , "(define foldl (combine zero xs)"
               , "  (if (null? xs)"
               , "    zero"
               , "    (foldl combine (combine (car xs) zero) (cdr xs))))"
               , ";  predefined uScheme functions S319a "
               , "(val newline      10)   (val left-round    40)"
               , "(val space        32)   (val right-round   41)"
               , "(val semicolon    59)   (val left-curly   123)"
               , "(val quotemark    39)   (val right-curly  125)"
               , "                        (val left-square   91)"
               , "                        (val right-square  93)"
               , ";  predefined uScheme functions S319b "
               , "(define <= (x y) (not (> x y)))"
               , "(define >= (x y) (not (< x y)))"
               , "(define != (x y) (not (= x y)))"
               , ";  predefined uScheme functions S319c "
               , "(define max (x y) (if (> x y) x y))"
               , "(define min (x y) (if (< x y) x y))"
               , ";  predefined uScheme functions S319d "
               , "(define negated (n) (- 0 n))"
               , "(define mod (m n) (- m (* n (/ m n))))"
               , "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"
               , "(define lcm (m n) (if (= m 0) 0 (* m (/ n (gcd m n)))))"
               , ";  predefined uScheme functions S320b "
               , "(define list4 (x y z a)         (cons x (list3 y z a)))"
               , "(define list5 (x y z a b)       (cons x (list4 y z a b)))"
               , "(define list6 (x y z a b c)     (cons x (list5 y z a b c)))"
               , "(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))"
               ,
               "(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))"
                ]
val initial_env = mkEnv (["true", "false"], [ref (BOOLV true), ref (BOOLV false)])
val initialBasis =
  let val xdefs = stringsxdefs ("predefined functions", predefs)
      val basis_with_predefs = readEvalPrintWith predefinedFunctionError
                                                  (xdefs, primitiveBasis, noninteractive)
  in  basis_with_predefs <+> initial_env
  end
(* type declarations for consistency checking *)
val _ = op primitiveBasis : basis
val _ = op initialBasis : basis


(*****************************************************************)
(*                                                               *)
(*   FUNCTION [[RUNSTREAM]], WHICH EVALUATES INPUT GIVEN [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* function [[runStream]], which evaluates input given [[initialBasis]] S240b *)
fun runStream inputName input interactivity basis = 
  let val _ = setup_error_format interactivity
      val prompts = if prompts interactivity then stdPrompts else noPrompts
      val xdefs = filexdefs (inputName, input, prompts)
  in  readEvalPrintWith eprintln (xdefs, basis, interactivity)
  end 
(* type declarations for consistency checking *)
val _ = op withHandlers : ('a -> 'b) -> 'a -> (string -> 'b) -> 'b
(* type declarations for consistency checking *)
val _ = op runStream : string -> TextIO.instream -> interactivity -> basis ->
                                                                           basis


(*****************************************************************)
(*                                                               *)
(*   LOOK AT COMMAND-LINE ARGUMENTS, THEN RUN                    *)
(*                                                               *)
(*****************************************************************)

(* look at command-line arguments, then run S240c *)
fun runPathWith interactivity ("-", basis) =
      runStream "standard input" TextIO.stdIn interactivity basis
  | runPathWith interactivity (path, basis) =
      let val fd = TextIO.openIn path
      in  runStream path fd interactivity basis
          before TextIO.closeIn fd
      end 
(* type declarations for consistency checking *)
val _ = op runPathWith : interactivity -> (string * basis -> basis)
(* look at command-line arguments, then run S240d *)
val usage = ref (fn () => ())
(* type declarations for consistency checking *)
val _ = op usage : (unit -> unit) ref
(* look at command-line arguments, then run S241a *)
datatype action
  = RUN_WITH of interactivity  (* call runPathWith on remaining arguments *)
  | DUMP     of unit -> unit   (* dump information *)
  | FAIL     of string         (* signal a bad command line *)
  | DEFAULT                    (* no command-line options were given *)
(* look at command-line arguments, then run S241b *)
val default_action = RUN_WITH (PROMPTING, ECHOING)
(* look at command-line arguments, then run S241c *)
fun perform (RUN_WITH interactivity, []) =
      perform (RUN_WITH interactivity, ["-"])
  | perform (RUN_WITH interactivity, args) =
      ignore (foldl (runPathWith interactivity) initialBasis args)
  | perform (DUMP go, [])     = go ()
  | perform (DUMP go, _ :: _) = perform (FAIL "Dump options take no files", [])
  | perform (FAIL msg, _)     = (eprintln msg; !usage())
  | perform (DEFAULT, args)   = perform (default_action, args)
(* type declarations for consistency checking *)
val _ = op perform: action * string list -> unit
(* look at command-line arguments, then run S241d *)
fun merge (_, a as FAIL _) = a
  | merge (a as FAIL _, _) = a
  | merge (DEFAULT, a) = a
  | merge (_, DEFAULT) = raise InternalError "DEFAULT on the right in MERGE"
  | merge (RUN_WITH _, right as RUN_WITH _) = right
  | merge (DUMP f, DUMP g) = DUMP (g o f)
  | merge (_, r) = FAIL "Interpret or dump, but don't try to do both"
(* type declarations for consistency checking *)
val _ = op merge: action * action -> action
(* look at command-line arguments, then run S241e *)
val actions =
  [ ("",    RUN_WITH (PROMPTING,     ECHOING))
  , ("-q",  RUN_WITH (NOT_PROMPTING, ECHOING))
  , ("-qq", RUN_WITH (NOT_PROMPTING, NOT_ECHOING))
  , ("-names",      DUMP (fn () => dump_names initialBasis))
  , ("-primitives", DUMP (fn () => dump_names primitiveBasis))
  , ("-help",       DUMP (fn () => !usage ()))
  ]
                                                          (*OMIT*)
val unusedActions =  (* reveals answers to homeworks *)   (*OMIT*)
  [ ("-predef",     DUMP (fn () => app println predefs))  (*OMIT*)
  ]                                                       (*OMIT*)
(* type declarations for consistency checking *)
val _ = op actions : (string * action) list
(* look at command-line arguments, then run S242a *)
val _ = usage := (fn () =>
  ( app eprint ["Usage:\n"]
  ; app (fn (option, action) =>
         app eprint ["       ", CommandLine.name (), " ", option, "\n"]) actions
  ))
(* look at command-line arguments, then run S242b *)
fun action option =
  case List.find (curry op = option o fst) actions
    of SOME (_, action) => action
     | NONE => FAIL ("Unknown option " ^ option)
(* type declarations for consistency checking *)
val _ = op action : string -> action
(* look at command-line arguments, then run S242c *)
fun strip_options a [] = (a, [])
  | strip_options a (arg :: args) =
      if String.isPrefix "-" arg andalso arg <> "-" then
          strip_options (merge (a, action arg)) args
      else
          (a, arg :: args)

val _ = if hasOption "NORUN" then ()
        else perform (strip_options DEFAULT (CommandLine.arguments ()))
(* type declarations for consistency checking *)
val _ = op strip_options : action -> string list -> action * string list