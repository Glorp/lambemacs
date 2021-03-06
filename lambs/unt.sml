signature REPL =
sig
val repl : unit -> unit
end

structure repl :> REPL =
struct

fun curry f a b = f (a, b)
fun uncurry f (a, b) = f a b
fun flip f a b = f b a
fun apply f a = f a

fun omap opt f = case opt of
                     NONE   => NONE
                   | SOME x => SOME (f x)
fun obnd opt f = case opt of
                     NONE   => NONE
                   | SOME x => f x
fun oor opt f = case opt of
                    NONE => f ()
                  | x    => x

datatype term = Lam of string * term
              | App of term * term
              | Var of string
 
datatype def = Define of string * term
 
datatype stmt = RenameDefs of term
              | Eval of (term * int)
              | Def of def
              | Undef of string
              | ShowDefs
 
datatype exec = Reduction of (term * term)
              | Rename of (string * term * string * term)
              | Normal of term

fun pstring s = "(" ^ s ^")"
                                     
fun termstr (Lam (p, b))          = "#l" ^ p ^ "." ^ termstr b
  | termstr (App (Lam (p, b), a)) = pstring (termstr (Lam (p, b))) ^ " " ^ argstring a
  | termstr (App (f, a))          = termstr f ^ " " ^ argstring a
  | termstr (Var s)               = s
and argstring (Var s) = s
  | argstring t       = pstring (termstr t)

val lam = curry Lam
val app = curry App
 
fun findTerm pred t =
    let fun findBin ctor t1 t2 res = oor (find t1 (flip ctor t2 :: res))
                                         (fn _ => find t2 (ctor t1  :: res))
        and find t res      =
            case (pred t) of
                SOME x => SOME (x, res)
              | NONE   => (case t of
                               Lam (p, b) => find b (lam p :: res)
                             | App (f, a) => findBin app f a res
                             | Var _      => NONE)
    in find t []
    end
 
fun fterm t tz = foldl (uncurry apply) t tz
 
fun reducible (App (Lam (p, b), a)) = SOME ((p, b, a), fn (p, b, a) => (App (Lam (p, b), a)))
  | reducible _                     = NONE
 
 
fun subst t s (App (f, a)) = App (subst t s f, subst t s a)
  | subst t s (Lam (p, b)) = if p = s then Lam (p, b)
                             else Lam (p, subst t s b)
  | subst t s (Var v)      = if v = s then t
                             else Var v
 
val renameDefs =
    let fun renameDef ((Define (ds, dt)), trm) = subst dt ds trm
    in foldr renameDef
    end

structure SSet = ListSetFn (struct
                            type ord_key = string
                            val compare = String.compare
                            end)
 
fun freeIds (Var s) env      = if SSet.member (env, s) then SSet.empty
                                  else SSet.singleton s
  | freeIds (App (f, a)) env = SSet.union (freeIds f env, freeIds a env)
  | freeIds (Lam (p, b)) env = freeIds b (SSet.add (env, p))
 
fun allIds (Var s)      = SSet.singleton s
  | allIds (App (f, a)) = SSet.union (allIds f, allIds a)
  | allIds (Lam (p, b)) = SSet.add (allIds b, p)
 
fun uniqueId t s =
    let val ids = allIds t
        fun uniq n = let val newId = s ^ Int.toString n
                     in if SSet.member (ids, newId) then uniq (n + 1)
                        else newId
                     end
    in uniq 2
    end
 
fun conflict param body arg =
    let val ids = (freeIds arg SSet.empty)
        fun conf t poss res =
            let fun binderConf p b f =
                if param = p
                then NONE
                else if SSet.member (ids, p)
                     then conf b (SOME (p, b, f, res)) (curry f p :: res)
                     else conf b poss (curry f p :: res)
                fun binConf t1 t2 f = oor (conf t1 poss (flip app t2 :: res))
                                          (fn _ => conf t2 poss (app t1 :: res))
            in case t of
                   Lam (p, b) => binderConf p b (fn (p, b) => Lam (p, b))
                 | App (f, a) => binConf f a app
                 | Var s      => if s = param
                                 then poss
                                 else NONE
            end
    in if SSet.isEmpty ids then NONE
       else conf body NONE []
    end
 
fun reduceRename t =
    let fun rename p b f tz = let val s = uniqueId t p
                              in Rename (p, t, s, fterm (f (s, (subst (Var s) p b))) tz)
                              end
        fun reduceRename0 (((rp, rb, ra), rfun), rtz) =
        case conflict rp rb ra of
            NONE                     => Reduction (t, fterm (subst ra rp rb) rtz)
          | SOME (cp, cb, cfun, ctz) => rename cp cb cfun (List.concat [ctz, [fn b => rfun (rp, b, ra)], rtz])
    in case findTerm reducible t of
           SOME red => reduceRename0 red
         | NONE     => Normal t
    end

datatype ctxi = Type of string | Annot of string * term

fun lookup s []                   = NONE
  | lookup s (Type n :: xs)       = if s = n then SOME (Type n)
                                    else lookup s xs
  | lookup s (Annot (n, t) :: xs) = if s = n then SOME (Annot (n, t))
                                    else lookup s xs

fun substVery param body arg t =
    case conflict param body arg of
        NONE                     => subst arg param body
      | SOME (cp, cb, cfun, ctz) =>
        let val s = uniqueId t cp
        in substVery param (fterm (cfun (s, (subst (Var s) cp cb))) ctz) arg t
        end
                                  
fun seperator c = c = #"."
                  orelse c = #"("
                  orelse c = #")"
                  orelse c = #"#"
                  orelse Char.isSpace c

fun readChar c (s, p, m) = if m > p andalso String.sub (s, p) = c then SOME (s, p + 1, m)
                           else NONE

fun readWhites (s, p, m) = 
    if m > p andalso Char.isSpace (String.sub (s, p)) then readWhites (s, p + 1, m)
    else (s, p, m)

fun atEnd str =
    let val (s, p, m) = readWhites str
    in m <= p
    end

fun readWord str =
    let val (s, p, m) = readWhites str
        fun read p = if atEnd (s, p, m) orelse seperator (String.sub (s, p)) then p
                     else read (p + 1)
        val pos = read p
    in if pos = p then NONE
       else SOME (String.substring (s, p, pos - p), (s, pos, m))
    end

fun readVar str = omap (readWord str) (fn (s, rest) => (Var s, rest))

fun listToApp []                = NONE
  | listToApp [x]            = SOME x
  | listToApp [x, y]         = SOME (App (x, y))
  | listToApp (x :: y :: xs) = listToApp ((App (x, y)) :: xs)

fun splitAt c (s, p, m) =
    let fun findStop pos l =
        if atEnd (s, pos, m) then NONE
        else if String.sub (s, pos) = c andalso l = 0 then SOME pos
        else if String.sub (s, pos) = #"(" then findStop (pos + 1) (l + 1)
        else if String.sub (s, pos) = #")" then (if l = 0 then NONE
                             else findStop (pos + 1) (l - 1))
        else findStop (pos + 1) l
    in omap (findStop p 0) (fn stop => ((s, p, stop), (s, stop + 1, m)))
    end

fun strMatch str (s, p, m) = m > p + (String.size str)
                 andalso String.substring (s, p, String.size str) = str

fun isLam str = strMatch "#l" str

fun readTerm str =
    obnd (readList str)
         (fn l => listToApp l)
and readList str =
    let val (s, p, m) = readWhites str
        fun readRest (t, rest) = omap (readList rest) (fn l => t :: l)
    in if atEnd (s, p, m) then SOME []
    else if isLam (s, p, m) then (omap (readLam (s, p + 2, m)) (fn t => [t]))
    else if String.sub (s, p) = #"(" then obnd (readParen (s, p + 1, m)) readRest
    else obnd (readVar (s, p, m)) readRest
    end
and readLam str =
    let fun readT (p, str) = omap (readTerm str)
                                  (fn t => Lam (p, t))
        val param = obnd (readWord str)
                         (fn (p, str) => omap (readChar #"." str)
                                              (fn str => (p, str)))
    in obnd param readT
    end
and readParen (s, p, m) =
    let fun findStop pos l =
        if atEnd (s, pos, m) then NONE
        else if String.sub (s, pos) = #"(" then findStop (pos + 1) (l + 1)
        else if String.sub (s, pos) = #")" then (if l = 0 then SOME pos
                                                 else findStop (pos + 1) (l - 1))
        else findStop (pos + 1) l
    in obnd (findStop p 0)
            (fn stop => omap (readTerm (s, p, stop))
                             (fn t => (t, (s, stop + 1, m))))

    end


fun isDef str =
    let val w = obnd (readWord str)
             (fn (name, rest) => readWord rest)
    in case w of
       SOME (":=", _) => true
     | _              => false
    end

fun isSadface str =
    let val (s, p, m) = readWhites str
    in atEnd (s, p, m)
       orelse (m > p + 2
           andalso String.substring (s, p, 2) = ":("
           andalso atEnd (s, p + 2, m))
    end

fun readDef str = 
    let val name = obnd (readWord str)
            (fn (name, rest) => omap (readWord rest)
                         (fn (_, rest) => (name, rest)))
    val firstW = omap name (fn (_, rest) => isSadface rest)
    in case (name, firstW) of
       (SOME (n, _), SOME true) => SOME (Undef n)
                           
     | (SOME (n, rest), _)           => omap (readTerm rest)
                               (fn t => Def (Define (n, t)))
     | _                             => NONE
    end

fun readStmt s =
    let val ren = "#rename "
        val ev1000 = "#eval1000 "
        val str = (s, 0, size s)
    in if String.isPrefix "#defs" s
       then SOME ShowDefs

       else if String.isPrefix ren s
       then omap (readTerm (s, size ren, size s)) RenameDefs

       else if String.isPrefix ev1000 s
       then omap (readTerm (s, size ev1000, size s)) (fn t => Eval (t, 1000))

       else if isDef str
       then readDef (s, 0, size s)

       else omap (readTerm str) (fn t => Eval (t, 1))
    end   

fun addDef def [] = [def]
  | addDef (Define (n1, t1))  (Define (n2, t2) :: xs) =
    if n1 = n2 then Define (n1, t1) :: xs
    else Define (n2, t2) :: addDef (Define (n1, t1)) xs

fun removeDef _ [] = []
  | removeDef s (Define (n, t) :: xs) =
    if s = n then xs
    else Define (n, t) :: removeDef s xs



fun nextExec (Reduction (_, t))    = SOME t
  | nextExec (Rename (_, _, _, t)) = SOME t
  | nextExec (Normal _)            = NONE

fun execStr (Reduction (_, t))      = "\n" ^ termstr t
  | execStr (Rename (s1, _, s2, t)) = "\n" ^ termstr t ^ " | [" ^ s2 ^ "/" ^ s1 ^ "]"
  | execStr (Normal t)              = "\n" ^ termstr t

fun stmtstr _ (Def (Define (s, _))) = "\n" ^ s ^ " :)"
  | stmtstr _ (Undef s)             = "\n" ^ s ^ " :("
  | stmtstr _ _                     = ""  

fun runrepl defs _ =
    let val inp = TextIO.inputLine TextIO.stdIn
      fun hTerm t c p = 
          if c = 0
          then defs
          else let val res = reduceRename t
                   val flush = if c mod 10 = 0 orelse c < 10 then TextIO.print "#e" else ()
                   in case nextExec res of
                          SOME t => (TextIO.print (execStr res);
                                     hTerm t (c - 1) false)
                        | NONE   => (if p then TextIO.print (execStr res) else ();
                                     defs)
                   end

      fun hStmt (Eval (t, c))         = (hTerm t c true;
                                         defs)
        | hStmt (RenameDefs t)        = (TextIO.print ("\n" ^ termstr (renameDefs t defs));
                                         defs)
        | hStmt (Def (Define (s, t))) = (TextIO.print ("\n" ^ s ^ " :)");
                                         addDef (Define (s, t)) defs)
        | hStmt (Undef s)             = (TextIO.print ("\n" ^ s ^ " :(");
                                         removeDef s defs)
        | hStmt ShowDefs              = let fun printDef (Define (s, t)) =  TextIO.print ("\n" ^ s ^ " := " ^ termstr t)
                                        in (map printDef defs;
                                            defs)
                                        end

      fun handleString s =
          let val res = case readStmt s of
                            SOME st => hStmt st
                          | NONE    => (TextIO.print "#w"; defs)
          in (TextIO.print "#e"; res)
          end

    in case inp of
           NONE           => OS.Process.failure
         | SOME "#end\n"  => OS.Process.success
         | SOME s         => runrepl (handleString (hd (String.fields (fn c => c = #"|") s))) ()
    end

fun repl x = (TextIO.print "Meep meep meep! :D\n#e"; runrepl [] x; ())

end
