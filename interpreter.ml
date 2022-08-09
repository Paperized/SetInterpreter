type ide = string;;
type typeSet = string;;

(* Espressioni utilizzabili dall'interprete *)
type exp =
  | EInt of int
  | EBool of bool
  | EString of string
  | ESet of (exp list) * typeSet
  | Den of ide
  | Sum of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Div of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
  | Eq of exp * exp
  | Ge of exp * exp
  | IfThenElse of exp * exp * exp
  | Let of ide * exp * exp
  | Fun of ide * exp
  | FunRec of ide * ide * exp
  | FunApply of exp * exp
  | Empty of typeSet
  | Singleton of exp * typeSet
  | Of of typeSet * (exp list)
  | Insert of exp * exp
  | Remove of exp * exp
  | IsEmpty of exp
  | Contains of exp * exp
  | IsSubSet of exp * exp
  | Min of exp
  | Max of exp
  | UnionSet of exp * exp
  | IntersectionSet of exp * exp
  | SubtractSet of exp * exp
  | ForAll of exp * exp
  | Exists of exp * exp
  | Filter of exp * exp
  | Map of exp * exp
  | ToString of exp;;

(* Tipi di variabile a runtime e definizione di ambiente*)
type evT =
  | Int of int
  | Bool of bool
  | String of string
  | FunClosure of ide * exp * env
  | RecClosure of ide * ide * exp * env
  | Unbound
  | Set of (evT list) * typeSet
and env = (ide * evT) list;;

(* Binding di una variabile e valore ad un ambiente *)
let bind (id : ide) (value : evT) (ev : env) : env = (id, value)::ev;;

(* Controllo identificativo all'interno dell'ambiente *)
let rec lookupEnv (id : ide) (ev : env) : evT =
  match ev with
  | [] -> Unbound
  | (x, value)::others -> if id = x then value else lookupEnv id others;;

(* Funzione centralizzata typecheck dinamico *)
let typecheck (tp: typeSet) (value: evT) : bool =
  match tp with
  | "int" -> (match value with
      | Int(_) -> true
      | _ -> false)
  | "bool" -> (match value with
      | Bool(_) -> true
      | _ -> false)
  | "string" -> (match value with
      | String(_) -> true
      | _ -> false)
  | "set" -> (match value with
      | Set(_) -> true
      | _ -> false)
  | _ -> failwith("Valore non supportato!");;

(* Tipi possibili di un set *)
let typesAvailableSet (tp: typeSet) : bool =
  match tp with
  | "int" -> true
  | "bool" -> true
  | "string" -> true
  | _ -> failwith("Types not available <set>");;

(* Restituisce la stringa del tipo di una variabile *)
let guessType (v: evT) : typeSet =
  match v with
  | Int(_) -> "int"
  | Bool(_) -> "bool"
  | String(_) -> "string"
  | Set(_) -> "set"
  | _ -> "Invalid";;

(* Converte una variabile in stringa *)
let valueToString (v: evT) : evT =
  match v with
  | Int(value) -> String(string_of_int value)
  | Bool(value) -> String((if value = true then "true" else "false"))
  | String(value) -> String(value)
  | _ -> failwith("<valueToString> only int, bool and string can be converted to string!");;

(* Funzione typecheck per tipi set *)
let typecheckValueSet (set: (evT list) * typeSet) (value: evT) : bool =
  let (_, tp) = set in typecheck tp value;;

let sum x y = if typecheck "int" x && typecheck "int" y then
    match (x,y) with
    | (Int(n), Int(u)) -> Int(n + u)
    | _ -> failwith("<sum> Errore di tipo")
  else
    failwith("<sum> Errore di tipo");;

let sub x y = if typecheck "int" x && typecheck "int" y then
    match (x,y) with
    | (Int(n), Int(u)) -> Int(n - u)
    | _ -> failwith("<sub> Errore di tipo")
  else
    failwith("<sub> Errore di tipo");;

let mult x y = if typecheck "int" x && typecheck "int" y then
    match (x,y) with
    | (Int(n), Int(u)) -> Int(n * u)
    | _ -> failwith("<mult> Errore di tipo")
  else
    failwith("<mult> Errore di tipo");;

let div x y = if typecheck "int" x && typecheck "int" y then
    match (x,y) with
    | (Int(_), Int(0)) -> failwith("<div> Impossibile dividere per 0.")
    | (Int(n), Int(u)) -> Int(n * u)
    | _ -> failwith("<div> Errore di tipo")
  else
    failwith("<div> Errore di tipo");;

let eq x y = match (x,y) with
  | (Int(n), Int(u)) -> Bool(n = u)
  | (Bool(n), Bool(u)) -> Bool(n = u)
  | (String(n), String(u)) -> Bool(n = u)
  | _ -> failwith("<eq> Errore tipi differenti");;

let ge x y = if typecheck "int" x && typecheck "int" y then
    match (x,y) with
    | (Int(n), Int(u)) -> Bool(n >= u)
    | _ -> failwith("<ge> Errore tipi differenti")
  else
    failwith("<ge> Errore di tipo");;

let andOp x y = if typecheck "bool" x && typecheck "bool" y then
    match (x,y) with
    | (Bool(n), Bool(u)) -> Bool(n && u)
    | _ -> failwith("<andOp> Errore di tipo")
  else
    failwith("<andOp> Errore di tipo");;

let orOp x y = if typecheck "bool" x && typecheck "bool" y then
    match (x,y) with
    | (Bool(b), Bool(e)) -> Bool(b || e)
    | _ -> failwith("<orOp> Errore di tipo")
  else
    failwith("<orOp> Errore di tipo");;

let notOp x = if typecheck "bool" x then
    match x with
    | Bool(true)  -> Bool(false)
    | Bool(false) -> Bool(true)
    | _ -> failwith("<notOp> Errore di tipo")
  else
    failwith("<notOp> Errore di tipo");;

(* Crea un set vuoto di tipo 'tp' *)
let emptySet (tp: typeSet) : evT = 
  if typesAvailableSet tp then
    Set([], tp)
  else
    failwith("<emptySet> type not supported!");;

(* Crea un set a partire da un tipo e un valore *)
let singletonSet (tp: typeSet) (x: evT) : evT = 
  if typesAvailableSet tp then
    match (tp, x) with
    | ("int", Int(_)) -> Set([x], tp)
    | ("bool", Bool(_)) -> Set([x], tp)
    | ("string", String(_)) -> Set([x], tp)
    | _ -> failwith("<singletonSet> value x is not of type " ^ tp)
  else
    failwith("<singletonSet> type not supported!");; 

(* Controlla se un set contiene un valore x *)
let containsSet (set: (evT list) * typeSet) (x: evT) : evT =
  let rec aux lst x =
    (match (lst, x) with
     | ([], _) -> Bool(false)
     | (y::ys, o) -> if y = o then Bool(true) else aux ys x)
  in let (ls, _) = set in
  if (typecheckValueSet set x) then
    aux ls x
  else
    failwith("<containsSet> type x not supported!");;

(* Controlla se l'insieme set1 è contenuto in set2 *)
let isSubsetOf (set1: (evT list) * typeSet) (set2: (evT list) * typeSet) : evT =
  let rec aux lst1 lst2 typ =
    (match (lst1, lst2) with
     | ([], []) -> Bool(true)
     | (_, []) -> Bool(false)
     | ([], _) -> Bool(true)
     | (x::xs, _) -> if (containsSet (lst2, typ) x) = Bool(true) then 
           aux xs lst2 typ
         else
           Bool(false))
  in let (ls1, tp1) = set1 in let (ls2, tp2) = set2 in
  if (tp1 = tp2 && typesAvailableSet tp1) then
    aux ls1 ls2 tp1
  else
    Bool(false)

(* Inserisce un valore univoco all'interno di un set *)
let insertSet (set: (evT list) * typeSet) (x: evT) : evT =
  if containsSet set x = Bool(true) then
    let (lst, tp) = set in Set(lst, tp)
  else
  if typecheckValueSet  set x then
    let (lst, tp) = set in Set(x::lst, tp)
  else 
    failwith("<insertSet> type x not supported");;

(* Rimuove un valore da un set *)
let removeSet (set: (evT list) * typeSet) (x: evT) : evT =
  let rec aux lst x =
    (match (lst, x) with
     | ([], _) -> []
     | (y::ys, o) -> if y = o then ys else y::(aux ys x))
  in if typecheckValueSet set x then
    let (ls, tp) = set in Set((aux ls x), tp)
  else 
    failwith("<removeSet> Type x not supported");;

(* Crea un set a partire da un tipo e una lista di valori *)
let ofSet (tp: typeSet) (lst: (evT list)) : evT  = 
  let rec aux l res =
    match l with
    | [] -> res
    | x::xs -> let newSet = insertSet res x in
        match newSet with
        | Set(newLst, _) -> aux xs (newLst, tp)
        | _ -> failwith("Something went wrong..")
  in if typesAvailableSet tp then
    let (newLst, _) = aux lst ([], tp) in
    Set(newLst, tp)
  else
    failwith("<of> type not supported!");;

(* Controlla se il set è vuoto *)
let isEmptySet (set: (evT list) * typeSet) : evT =
  let (lst, _) = set in
  match lst with
  | [] -> Bool(true)
  | _ -> Bool(false);; 

(* Restituisce il minimo o massimo di un set *)
let getMinMaxSet (set: (evT list) * typeSet) (mode: string) : evT =
  let rec find lst typ =
    let pickCondition f1 f2 = (if mode = "max" then f1 >= f2 else f1 <= f2)
    in (match lst with
        | [] -> Unbound
        | x::y::ys -> if pickCondition x y then
              find (x::ys) typ
            else
              find (y::ys) typ
        | x::_ -> x)
  in let (l, t) = set in
  if typesAvailableSet t then
    if mode = "max" || mode = "min" then
      find l t
    else
      failwith("<getMinMaxSet> internal error, select a right mode")
  else
    failwith("<getMinMaxSet> type not allowed!");;

(* Unione tra due set *)
let unionSet (set1: (evT list) * typeSet) (set2: (evT list) * typeSet): evT = 
  let rec aux lst1 lst2 typ =
    (match lst1 with
     | [] -> lst2
     | x::xs -> let newSet = insertSet (lst2, typ) x in
         match newSet with
         | Set(newLst, _) -> aux xs newLst typ
         | _ -> failwith("Something went wrong.."))
  in let (ls1, tp1) = set1 in let (ls2, tp2) = set2 in
  if (typesAvailableSet tp1 && typesAvailableSet tp2) then
    if (tp1 = tp2) then
      Set((aux ls1 ls2 tp1), tp1)
    else
      failwith("<unionSet> union set must be between similar types!")
  else
    failwith("<unionSet> type set1 or set2 not allowed!");;

(* Intersezione tra due set *)
let intersectionSet (set1: (evT list) * typeSet) (set2: (evT list) * typeSet): evT =
  let rec aux lst1 lst2 typ =
    (match lst1 with
     | [] -> []
     | x::xs -> if (containsSet (lst2, typ) x) = Bool(false) then
           aux xs lst2 typ
         else
           x::(aux xs lst2 typ))
  in let (ls1, tp1) = set1 in let (ls2, tp2) = set2 in
  if (typesAvailableSet tp1 && typesAvailableSet tp2) then
    if (tp1 = tp2) then
      Set((aux ls1 ls2 tp1), tp1)
    else
      failwith("<intersectionSet> union set must be between similar types!")
  else
    failwith("<intersectionSet> type set1 or set2 not allowed!");;

(* Sottrae il primo set dal secondo (set2 - set1) *)
let subtractSet (set1: (evT list) * typeSet) (set2: (evT list) * typeSet): evT = 
  let rec aux lst1 lst2 typ =
    (match (lst1, lst2) with
     | ([], _) -> []
     | (_, []) -> lst1
     | (x::xs, _) -> let newSet = removeSet (lst1, typ) x in
         match newSet with
         | Set(newLst, _) -> aux newLst xs typ
         | _ -> failwith("Something went wrong..."))
  in let (ls1, tp1) = set1 in let (ls2, tp2) = set2 in
  if (typesAvailableSet tp1 && typesAvailableSet tp2) then
    if (tp1 = tp2) then
      Set((aux ls1 ls2 tp1), tp1)
    else
      failwith("<subtractSet> union set must be between similar types!")
  else
    failwith("<subtractSet> type set1 or set2 not allowed!");;

(* Funzione chiave dell'interpete, data un espressione e un ambiente la risolve e restituisce il risultato 
 * Ogni espressione è gestita separatamente e ognuna ha la propria implementazione valutando in maniera
 * ricorsiva ogni espressione *)
let rec eval (e : exp) (ev : env) : evT =
  match e with
  | EInt(x) -> Int(x)
  | EBool(b) -> Bool(b)
  | EString(s) -> String(s)
  | ESet(value, tp) -> createSet (value, tp) ev
  | ToString(x) -> valueToString (eval x ev)
  | Den(x) -> lookupEnv x ev
  | Sum(x, y) -> sum (eval x ev) (eval y ev)
  | Sub(x, y) -> sub (eval x ev) (eval y ev)
  | Mul(x, y) -> mult(eval x ev) (eval y ev)
  | Div (x, y) -> div(eval x ev) (eval y ev)
  | Ge (x, y) -> ge (eval x ev) (eval y ev)
  | And(x, y) -> andOp (eval x ev) (eval y ev)
  | Or(x, y) -> orOp (eval x ev) (eval y ev)
  | Not(x) -> notOp (eval x ev)
  | Eq(x, y) -> eq (eval x ev) (eval y ev)
  | IfThenElse(cond, e1, e2) -> (match eval cond ev with
      | Bool(true) -> eval e1 ev
      | Bool(false) -> eval e2 ev
      | _ -> failwith("Failed eval IfThenElse, condition not boolean"))
  | Let(id, e1, e2) -> eval e2 (bind id (eval e1 ev) ev)
  | Fun(param, body) -> FunClosure(param, body, ev)
  | FunRec(id, param, body) -> RecClosure(id, param, body, ev)
  | FunApply(f, e) -> let evalue = eval e ev in
      let fclose = eval f ev in
      internalFunApply fclose evalue
  | Empty(tp) -> emptySet tp
  | Singleton(x, tp) -> singletonSet tp (eval x ev)
  | Of(tp, lst) -> let rec lstToEvt l =
                     (match l with
                      | [] -> []
                      | x::xs -> (eval x ev)::(lstToEvt xs))
      in ofSet tp (lstToEvt lst)
  | Insert(set, x) -> (let newSet = eval set ev in
                       let v = eval x ev in
                       match newSet with
                       | Set(lst, tp) -> insertSet (lst, tp) v
                       | _ -> failwith("Failed eval Insert, it's not a Set")) 
  | Remove(set, x) -> (let newSet = eval set ev in
                       let v = eval x ev in
                       match newSet with
                       | Set(lst, tp) -> removeSet (lst, tp) v
                       | _ -> failwith("Failed eval Remove, it's not a Set"))
  | IsEmpty(set) -> (let newSet = eval set ev in
                     match newSet with
                     | Set(lst, tp) -> isEmptySet (lst, tp)
                     | _ -> failwith("Failed eval IsEmpty, it's not a Set"))
  | Contains(set, x) -> (let newSet = eval set ev in
                         let v = eval x ev in
                         match newSet with
                         | Set(lst, tp) -> containsSet (lst, tp) v
                         | _ -> failwith("Failed eval Contains, it's not a Set")) 
  | IsSubSet(set1, set2) -> (let newSet1 = eval set1 ev in
                             let newSet2 = eval set2 ev in
                             match (newSet1, newSet2)  with
                             | (Set(lst1, tp1), Set(lst2, tp2)) -> isSubsetOf (lst1, tp1) (lst2, tp2)
                             | _ -> failwith("Failed eval IsSubSet, both parameters must be sets"))
  | Min(set) -> (let newSet = eval set ev in
                 match newSet with
                 | Set(lst, tp) -> getMinMaxSet (lst, tp) "min"
                 | _ -> failwith("Failed eval Min, it's not a Set"))
  | Max(set) -> (let newSet = eval set ev in
                 match newSet with
                 | Set(lst, tp) -> getMinMaxSet (lst, tp) "max"
                 | _ -> failwith("Failed eval Max, it's not a Set"))
  | UnionSet(set1, set2) -> (let newSet1 = eval set1 ev in
                             let newSet2 = eval set2 ev in
                             match (newSet1, newSet2)  with
                             | (Set(lst1, tp1), Set(lst2, tp2)) -> unionSet (lst1, tp1) (lst2, tp2)
                             | _ -> failwith("Failed eval UnionSet, both parameters must be sets"))
  | IntersectionSet(set1, set2) -> (let newSet1 = eval set1 ev in
                                    let newSet2 = eval set2 ev in
                                    match (newSet1, newSet2)  with
                                    | (Set(lst1, tp1), Set(lst2, tp2)) -> intersectionSet (lst1, tp1) (lst2, tp2)
                                    | _ -> failwith("Failed eval UnionSet, both parameters must be sets"))
  | SubtractSet(set1, set2) -> (let newSet1 = eval set1 ev in
                                let newSet2 = eval set2 ev in
                                match (newSet1, newSet2)  with
                                | (Set(lst1, tp1), Set(lst2, tp2)) -> subtractSet (lst1, tp1) (lst2, tp2)
                                | _ -> failwith("Failed eval UnionSet, both parameters must be sets"))
  | ForAll(predicate, set) -> forAllSet predicate set ev
  | Exists(predicate, set) -> existsSet predicate set ev
  | Filter(predicate, set) -> filterSet predicate set ev
  | Map(predicate, set) -> mapSet predicate set ev
(* Converte un ESet in Set *)
and createSet (set: (exp list) * typeSet) (ev: env) : evT =
  let (lst, tp) = set in
  if typesAvailableSet tp then
    let rec aux s res =
      let (lt, t) = s in
      match lt with
      | [] -> res
      | x::xs -> let newSet = insertSet res (eval x ev) in
          match newSet with
          | Set(newLst, _) -> aux (xs, t) (newLst, t)
          | _ -> failwith("Something went wrong..")
    in let (resLst, _) = aux (lst, tp) ([], tp) 
    in Set(resLst, tp)
  else
    failwith("<createSet> type not supported")
(* Funzionalità che esegue una funziona all'interno del proprio ambiente *)
and internalFunApply (closure: evT) (evalue: evT) : evT =
  match closure with
  | FunClosure(idparam, body, env) ->
      eval body (bind idparam evalue env)
  | RecClosure(fid, idparam, body, env) ->
      let recEnv = bind fid closure env in
      eval body (bind idparam evalue recEnv)
  | _ -> failwith("Invalid function")
(* Funzione che esegue l'eval sul predicato e set derivanti, altrimente lancia un eccezione *)
and evalPredicateAndSet (pred: exp) (set: exp) (ev: env) =
  (let func = eval pred ev in
   let newSet = eval set ev in
   match (func, newSet) with
   | (FunClosure(p, b, _ev), Set(ls, tp)) -> (FunClosure(p, b, _ev), (ls, tp))
   | (RecClosure(id, p, b, _ev), Set(ls, tp)) -> (RecClosure(id, p, b, _ev), (ls, tp))
   | (_, _) -> failwith("Failed evalPredicateAndSet, first parameter must be a function and second must be a set"))
(* Controlla che ogni valore all'interno di un set rispetti un predicato *)
and forAllSet (pred: exp) (set: exp) (ev: env) : evT =
  let rec aux f lst : bool =
    (match lst with
     | [] -> true
     | x::xs -> let evaluated = internalFunApply f x in
         (match evaluated with
          | Bool(true) -> (true && (aux f xs))
          | Bool(false) -> false
          | _ -> failwith("<forAllSet> Predicate must return a boolean expression"))) in
  let (func, (newLst, _)) = evalPredicateAndSet pred set ev in
  Bool(aux func newLst)
(* Controlla che almeno un elemento all'interno di un set rispetti un predicato *)
and existsSet (pred: exp) (set: exp) (ev: env) : evT =
  let rec aux f lst : bool =
    (match lst with
     | [] -> false
     | x::xs -> let evaluated = internalFunApply f x in
         (match evaluated with
          | Bool(true) -> true
          | Bool(false) -> aux f xs
          | _ -> failwith("<existsSet> Predicate must return a boolean expression"))) in
  let (func, (newLst, _)) = evalPredicateAndSet pred set ev in
  Bool(aux func newLst)
(* Dato un set iniziale e un predicato filtra ogni elemento *)
and filterSet (pred: exp) (set: exp) (ev: env) : evT =
  let rec aux f lst t : (evT list) * typeSet =
    (match lst with
     | [] -> ([], t)
     | x::xs -> let (l, t) = aux f xs t in
         let evaluated = internalFunApply f x in
         match evaluated with
         | Bool(true) -> (let newSet = insertSet (l, t) x in
                          match newSet with
                          | Set(newLst, _) -> (newLst, t)
                          | _ -> failwith("Something went wrong..."))
         | Bool(false) -> (l, t)
         | _ -> failwith("<filterSet> Predicate must return a boolean expression")) in
  let (func, (newLst, tp)) = evalPredicateAndSet pred set ev in
  let (resLst, _) = aux func newLst tp in
  Set(resLst, tp)
(* Dato un set esegue il map di ogni valore con un predicato *)
and mapSet (pred: exp) (set: exp) (ev: env) : evT =
  let rec aux f lst : (evT list) * typeSet =
    (match lst with
     | [] -> ([], "Unbound")
     | x::xs -> let (l, t) = aux f xs in
         if t = "Unbound" then
           let evaluated = internalFunApply f x in
           let tp = guessType evaluated in
           if typesAvailableSet tp then
             ([evaluated], tp)
           else
             failwith("<mapSet> Predicate must return a valid type value")
         else
           let evaluated = internalFunApply f x in
           let newSet = insertSet (l, t) evaluated in
           match newSet with
           | Set(newLst, newT) -> (newLst, newT)
           | _ -> failwith("<mapSet> Predicate must return alwyas the same data type, set must be omogenous")) in
  let (func, (newLst, tp)) = evalPredicateAndSet pred set ev in
  let (newLst, newTp) = aux func newLst in
  if newTp = "Unbound" then
    Unbound
  else
    Set(newLst, newTp);;

(* Test interprete *)
(* Creo un ambiente *)
let myEnv = [];;

(* Definisco la funzione del quadrato e la invoco con 10 *)
let pow = Fun("x", Mul(Den("x"), Den("x")));;
eval (FunApply(pow, EInt(10))) myEnv;;

(* Definisco il fattoriale (ricorsivo) e lo invoco su 4 *)
let fact = FunRec("fact", "x", 
                  IfThenElse(Eq(Den("x"), EInt(2)), 
                             EInt(2), 
                             Mul(Den("x"), FunApply(Den("fact"), Sub(Den("x"), EInt(1))))));;
eval (FunApply(fact, EInt(4))) myEnv;;

(* Creo un set in 4 modi diversi; cotruttore, empty, singleton , of *)
let setConstr = ESet([EInt(45); EInt(19); EInt(0)], "int");;
eval setConstr myEnv;;

let setEmpty = Empty("string");;
eval setEmpty myEnv;;

let setSingleton = Singleton(EBool(true), "bool");;
eval setSingleton myEnv;;

let setOf = Of("string", [EString("anatra"); EString("basilicata"); EString("zio")]);;
eval setOf myEnv;;

(* Controllo il set vuoto *)
eval (IsEmpty(setEmpty)) myEnv;;

(* Inserisco degli elementi nell'insieme vuoto *)
let setEmpty1 = Insert(setEmpty, EString("nuovo"));;
eval setEmpty1 myEnv;;
let setEmpty2 = Insert(setEmpty1, EString("zio"));;
eval setEmpty2 myEnv;;
let setEmpty3 = Insert(setEmpty2, EString("nuovo"));;
eval setEmpty3 myEnv;;

(* Controllo se contiene un elemento dopo averlo rimosso *)
eval (Contains(setEmpty3, EString("nuovo"))) myEnv;;
let setEmpty4 = Remove(setEmpty3, EString("nuovo"));;
eval setEmpty4 myEnv;;
eval (Contains(setEmpty4, EString("nuovo"))) myEnv;;

(* Controllo se setEmpty è sottoinsieme di setOf e anche dopo aver aggiunto un elemento diverso *)
eval (IsSubSet(setEmpty4, setOf)) myEnv;;
let setEmpty5 = Insert(setEmpty4, EString("stringa diversa"));;
eval setEmpty5 myEnv;;
eval (IsSubSet(setEmpty5, setOf)) myEnv;;

(* Controllo minimo e massimo dell'insieme setOf, essendo stringhe è un confronto tra stringhe, il min/max è lessicografico *)
eval (Min(setOf)) myEnv;;
eval (Max(setOf)) myEnv;;

(* Operazioni insiemistiche; unione, intersezione e sottrazione *)
let setInt1 = Of("int", [EInt(0); EInt(5)]);;
let setInt2 = Of("int", [EInt(100); EInt(5); EInt(9)]);;

eval (UnionSet(setInt1, setInt2)) myEnv;;
eval (IntersectionSet(setInt1, setInt2)) myEnv;;
eval (SubtractSet(setInt1, setInt2)) myEnv;;

(* Funzionalità ForAll (controllo positivi) *)
let setInt3 = Of("int", [EInt(-1); EInt(1); EInt(8)]);;
eval (ForAll(Fun("x", Ge(Den("x"), EInt(0))), setInt3)) myEnv;;

let setInt3Positivi = Remove(setInt3, EInt(-1));;
eval (ForAll(Fun("x", Ge(Den("x"), EInt(0))), setInt3Positivi)) myEnv;;

(* Funzionalità Exists (esiste il 10?) *)
let setInt4 = Of("int", [EInt(-1); EInt(1); EInt(8)]);;
eval (Exists(Fun("x", Eq(Den("x"), EInt(10))), setInt4)) myEnv;;

let setInt4With10 = Insert(setInt4, EInt(10));;
eval (Exists(Fun("x", Eq(Den("x"), EInt(10))), setInt4With10)) myEnv;;

(* Funzionalità Filter (rimuovo gli elementi negativi, poi i positivi [0 numero neutro]) *)
let setInt5 = Of("int", [EInt(10); EInt(6); EInt(-9); EInt(99); EInt(-99); EInt(-1)]);;
eval (Filter(Fun("x", Ge(Den("x"), EInt(0))), setInt5)) myEnv;;
eval (Filter(Fun("x", Ge(EInt(0), Den("x"))), setInt5)) myEnv;;

(* Funzionalità Map (Mappo una lista di interi nel proprio quadrato, successivamente li mappo in un set di stringhe *)
let setInt6 = Of("int", [EInt(4); EInt(6); EInt(1); EInt(10); EInt(11); EInt(-1)]);;
let mapToPow = Map(pow, setInt6);;
let mapToString = Map(Fun("x", ToString(Den("x"))), mapToPow);;

eval mapToPow myEnv;;
eval mapToString myEnv;;