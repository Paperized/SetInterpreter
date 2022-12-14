# SetInterpreter
Small interpreter written in OCaml with Sets and functionality to work with them (Map, Filter, etc...)

To run it you need OCaml installed or copy and paste the "interpreter.ml" file here https://try.ocamlpro.com/

This is a small interpreter, it has some basic programming language functionalities like if-else, variable declaration, functions and recursive functions.
In addition to this there is a structure set-like with a lot of functionalities, it can be extended pretty easily.

Example taken from the file tests:

```
(* Filter Function (delete negative numbers, then the positive numbers) *)
let setInt5 = Of("int", [EInt(10); EInt(6); EInt(-9); EInt(99); EInt(-99); EInt(-1)]);;
eval (Filter(Fun("x", Ge(Den("x"), EInt(0))), setInt5)) myEnv;;
eval (Filter(Fun("x", Ge(EInt(0), Den("x"))), setInt5)) myEnv;;

(* Map Function (Map a list of integers and multiply each of them by themselves, then convert them to strings) *)
let setInt6 = Of("int", [EInt(4); EInt(6); EInt(1); EInt(10); EInt(11); EInt(-1)]);;
let mapToPow = Map(pow, setInt6);;
let mapToString = Map(Fun("x", ToString(Den("x"))), mapToPow);;
```
