(* Units *)
(* unit is a type in OCaml. It looks like () when in funciton headers *)

(*you can pattern match it *)
|() -> print_endline "What am I suppposed to do with this?"

(*commands: print_endline *)
(* it can be tyhe input type for a function *)
let memes() : int = 4
let x = memes()

(*unit type practice *)
let test () : bool = 
4+5 = 9
(* type is unit to bool *)
() test () : bool
(* type is just bool *)

(* what is type of run_test *)
;; run_test "simple test" test
(* string -> (unit -> bool) -> unit *)

(* sequencing *)
(* ; is the sequeeencing operator. 
Using ; allows us to do multiple things in sequence
"Throws away" return value of what comes before; *)

let print_str (x: string) : int =
  if x = "5" then (print_endline x; 5) else 6
  
(* Options *)
(*Funcitons don't always have somethign concrete to evaluate to. 
  -HW 1 : What's the average when tehre is no rainfall?
  -HW 2 : What is the root of an empty tree
 -Conceptually, options are a solution to this problem
 -Syntactically, options are built-in to Ocaml, but we'll explicitly define them here as a user-defined type *)
 
 type 'a option = 
 | None
 | Some of 'a
 
 type 'a tree = 
  | Empty
  | Node of 'a tree * 'a * 'a tree
  
(*before we had options... *)
let rec bst_max (t: 'a tree) : 'a = 
  begin match t with 
    | Empty -> failwith "bst_max: no such element"
    | Node (_, n, Empty) -> n
    | Node (_, n, rt) -> bst_max rt
   end

let rec bst_max (t: 'a tree) : 'a option = 
  begin match t with 
    | Empty -> None
    | Node (_, n, Empty) -> Some n 
    | Node (_, N, rt) -> bst_max rt
  end
  
  (* records *)
  (* records are a new builty-in data structure, which we usually use in user-defined tyupes 
    -defining the record *)
   
   type date = { month: string; day: int }
   
   (* declaring a record *)
   
   let my_birthday : date = { month = "Aug"; day = 27 }
   
   ;; print)_endline (my_birthday. month ^ ""...)
   
   (* Records
        -"with" keyword
        - allows us to declare another record using the fields of a pre-existing record.  *)
   
  let her_birthday : date = {my_birthday with day = 3}

(* Mutabel Records *)
(* Mutable records let you update the values in the record. Fields whose type definiciotn have the mutable keyword beome mutable *)

type date = { mutable month: string; mutable day: int }

let my_birthday : date = { month = "Aug"; day = 27 }

;; my_birthday.month <- "Nov"
;; my_birthday.day <- 5
 
 
(* 'a ref 
- built in, mutable record type *)

type 'a ref = { mutable contents: 'a }

let r = {content = a} in
let r = ref a in

let x = r.contents in
r. contents <- a

(* The Abstract Stack Machine (ASM *)
(* the abstract stack machine is tripartite
1. Workspace: stores code being run
2. Stack: stores identifiers and primitive values
3. Heap: stores non-primitive values

W     S     H
*)

type teaching_assistant = { name : string; age : int }
type student = { name : string; ta : teaching_assistant }

let name : string = "Helen"
let age : int = 21
let ta : teaching_assistant = {name...
...
...
...


