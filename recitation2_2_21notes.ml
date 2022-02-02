(* generiucs & higher order functions *)

(* Generics
- writing 'a (or 'b, 'c etc.) in place of the type allows that value to be of any type
-generics allow us to generalize functions that take in a specific type (tick a, tick b, etc.)
*)

let first_el_string (l: string list) : string =
  begin match l with
    | [] -> failwith "empty list"
    | hd::tl -> hd
   end
   
 
let first_el_element (l: 'a) : string =
  begin match l with
    | [] -> failwith "empty list"
    | hd::tl -> hd
   end
   
let f1 (g : int -> int) (y: int) : int=
  g 5 * y in
let f2 (x: int) : int = 
  x + 2 in
  
f1 f2 4

(* f1 f2 4. so it is f1 (f2 5 times 4) which is 7 times 4 is 28 *)

(* Transform and Fold *)

let rec flip_bools (l: bool list) : bool list = 
  begin match l with
    | [] -> []
    | hd::tl -> not hd :: flip_bools
   end
   
let rec div_value_list (l: int list) (divisor : int): bool list = 
  begin match l with
    | [] -> []
    | hd::tl -> hd / divisor :: div_value_list tl divisor
   end
(* lot in common : both recurive
take in a list and returna list
pattern match
do somethign to each element of the list *)

let rec useful_function (l : 'a list) : 'b list =
  begin match l with
    | [] -> []
    | hd :: tl -> (* do smth to hd *):: useful_function tl 
   end
   
let rec useful_function (f: 'a -> 'b) (l : 'a list) : 'b list =
  begin match l with
    | [] -> []
    | hd :: tl -> f hd :: useful_function tl 
   end
   
let rec transform (f: 'a -> 'b) (l : 'a list) : 'b list =
  begin match l with
    | [] -> []
    | hd :: tl -> f hd :: transform tl 
   end
(* note: transform returns a list *)
(* combine 'a -> 'b -> 'b *)
let rec fold (combine: 'a -> 'b -> 'b) (base : 'b) (l: 'a list) : 'b = 
  begin match l with
    | [] -> base
    | hd :: tl -> combine hd (fold combine base tl)
   end
 (* make sure you know the change in types for combine as well as just goin from type to type *)
(* transform - applies a fucntion to each element of a list
fold - combines elemnts in a list in some way*)

(*write a function incrememnt that increments each elemtn in a list by 1*)
let increment (l: int list) : int list = 
  transform( fun x -> x +1) l
(* write a function concat_each that combines each pair of strings in a tuple  list with a separator in between. *)

let concat_each (s: string) (l: (string * string) list : string list =
  transform (fun (s1,s2) -> s1^s^s2) l
  (* OR *)
  fold (fun (s1,s2) (* x *) acc-> (s1^s^s2)::acc)[] l   
(* acc is liike a recursive call *)  
  begin match l with 
    | [] -> []
    | (s1,s2) :: tl -> (s1^s^s2) :: acc
   end

fold ( fun x acc -> x = v || acc) false l
