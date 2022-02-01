(* Homework Assignment 1: OCaml Finger Exercises! *)

(* See the web pages for instructions on how to get started using Codio. *)

(* The following command tells OCaml to use the "assert" library that
   defines the run_test command used below. *)
;; open Assert

(* The assertion library by default will run _all_ of the test cases
   associated with this program.  However, while debugging you
   may prefer to have the testing stop on the first failure that
   is encountered. The command below requests this behavior.
   Remove or comment out the line to see all of the test errors at once. *)
;; stop_on_failure ()


(* NOTE: you should _not_ use functions built into OCaml, especially the ones in
   the List module, except where they are explicitly allowed in the comments.
   The purpose of this assignment is to familiarize you with the basics of OCaml
   programming, so we want you to explicitly write out each of these problems
   even though there is often a built-in function that would achieve the same
   result. You will not receive credit for solutions that are contrary to the
   spirit of the assignment. *)

(*************************************************************************)
(* Problem 1 (counting coins) *)

(* Your job in this problem is to calculate the smallest number of
   pennies, nickels, and dimes that can be used to add up to the given
   amount. For example, to make 7 cents, a total of 3 coins are
   needed (two pennies and a nickel); to make 99 cents, 14 coins are
   needed (9 dimes, 1 nickel, and 4 pennies).
  
   First, have a look at our tests (just below the definition of the
   coins function), make sure you understand them, and fill in two
   more tests of your own.
  
   Then come back and fill in the body of the function `coins` below
   so that it returns the right answers on all the tests. (Start by
   deleting the line beginning `failwith`.) *)

let rec coins (amount: int) : int =
  let dimes = amount / 10 in
  let minusdimes = amount mod 10 in
  let nickels = minusdimes/5 in
  let pennies = minusdimes mod 5 in
  pennies + nickels + dimes
  
(* Here are two test cases for this problem. *)

let test () : bool =
  (coins 7) = 3
;; run_test "coins nickels and pennies" test

let test () : bool =
  (coins 99) = 14
;; run_test "coins dimes, nickels, and pennies" test

(* Here are two more test case stubs. Please edit them to produce real
   tests for the coins function. For each of the problems in the
   assignment, we provide some test cases like the ones above.
   However, just because your code passes the given tests does not
   mean that you will get full credit. When you submit your
   assignment, we will test it using DIFFERENT tests from the ones
   above. To make sure that your solution is robust enough to pass our
   tests, you should think about what tests you can add to make sure
   that your program is correct.
  
   STARTING FROM HW 02, WE WILL GRADE YOU ON THE QUALITY AND ROBUSTNESS
   OF YOUR TEST CASES AS PART OF YOUR "STYLE GRADE."
  
   Please refer to the FAQ page for an explanation about test cases. *)

(* This test will show that 26 cents with 4 coins requires
   2 dimes, 1 nickel, and 1 penny. *)
let test () : bool =
  (coins 26) = 4
;; run_test "coins dimes, nickels, and pennies" test
(* This test will show that 12 cents with 3 coins requires
   1 dime and 3 pennies. *)
let test () : bool =
 (coins 12) = 3
;; run_test "coins dimes and pennies" test


(*************************************************************************)
(* Example (printing) *)

(* Printing is a useful tool, letting you see the output of your code
   on the console. In this part, we will show you how to print in
   OCaml. *)

(* Recall that OCaml files are composed of top-level definitions,
   which begin with the `let` keyword, and commands, which begin with
   two semicolons. One useful command instructs OCaml to print
   text. *)

(* The `print_endline` function causes its string argument to appear in the
   output window (much like `System.out.println` in Java). *)

;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~"
;; print_endline "Start of printing example"

(* Adding commands to print values can be very useful for debugging
   your assignment. For example, consider the following buggy
   function: *)

let day_after (day: string) : string =
  begin match day with
  | "Monday"    -> "Tuesday"
  | "Tuesday"   -> "Wednesday"
  | "Wednesday" -> "Thursday"
  | "Thursday"  -> "Friday"
  | "Friday"    -> "Saturday"
  | "Saturday"  -> "Sunday"
  | "Sunday"    -> "Monday"
  | _           -> failwith "not a valid day"
  end

(* The following test case for this definition fails, telling us that
   this definition definitely has a bug (since the test case matches
   our understanding of what the function is supposed to do). But
   running the program just tells us that the answer is wrong, without
   showing the actual answer. *)
;; print_endline ("The day after Tuesday is " ^ (day_after "Tuesday") ^ ".")

let test () : bool =
  (day_after "Tuesday") = "Wednesday"
;; run_test "day_after Tuesday" test

(* Adding a print command will let us see what the erroneous result
   actually is.

   Try moving the `print_endline` command from the line below to
   before the failing test case (so that its output is displayed
   before the test fails); then run your code again. *)

(* (After running this example, fix the bug in the `day_after` 
   function so that the test passes). *)

(* Note: If the result that you want to print is not a string, you
   need to convert it to be a string. OCaml includes two library
   functions for this conversion: `string_of_int` and `string_of_bool`.
  
   After you finish problem 1 above, uncomment the next command
   to demonstrate printing integer values. *)


;;print_endline ("Coins to make 99 cents is "
               ^ (string_of_int (coins 99)))


(* Feel free to add whatever printing commands you like to this
   homework assignment. The testing infrastructure will ignore any
   output that your code produces. *)

;; print_endline "End of printing example"
;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~"


(*************************************************************************)
(* Problem 2 (geometry) *)

(* Street magicians often use crates as tables in their acts.  Given
   the dimensions of a crate, your job in this part is to write a function to
   find the largest surface area it can provide when used as a table.
  
   Hint: OCaml provides built-in `max` and `min` functions that take in
   two arguments and behave exactly as you might expect: `max 5 2`
   returns 5, for example.  This problem can be solved using just `max`,
   `min`, and simple arithmetic.
  
   Your function's behavior when one or more of the input side lengths
   is zero or negative is not important. Your `maximum_table_area`
   function may return any value in such cases; we will not test them.
  
   Once again, you should look at our test cases first, then add your
   own test cases, and THEN come back and implement `maximum_table_area`. *)


let rec maximum_table_area (side1: int) (side2: int) (side3: int) : int =
  let a = max side1 side2 in
  if a <= side3 then a*side3
  else if min side1 side2 > side3 then side1 *side2
  else a*(max side2 side3)

let test () : bool =
  (maximum_table_area 1 2 3) = 6
;; run_test "maximum_table_area three different side lengths" test

let test () : bool =
  (maximum_table_area 4 3 3) = 12
;; run_test "maximum_table_area two sides the same length" test

let test () : bool =
   (maximum_table_area 4 4 4) = 16
;; run_test "maximum_table_area three sides the same length" test

let test () : bool =
  (maximum_table_area 2 4 6) = 24
;; run_test "maximum_table_area three different side lengths" test


(*************************************************************************)
(* Problem 3 (simulating robot movement) *)

(* Help a robot move along a (linear) track, with spaces numbered 0
   through 99, by calculating its new position when given `dir` (a
   string that will be either "forward" or "backward") and `num_moves`
   indicating a (non-negative) number of spaces.  Keep in mind that
   the robot can't move past the 0 or 99 spot, so when it reaches
   either end it stays there. *)

let rec move_robot (pos: int) (dir: string) (num_moves: int) : int =
  if dir = "forward" then (if pos+num_moves <=99 then pos + num_moves else 99)
  else (if pos - num_moves >=0 then pos- num_moves else 0)

let test () : bool =
  (move_robot 10 "forward" 3) = 13
;; run_test "move_robot forward in bounds" test

let test () : bool =
  (move_robot 1 "backward" 4 ) = 0
;; run_test "move_robot backward out of bounds" test

let test () : bool =
 (move_robot 10 "backward" 4 ) = 6
;; run_test "move_robot backward in bounds" test

let test () : bool =
 (move_robot 90 "forward" 10 ) = 99
;; run_test "move_robot forward out of bounds" test

let test () : bool =
 (move_robot 99 "forward" 10 ) = 99
;; run_test "move_robot forward 99+10 out of bounds" test


(*************************************************************************)
(* Problem 4 (Philadelphia geography) *)

(* Philadelphia has a fairly logical layout: the numbered streets
   are typically one-way, and their direction is determined by their
   number and where you are in the city.
  
   Even streets go one way and odd streets go another:
  
     East of Broad (< 14th): even go south, odd go north
     West of Broad (> 14th): even go north, odd go south
     West Philly  (>= 32nd): even go south, odd go north
     West Philly  (>= 46th): two-way
  
   There are, however, a few exceptions.
     - 1st and 14th do not actually exist as street names -- they're
       called Front and Broad. We'll ignore this and pretend they do.
     - Broad (14th), 25th, 38th, 41st, and 42nd are all two-way.
     - 24th and 59th go south.
     - 58th goes north.
  
   Write a program that returns one of four string values for each street
   number:
     - "N/A" when the street doesn't exist. We only consider 1st
       through 69th Streets.
     - "N" when the street goes north.
     - "S" when the street goes south.
     - "NS" when the street is two-way.
  
   Hints:
     - You may find the infix `mod` (modulo) function useful: for example,
       `x mod 2` evaluates to 0 if x is even and 1 otherwise.
     - Sometimes there is no simple-and-clever way of writing down a
       complex case analysis: you just have to write out all the cases.
  
   Welcome to Philadelphia! *)
(* edit this to be more efficient*)
let rec street_direction (st: int) : string =
  if st > 69 || st<1 then "N/A"
  else if st=14||st=25||st=38||st=41||st=42|| 
    (st>=46&&st<>59&&st<>58&&st<=69)
  then "NS"
 else if (st<14&&st mod 2=1)||(st>14&&st<32&&st mod 2=0&&st<>24)||
    (st>=32&&st mod 2=1)||st=58 
  then "N" 
  else "S"

let test () : bool =
  (street_direction 14) = "NS"
;; run_test "street_direction Broad is two-way" test

let test () : bool =
  (street_direction 0) = "N/A"
;; run_test "street_direction 0 DNE" test

let test () : bool =
  (street_direction 24) = "S"
;; run_test "street_direction 24 goes south" test

let test() : bool = 
  (street_direction (-1)) = "N/A"
;; run_test "street_direction -1 DNE" test

let test () : bool =
  (street_direction 9) = "N"
;; run_test "street_direction 9th goes north" test

let test () : bool =
  (street_direction 31) = "S"
;; run_test "street_direction 31th goes south" test

let test () : bool =
   (street_direction 48) = "NS"
;; run_test "street_direction 48th is two-way" test

let test () : bool =
  (street_direction 70) = "N/A"
;; run_test "street_direction 70 does not exist" test

let test () : bool =
   (street_direction 69) = "NS"
;; run_test "street_direction 69th is two-way" test


(*************************************************************************)
(* The remaining exercises provide practice with lists and recursion.
   It is best to wait until after that topic is covered in lecture before
   tackling these problems. *)

(*************************************************************************)
(* Problem 5 (exists) *)

(* Write a function that determines whether at least one boolean value
   in its input list is true. *)

let rec exists (bools: bool list) : bool =
  begin match bools with
    | [] -> false
    | x::tl -> x || exists tl
  end
(* The `not` function below takes in a boolean value and returns its
   complement *)
let test () : bool =
  not (exists [false; false])
;; run_test "exists all false" test

let test () : bool =
  (exists [true; false; true])
;; run_test "exists multiple true" test

let test () : bool =
  (exists [false; false; true])
;; run_test "exists multiple false where last is true" test

let test () : bool =
  (exists [true; true; true; false])
;; run_test "exists all is true except last" test


(*************************************************************************)
(* Problem 6 (join) *)

(* Write a function that takes a list of strings and "flattens" it
   into a single string. Your function should also take an additional
   argument, a separator string, which is interspersed between all of
   the strings in the list.
  
   Hint: the ^ operator concatenates two strings together. For example,
   `"a" ^ "bc"` evaluates to "abc". *)

let rec join (separator: string) (l: string list) : string =
  begin match l with 
    | [] -> ""
    | x::[] -> x
    | x::tl -> x^separator^join separator tl
  end

let test () : bool =
  (join "," ["a"; "b"; "c"]) = "a,b,c"
;; run_test "test_join nonempty separator" test

let test () : bool =
  (join "" ["a"; "b"; "c"]) =  "abc"
;; run_test "test_join empty separator" test

let test () : bool =
 (join "hello" ["a"]) = "a"
;; run_test "test_join separator with one element" test

let test () : bool =
 (join " " []) = ""
;; run_test "test_join empty separator with empty list" test


(*************************************************************************)
(* Example (printing lists) *)

;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~"
;; print_endline "Start of list printing example"

(* Once you have implemented the `join` function above, you can use it
   to print out lists of strings. This can be very useful for
   debugging the remaining tasks in this assignment, as you can print
   out the output of your functions to help you understand why your
   test cases are failing *)

;; print_endline (join "," ["a"; "b"; "c"])


(* If you would like to print a list of `int`s, you'll need a variant
   of the `join` function for this purpose. We advise that you go
   ahead and do this so that you can use this function to help debug
   the last few tasks in this homework. *)

let rec int_join (separator: string) (l: int list) : string =
    begin match l with 
      | [] -> ""
      | x::[] -> string_of_int(x)
      | x::tl -> string_of_int(x)^separator^int_join separator tl
    end


;; print_endline ("[" ^ (int_join ";" [1; 2; 3]) ^ "]")


;; print_endline "End of list printing example"
;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~"

(*************************************************************************)
(* Problem 7 (append) *)

(* Write a function that takes lists l1 and l2 and returns a list
   containing all the items in l1 followed by all the items in l2.
  
   NOTE: OCaml actually already provides this function. In future
   homeworks you can use built in operator `@`, which appends l1 and l2
   as in l1 @ l2. Do *not* use the @ operator in your solution to this
   problem. *)

let rec append (l1: string list) (l2: string list) : string list =
  begin match l1 with 
    | [] -> l2
    | x::tl -> x:: append tl l2
  end

let test () : bool =
  (append [] []) = []
;; run_test "append two empty lists" test

let test () : bool =
  (append ["1"; "2"] ["3"]) = ["1"; "2"; "3"]
;; run_test "append different lengths" test

let test () : bool =
  (append ["1"; "2"; "3"] []) = ["1"; "2"; "3"]
;; run_test "append first unempty list then second empty list" test

let test () : bool =
  (append []["1";"2";"3"]) = ["1"; "2"; "3"]
;; run_test "append first empty list then second unempty list" test


(*************************************************************************)
(* Problem 8 (finding names in a list) *)

(* Write a function that checks whether a list of names contains some
   particular name. *)

let rec contains_str (l: string list) (name: string) : bool =
  begin match l with 
   | [] -> false
   | x::tl -> x = name || contains_str tl name
  end

let test () : bool =
  (contains_str ["Garnet"; "Amethyst"; "Pearl"] "Amethyst")
;; run_test "contains_str name in list once" test

let test () : bool =
  not (contains_str ["Garnet"; "Amethyst"; "Pearl"] "Steven")
;; run_test "contains_str name not in list" test

let test () : bool =
  (contains_str ["Garnet"; "Amethyst"; "Amethyst"; "Pearl"] "Amethyst")
;; run_test "contains_str name in list twice" test

let test () : bool =
  (contains_str ["Garnet"; "Garnet"; "Garnet"] "Garnet")
;; run_test "contains_str name in list all times" test


(* Next, write a function that, given two lists of names, filters the
   first list so that only those that are also in the second list
   remain. That is, your function should return a list containing all
   the elements that appear in both lists, in the order that they
   appear in the first list. *)

let rec in_both (names1: string list) (names2: string list) : string list =
  begin match names1 with
    | [] -> []
    | x::tl -> if contains_str names2 x then x::in_both tl names2 else in_both tl names2
  end

let test () : bool =
  (in_both ["Garnet"; "Amethyst"; "Pearl"] ["Pearl"; "Steven"]) = ["Pearl"]
;; run_test "in_both Pearl in both lists" test

let test () : bool =
  (in_both [] ["Pearl"; "Steven"]) = []
;; run_test "in_both empty name list" test

let test () : bool =
  (in_both ["Garnet"; "Amethyst"; "Pearl"] ["Pearl"; "Pearl"]) = ["Pearl"]
;; run_test "in_both name in second list appears twice" test

let test () : bool =
  (in_both ["Garnet"; "Pearl"; "Pearl"] ["Pearl"; "Steven"]) = ["Pearl"; "Pearl"]
;; run_test "in_both name appears twice in first list" test

let test () : bool =
(in_both ["Garnet"; "Pearl"; "Garnet"] ["Garnet"; "Steven"; "Pearl"]) = ["Garnet"; "Pearl"; "Garnet"]
;; run_test "in_both two same names in first list" test

(*************************************************************************)
(* Problem 9 (merging lists) *)

(* Write a function that merges two input lists into a single list
   that contains all the elements from both input lists in alternating order:
   the first, third, etc. elements come from the first input list and
   the second, fourth, etc. elements come from the second input list.
  
   The lengths of the two lists may not be the same -- any extra
   elements should appear at the very end of the result. *)

let rec merge (l1: int list) (l2: int list) : int list =
  begin match l1 with
    | [] -> l2
    | x::tl -> x::merge l2 tl
  end 

let test () : bool =
  (merge [1; 3; 5; 7] [2; 4; 6; 8]) = [1; 2; 3; 4; 5; 6; 7; 8]
;; run_test "merge lists same size" test

let test () : bool =
  (merge [] [1; 2; 3]) = [1; 2; 3]
;; run_test "merge one empty list" test

let test () : bool =
  (merge [] []) = []
;; run_test "merge two empty lists" test

let test () : bool =
  (merge [3;5;6;7;8] [1; 2; 3]) = [3;1;5;2;6;3;7;8]
;; run_test "merge 2 different length lists" test


(*************************************************************************)
(* Problem 10 (is_sorted) *)

(* Write a function that determines whether a given list of integers
   is SORTED -- that is, whether the elements appear in ascending
   order. It is okay if the list has repeated elements, so long as
   they are next to each other.
  
   Lists containing zero or one elements are always sorted. *)

let rec is_sorted (l: int list) : bool =
  begin match l with
    | [] -> true
    | x::[] -> true
    | x::y::[] -> x<=y
    | x::y::tl -> if x<=y then is_sorted (y::tl) else false
  end

let test () : bool =
  (is_sorted [1; 2; 3])
;; run_test "is_sorted sorted list" test

let test () : bool =
  not (is_sorted [3; 2; 1])
;; run_test "is_sorted unsorted list" test

let test () : bool =
 (is_sorted [1;1;1;3;5])
;; run_test "is_sorted sorted list with same elements" test

let test () : bool =
 (is_sorted [1;3])
;; run_test "is_sorted 2 elements" test

let test () : bool =
 (is_sorted [1])
;; run_test "is_sorted 1 element" test

let test () : bool =
 not (is_sorted [1;2;1;3])
;; run_test "is_sorted unsorted list" test


(*************************************************************************)
(* Problem 11 (merge_sorted) *)

(* Write a function that takes two sorted lists (in ascending order)
   and yields a merged list that is also sorted and contains all the
   elements from the two input lists. *)

let rec merge_sorted (l1: int list) (l2: int list) : int list =
  begin match (l1, l2) with 
    | ([],[]) -> []
    | ([], y::stl) -> l2
    | (x::ftl, []) -> l1
    | (x::ftl, y::stl) -> if x <= y then x::merge_sorted ftl l2 
      else y::merge_sorted l1 stl 
  end

let test () : bool =
  (merge_sorted [2; 7] [3; 5; 11]) = [2; 3; 5; 7; 11]
;; run_test "merge_sorted lists different size" test

let test () : bool =
  (merge_sorted [1; 2; 3] [4; 5; 6]) = [1; 2; 3; 4; 5; 6]
;; run_test "merge_sorted lists same size" test

let test () : bool =
  (merge_sorted [] [4; 5; 6]) = [4;5;6]
;; run_test "merge_sorted first empty list" test

let test () : bool =
  (merge_sorted [1; 2; 3] []) = [1; 2; 3]
;; run_test "merge_sorted second empty list" test

let test () : bool =
  (merge_sorted [4; 7] [3; 5; 11]) = [3;4;5;7;11]
;; run_test "merge_sorted lists different size and second list is greater" test


(*************************************************************************)
(* Problem 12 (sublist) *)

(* Write a function that takes two integer lists (not necessarily
   sorted) and returns true precisely when the first list is a sublist
   of the second.
  
   The first list may appear anywhere within the second, but its elements
   must appear contiguously.
  
   HINT: You should define and test a helper function that you can use
   in sublist. *)

let rec prefixexists (l1: int list) (l2: int list) : bool =
  begin match (l1,l2) with 
   | ([],[]) -> true
   | ([], y::stl) -> true
   | (x::ftl, []) -> false
   | (x::ftl, y::stl) -> x = y && prefixexists ftl stl
  end

let rec sublist (l1: int list) (l2: int list) : bool =
  begin match (l1,l2) with
    | ([],[]) -> true
    | ([], y::stl) -> true
    | (x::ftl, []) -> false
    | (x::ftl, y::stl) -> if prefixexists l1 l2 then true else sublist l1 stl
  end
let test () : bool =
  (sublist [] [])
;; run_test "sublist two empty lists" test

let test () : bool =
  (sublist [] [1;2;3])
;; run_test "sublist one empty list" test

let test () : bool =
  (sublist [2;3] [1;2;3])
;; run_test "sublist is a sublist" test

let test () : bool =
  not (sublist [2;3] [2;1;3])
;; run_test "sublist elements are not contiguous" test


(*************************************************************************)
(* Problem 13 (rainfall) *)

(* Design and implement a function called `rainfall` that consumes a
   list of ints representing daily rainfall readings. The list may
   contain the number -999 indicating the end of the data of interest.
  
   Produce the average of the non-negative values in the list up to the first
   -999 (if it shows up). There may be negative numbers other
   than -999 in the list, representing faulty readings; these should
   be skipped.  If you cannot compute an average, for whatever reason,
   return -1.  *)

let rec rainfallsum (readings: int list) : int = 
  begin match readings with
    | [] -> 0
    | x::tl -> if x >= 0 then x+rainfallsum tl
      else if x = -999 then 0
      else rainfallsum tl
  end

let rec rainfallcount (readings: int list) : int = 
  begin match readings with
    | [] -> 0
    | x::tl -> if x >= 0 then rainfallcount tl +1
      else if x = -999 then 0
      else rainfallcount tl
  end

let rainfall (readings: int list) : int =
  if rainfallcount readings = 0 then -1
  else rainfallsum readings/rainfallcount readings



(* For example, if we have three readings, then the average rainfall
   is (1 + 2 + 3) / 3 = 6/3 = 2 *)
let test () : bool =
  rainfall [ 1; 2; 3 ] = 2
;; run_test "example" test

(* NOTE: for simplicity, you should only use int operations in this
   problem.  (This may lead to slightly wrong answers, as integer
   division discards the fractional part of its result instead of
   rounding.) *)

let test () : bool =
  rainfall [ 2; 2; 2; 2; 1] = 1
;; run_test "use integer division to calculate average" test

(* HINT: Before you implement this function, make sure that you add
   more test cases. The two tests above do not cover all of the
   situations in the problem description. *)

let test () : bool =
  rainfall [ 2; 2; 2; 2; -999] = 2
;; run_test "rainfall last is -999" test

let test () : bool =
   rainfall [ 2; -999; 2] = 2
;; run_test "rainfall middle is -999" test

let test () : bool =
   rainfall [ -3; 1; 2; -999; 2] = 1
;; run_test "rainfall middle is -999 and negative number in list" test


(*************************************************************************)
(* Kudos Problem (permutations) *)

(* This one is a challenge problem, worth 0 points -- "kudos only." *)

(* A PERMUTATION of a list l is a list that has the same elements as l
   but is not necessarily in the same order.
  
   Write a function that, given a list l, calculates ALL of the
   permutations of l (and returns them as a list). For example,
  
       permutations [1;2;3]
  
   might yield
  
       [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]].
  
   (We say "might yield" here because we haven't specified the
   order of the permutations in the list returned by your function.
   For example, the result
  
       [[1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]; [1;2;3]]
  
   would also be correct.)
  
   Hint: Begin by writing a unit test or two, to make sure you
   understand the problem. (Even though you may need to rewrite them
   if your answer comes out in a different order, the exercise of
   writing them first is useful.) Also, you'll probably want to break
   the problem down into one or more sub-problems, each of which can
   be solved by recursion. *)


(* Note: Do not remove or comment out this function stub, even if you
   choose not to attempt the challenge problem. Your file will not
   compile when you upload it for grading if `permutations` is
   missing. *)

let rec permutations (l: int list) : int list list =
  failwith "permutations: unimplemented"

(* Note that you will also have to think about how to TEST
   permutations, as there may be several correct solutions for each
   input. *)

(* The last part of this file is a print statement. When you see this
   line after running your code, you will know that all of the tests
   in this file have succeeded. *)
;; print_endline "intro.ml: ran to completion"
