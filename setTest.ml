(******************************************************************************)
(* PROBLEM 5: WRITING TEST CASES                                              *)
(******************************************************************************)

(* `SetTest` is a reuseable module that we'll use to test other modules that
   conform to the `SET` interface. When `SetTest` is instantiated with a
   particular set implementation, it will run all of its test cases against
   the set type defined in that implementation.  This means that the _same_
   tests can be used for both the OrderedListSet and BSTSet implementations --
   this makes sense because all implementations of `SET` should behave the same!

   Read through the module, then write your test cases in the space provided
   below. Make sure NOT to test for structural equality with sets.  Instead,
   use the equals function specified in the interface.  Your TAs will be
   grading the completeness of your tests. *)

module SetTest (SetImpl: SetInterface.SET) = struct
  ;; open SetImpl

  (* We first redefine the `run_test` and `run_failing_test` functions so that
     they prepend the name of the set we're testing to the test description. *)

  let run_test desc = Assert.run_test (debug_name ^ ": " ^ desc)
  let run_failing_test desc = Assert.run_failing_test (debug_name ^ ": " ^ desc)

  ;; print_endline ("\n--- Running tests for " ^ debug_name ^ " ---")

  (* Here are a couple of test cases to help get you started... *)

  let test () : bool =
    is_empty empty
  ;; run_test "is_empty: call on empty returns true" test

  (* Note that some tests in this test module (such as the one below) may not
     pass until all the functions they depend on are implemented. For
     instance, the test below will fail for sets whose `set_of_list` function
     is not yet implemented (even if `is_empty` is correct).  This is fine:
     the goal here is just to record all the tests that we expect will pass
     when we get around to implementing everything later. *)

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    not (is_empty s)
  ;; run_test "is_empty: non-empty set returns false" test


(* Now, it's your turn! Make sure to comprehensively test all the other
   functions defined in the `SET` interface. It will probably be helpful to
   have the file `setInterface.ml` open as you work.  Your tests should stress
   the abstract properties of what it means to be a set, as well as the
   relationships among the operations provided by the SET interface.  Your
   tests should not assume that the underlying data structure was implemented
   in a certain way. For instance, your tests should not assume that the
   underlying implementation behind a set is a list/tree.  Additionally, if
   you are testing a function that relies on a helper/another function, first
   test the helper with a particular input to ensure that the helper works,
   then test the primary function with the same input used in the helper.

   We strongly advise that you write tests for the functions in the order they
   appear in the interface. Write tests for all of the functions here before
   you start implementing. After the tests are written, you should be able to
   implement the functions one at a time in the same order and see your tests
   incrementally pass.

   Your TAs will be manually grading the completeness of your test cases. *)

  (* ---------- Write your own test cases below. ---------- *)
  let test () : bool = 
    list_of_set empty = []
  ;; run_test"list_of_set: empty set returns true"
  
  let test () : bool =
    let s = set_of_list [1; 2; 3; 4; 5] in
    list_of_set s = [1; 2; 3; 4; 5]
  ;; run_test "list_of_set: non-empty set returns true" test

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    let u = add 4 s in
    equals u (set_of_list [1; 2; 3; 4])
  ;; run_test "add: non-empty set with distinct add number is true" test

 let test () : bool =
    let s = set_of_list [1; 2; 4] in
    let u = add 4 s in
    equals u (set_of_list [1; 2; 4])
  ;; run_test "add: non-empty set with same add number is true" test

 let test () : bool =
    let s = set_of_list [] in
    let u = add 2 s in
    equals u (set_of_list [2])
  ;; run_test "add: adding an element ot an empty set" test

 let test () : bool =
    let s = empty in
    let u = add 2 s in
    equals u (set_of_list[2])
  ;; run_test "add: adding an element ot an empty set" test

 let test () : bool =
    let s = empty in
    let u = remove 4 s in
    equals u s
  ;; run_test "remove: removing an element from an empty set" test

let test () : bool =
    let s = set_of_list [1; 2; 3; 4] in
    let u = remove 4 s in
    equals u (set_of_list [1; 2; 3])
  ;; run_test "remove: removing an element from a non-empty set" test

let test () : bool =
    let s = set_of_list [1; 2; 3] in
    let u = remove 4 s in
    equals u (set_of_list [1; 2; 3])
  ;; run_test "remove: removing a non-inclusive 
    element from a non-empty set" test

let test () : bool =
    let s = empty in
    not (member 100 s)
  ;; run_test "member: not a member in an empty set" test

let test () : bool =
    let s = set_of_list [1;2;3] in
    not (member 100 s)
  ;; run_test "member: not a member in a non-empty set" test

let test () : bool =
    let s = set_of_list[1;2;3] in
    member 2 s
  ;; run_test "member: is a member of non-empty set" test

let test () : bool =
    let s = empty in
    size s = 0
  ;; run_test "size: size of empty set" test

let test () : bool =
    let s = set_of_list [1;2;3] in
    size s = 3
  ;; run_test "size: size of non-empty set" test

let test () : bool =
    let s = set_of_list [1;2;3] in
    let u = set_of_list [1;2;3] in
    equals s u
  ;; run_test "equals: 2 sets are equal" test

let test () : bool =
    let s = set_of_list [3;2;1] in
    let u = set_of_list [1;2;3] in
    equals s u
  ;; run_test "equals: 2 unequal listss are equal" test

let test () : bool =
    let s = set_of_list [1;2;3] in
    let u = set_of_list [1;2;3;4] in
    not(equals s u)
  ;; run_test "equals: 2 sets are unequal" test

let test () : bool =
    let s = empty in
    let u = empty in
    equals s u
  ;; run_test "equals: 2 empty sets are equal" test

let test () : bool =
    let s = empty in
    let u = set_of_list [] in
    equals s u
  ;; run_test "equals: set of empty list and empty are equal" test

let test () : bool = 
  let s = set_of_list [1;2;3] in
  let u = add 1( add 2( add 3 empty)) in
  equals s u
 ;; run_test "set_of_list: making a set from non empty list 
  && out of order" test

let test () : bool = 
  let s = set_of_list [] in
  equals s empty
 ;; run_test "set_of_list: making a set from non empty list" test
(* above: very much like the equals of empty list and empty set *)

let test () : bool = 
  let s = set_of_list [1;2;3;1;2;2;2;2;3;1;2;3;3;3;3] in
  let u = add 1( add 2( add 3 empty)) in
  equals s u
 ;; run_test "set_of_list: set and list with duplicates" test



  (* ---------- Write your own test cases above. ---------- *)

end

(* We now instantiate the tests so they are executed for both OrderedListSet
   and BSTSet.  Don't modify anything below this comment. *)

module TestOrderedListSet = SetTest(ListSet.OrderedListSet)
;; print_newline ()

module TestBSTSet = SetTest(TreeSet.BSTSet)
;; print_newline ()
