(*****************************************************************************)
(* PROBLEM 7: WRITING TEST CASES                                             *)
(*****************************************************************************)

;; open Assert
;; open Deque

(* `DequeTest` is used to test the deque implementation from deque.ml. 

   Read through the module, then write your test cases in the space
   provided below.  Your TAs will be grading the completeness of your
   tests.  *)

;; print_endline ("\n--- Running tests for Deque ---")

(* Here is a test to help get you started. *)

let test () : bool =
  is_empty (create ())
;; run_test "is_empty: call on empty returns true" test


(* Now, it's your turn! Make sure to comprehensively test all the other
   functions you implemented in deque.ml. It will probably be helpful to
   have the files deque.ml/mli open as you work.

   We provide many test cases for you, so your job here is to finish writing
   tests for `remove_head`, `remove_tail`, `delete_last`, `delete_first`, and
   `reverse`.

   Your TAs will be manually grading the completeness of your test cases.

   Note: Remember the difference between structural and reference
   equality; think about why you shouldn't be directly comparing 
   deques with the '=' of structural equality. *)

(* ---------- Write your own test cases below. ---------- *)
(* INSERT_HEAD TESTS *)
let test () : bool =
  let d = create () in
  insert_head 1 d;
  valid d && peek_head d = 1 && peek_tail d = 1
;; run_test "insert_head into empty" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  valid d && peek_head d = 2 && peek_tail d = 1
;; run_test "insert_head into singleton" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  valid d && peek_head d = 3 && peek_tail d = 1
;; run_test "insert_head into non-empty, multi-element" test

(* INSERT_TAIL TESTS *)
let test () : bool =
  let d = create () in
  insert_tail 1 d;
  valid d && peek_head d = 1 && peek_tail d = 1
;; run_test "insert_tail into empty" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  valid d && peek_head d = 1 && peek_tail d = 2
;; run_test "insert_tail into singleton" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  valid d && peek_head d = 1 && peek_tail d = 3
;; run_test "insert_tail into non-empty, multi-element" test

(* REMOVE_HEAD *)
let test () : bool =
  let d = create () in
  insert_head 1 d;
  remove_head d = 1 && is_empty d
;; run_test "remove_head singleton" test

let test () : bool =
  let d = create () in
  remove_head d = 1
;; run_failing_test "remove_head empty list" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  remove_head d = 2 && peek_head d = 1 && peek_tail d = 1
;; run_test "remove_head two elements" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  remove_head d = 3 && peek_head d = 2 && peek_tail d = 1
;; run_test "remove_head multiple elements" test



(* REMOVE_TAIL *)
let test () : bool =
  let d = create () in
  insert_tail 1 d;
  remove_tail d = 1
;; run_test "remove_tail singleton" test

let test () : bool =
  let d = create () in
  remove_tail d = 1
;; run_failing_test "remove_tail empty list" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  remove_tail d = 2 && peek_head d = 1 && peek_tail d = 1
;; run_test "remove_tail two elements" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  remove_tail d = 3 && peek_head d = 1 && peek_tail d = 2
;; run_test "remove_tail multiple elements" test


(* DELETE_LAST *)
let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  delete_last 3 d;
  peek_head d = 1 && peek_tail d = 2; 
;; run_test "delete_last into non-empty, multi-element" test

let test () : bool =
  let d = create () in
  delete_last 1 d;
  is_empty d
;; run_test "delete_last empty" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  delete_last 2 d;
  peek_head d = 1 && peek_tail d = 1
;; run_test "delete_last value DNE" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  delete_last 1 d;
  is_empty d
;; run_test "delete_last element exists" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  delete_last 2 d;
  peek_head d = 3 && peek_tail d = 1
;; run_test "delete_last multiple values" test

let test () : bool =
  let d = create () in
  insert_head 3 d;
  insert_head 2 d;
  insert_head 1 d;
  delete_last 1 d;
  peek_head d = 2 && peek_tail d = 3
;; run_test "delete_last multi-element list, deleted element is first" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  delete_last 1 d;
  peek_head d = 3 && peek_tail d = 2
;; run_test "delete_last multi-element list, deleted element is last" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 1 d;
  insert_head 3 d;
  delete_last 1 d;
  to_list d = [3; 1; 2]
;; run_test "delete_last multi-element list, deleted element exists 
   more than once" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  delete_last 3 d;
  peek_head d = 2 && peek_tail d = 1
;; run_test "delete_last multi-element list,
   deleted element doesn't exist " test

(* DELETE_FIRST *)
let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  delete_first 3 d;
  peek_head d = 1 && peek_tail d = 2; 
;; run_test "delete_first into non-empty, multi-element" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  delete_first 3 d;
  peek_head d = 1 && peek_tail d = 2; 
;; run_test "delete_first into non-empty, multi-element" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  delete_first 1 d;
  peek_head d = 1 && peek_tail d = 1; 
;; run_failing_test "delete_first singleton" test

let test () : bool =
  let d = create () in
  delete_first 1 d;
  is_empty d
;; run_test "delete_first empty" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  delete_first 2 d;
  peek_head d = 1 && peek_tail d = 1
;; run_test "delete_first value DNE" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  delete_first 1 d;
  is_empty d
;; run_test "delete_first element exists" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  delete_first 2 d;
  peek_head d = 3 && peek_tail d = 1
;; run_test "delete_last multiple values" test

let test () : bool =
  let d = create () in
  insert_head 3 d;
  insert_head 2 d;
  insert_head 1 d;
  delete_first 1 d;
  peek_head d = 2 && peek_tail d = 3
;; run_test "delete_last multi-element list, deleted element is first" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  delete_first 1 d;
  peek_head d = 3 && peek_tail d = 2
;; run_test "delete_last multi-element list, deleted element is last" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 1 d;
  insert_head 3 d;
  delete_first 1 d;
  to_list d = [3; 2; 1]
;; run_test "delete_first deleted element exists 
   more than once" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  delete_first 3 d;
  peek_head d = 2 && peek_tail d = 1
;; run_test "delete_last element doesn't exist " test




(* REVERSE *)
let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  reverse d;
  peek_head d = 1 && peek_tail d = 3; 
;; run_test "reverse mutliple elements" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  reverse d;
  peek_head d = 1 && peek_tail d = 1; 
;; run_test "reverse single element" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d; 
  reverse d;
  peek_head d = 1 && peek_tail d = 2;
;; run_test "reverse two elements" test

let test () : bool =
  let d = create () in
  reverse d;
  peek_head d = 1 && peek_tail d = 1; 
;; run_failing_test "reverse:failing test through empty deque" test



(* ---------- Write your own test cases above. ---------- *)
