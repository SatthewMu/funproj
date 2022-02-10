;; open Assert
;; open HigherOrder

(******************************************************************************)
(* PROBLEM 6: ORDERED LIST SET                                                *)
(******************************************************************************)

(* We will first implement the `SET` interface using a simple list
   representation. Because a set is an "abstract type", it does not
   have a concrete implementation built into OCaml. Instead, it is
   defined in terms of its behavior, which is determined by the methods listed
   in the interface (the .mli files). Whether we use lists or trees, any 
   implementation that conforms to the interface and maintains the invariants, 
   or desired properties, is a set.

   The OCaml module system allows us to hide some information about
   how our set is represented under the hood, and gives us the
   opportunity to control how values of type `'a set` can be
   created. This allows us to maintain "representation invariants" (as
   discussed in the lecture) so our lists behave like sets. *)

module OrderedListSet : SetInterface.SET = struct

  (* We now inform OCaml that the abstract type 'a set is, behind the scenes,
     actually a `'a list`. This information is not available to any clients of
     the `SET` interface, because it is an implementation detail.

     NOTE: We are going to maintain the additional invariants that this list
     is sorted in ascending order, and contains no duplicates. *)

  type 'a set = 'a list

  let empty : 'a set = []

  let is_empty (s: 'a set) : bool =
    s = empty

  (* This function should return a list which is sorted in ascending order and
     contains no duplicate elements. We will be automatically testing that you
     are leveraging the invariants to write this as efficiently as possible. *)
  let list_of_set (s: 'a set) : 'a list =
    s

  (* This function will be a bit more complex than just cons-ing the element
     onto the list. Remember, we're maintaining the invariants that this list
     is sorted and contains no duplicate elements. You may NOT use a sorting
     function (such as `List.sort`) to accomplish this. *)
  let rec add (x: 'a) (s: 'a set) : 'a set =
    begin match s with
      | [] -> [x]
      | hd :: tl -> if x < hd then x::s else if x > hd then hd::(add x tl) 
        else s
    end

  (* The `remove` function returns a set without `x`. Try to use the invariants
     in order to "short-circuit" (terminate earlier than the end of the list) if
     the element is not found in its expected position. We will be automatically
     testing that this function short-circuits. *)
  let rec remove (x: 'a) (s: 'a set) : 'a set =
    begin match s with
      | [] -> empty
      | hd :: tl -> if x = hd then tl else if x > hd then hd::(remove x tl) 
        else s
    end

  (* You already implemented a generic version of this function in
     problem 1, but now write it using `fold`. Do not add the `rec`
     keyword. *)
  let member (x: 'a) (s: 'a set) : bool =
    fold(fun t acc -> t = x || acc) false s

  (* Because one of our representation invariants requires that our
     list contain no duplicate elements, it is much easier to
     calculate the size of the set.  Implement this function using
     `fold`. You may not add the `rec` keyword. *)
  let size (s: 'a set) : int =
    fold(fun x acc -> 1 + acc) 0 s

  (* We will be automatically testing that your implementation of `equals`
     makes use of the invariants to ensure efficiency. *)
  let equals (s: 'a set) (t: 'a set) : bool =
    s = t


  (* Remember that, because it accepts an arbitrary input of type `'a
     list`, this function will have to ensure that the resulting list
     follows the invariants. You should write some tests to ensure
     this is the case! *)

  let set_of_list (l: 'a list) : 'a set =
    fold(fun x acc -> add x acc) [] l

  (* Don't modify this: It's here for testing purposes. *)
  let debug_name: string = "OrderedListSet"
end

