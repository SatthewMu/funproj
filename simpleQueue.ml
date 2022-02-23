(******************************************************************************)
(* Problem 5: Simple Queues ***************************************************)
(******************************************************************************)

;; open Assert
;; open Imp

(* This problem takes a very simple approach to implementing the queue data
   structure.  We define the type 'a queue to be a record with single mutable
   field containing a 'a list. To conceptualize this, when items 
   are added to the queue, they are added to the list. *)



module SimpleQueue : QueueInterface.QUEUE = struct

  type 'a queue = { mutable data : 'a list }

   (* Note: you are allowed to use higher order functions 
      (e.g. transform/fold) and List library functions. *)

  (* All SimpleQueues are valid *)
  let valid (q: 'a queue) : bool =
    true

  (* Create an empty SimpleQueue *)
  let create () : 'a queue =
    { data = [] }

  (* Determine whether a queue is empty *)
  let is_empty (q: 'a queue) : bool =
    q.data = []

  (* Add an element to the tail of a queue *)
  let enq (elt: 'a) (q: 'a queue) : unit =
    begin match q.data with  
      | [] -> q.data <- [elt]
      | hd :: tl -> q.data <- q.data @ [elt]
    end

  (* Remove an element from the head of the queue and return the removed head *)
  let deq (q: 'a queue) : 'a =
    begin match q.data with
    | [] -> failwith "deq called on empty queue"
    | hd :: tl -> q.data <- tl; hd
    end

  (* Retrieve the list of values stored in the queue in the order they appear.
   Be sure to leverage the implementation invariant of this queue type! *)
  let to_list (q: 'a queue) : 'a list =
    q.data

  (* Get the length of a queue. *)
  let length (q: 'a queue) : int =
    List.length q.data

  (* Print out the data of a queue *)
  let rec print (q: 'a queue) (string_of_element: 'a -> string) : unit =
    print_endline "--- queue contents ---";
    List.iter (fun x -> print_endline (string_of_element x)) q.data;
    print_endline "--- end of queue ---"

  (* Convert a list into a queue; be sure to leverage the implementation
   invariant of this queue type! *)
  let from_list (l: 'a list) : 'a queue =
    let q = create() in
    q.data <- l; q

  (* Determine whether a given element is in the queue. Note that we
   test the elements for equality using reference equality (==). *)
  let contains (elt: 'a) (q: 'a queue) : bool =
    fold (fun v acc -> v == elt || acc) false q.data

  (* Truncate a queue after the first occurrence of a specified
   element. (i.e., remove all elements from the queue that follow the
   first occurrence of the given element). Remember to use reference
   equality. (We want to use reference equality to ensure that the
   element "elt" refers to the same item on the heap as the element
   you are looking for, and isn't simply just a copy.) *)
  let truncate (elt: 'a) (q: 'a queue) : unit =
    let rec listtruncate(l: 'a list) : 'a list = 
    begin match l with
      | [] -> []
      | hd :: tl -> if hd == elt then [hd] else hd::listtruncate tl
    end in
    q.data <- listtruncate q.data
  
  (* Delete all instances of the value elt from the queue q. *)
  let delete (elt: 'a) (q: 'a queue) : unit =
    q.data <- fold (fun v acc -> if v = elt then acc else v :: acc) [] q.data

  (* A SimpleQueue can't have a cycle. It's a list! *)
  let has_cycle (q: 'a queue) : bool =
    false

  (* DO NOT MODIFY *)
  let debug_name = "SimpleQueue"

end
