;; open Assert
;; open HigherOrder

(******************************************************************************)
(* PROBLEM 8 : USING SETS FOR TEXT PROCESSING                                 *)
(******************************************************************************)

(* This file contains a string list called `sat_vocab`, which contains
   496 common SAT vocabulary words. The file books.ml contains three
   texts which have been parsed into OCaml string lists. Your job is
   to use your set implementation to answer some questions about each
   of these three texts.

   You will design and then run a program that calculates various
   statistics about the _unique_ words in each text -- that is, we
   aren't interested in the number of times a word occurs in a text,
   but just _whether_ it occurs. Therefore, sets are a good candidate
   for this job.

   The texts in question are "A Modest Proposal" by Jonathan Swift,
   "The Things that Make Me Weak and Strange Get Engineered Away" by
   Cory Doctorow, and "Twenty Thousand Leagues Under the Sea", by
   Jules Verne. *)


(* First, we make the BST implementation of sets available. (This will also run
   any tests you have written inside the BSTSet module.) *)

;; open TreeSet.BSTSet

(* The string lists for each text are defined in a separate file books.ml
   (because they are huge). They are bound to the names `modest`, `weak`, and
   `twentyk`, respectively. Here we pair each of them with a string that helps
   identify them for printing purposes. *)

;; open Books
let texts_as_lists: (string * string list) list =
  [("Modest ", modest);
   ("Weak   ", weak);
   ("TwentyK", twentyk);]


(* It's useful to start by processing these texts into sets (rather than
   lists) of words.  Replace `[]` below with an expression that transforms
   `text_as_lists` into a list of names and sets of words. This function 
   takes in the list of (name, list) tuples and converts it to a list of
   (name, set) tuples. (Hint: use the `transform` function from `HigherOrder`
   and the `SET` interface.) *)

let texts: (string * string set) list =
transform (fun (x,y) -> (x,set_of_list y)) texts_as_lists

(* Here's an OCaml vocab list... *)
let ocaml_vocab : string list = ["set"; "abstraction"; "abstract"; "type";
  "static"; "strongly"; "typecheck"; "function"; "generic"; "higher"; "order";
  "tree"; "binary"; "search"; "testing"; "module"; "interface"; "datatype";
  "implementation"; "functional"; "programming"; "compiler"; "value";
  "oriented"; "immutable"; "transform"; "filter"; "fold"; "recursion"]

(* ... and a longer SAT vocab list. *)
let sat_vocab : string list = ["induce"; "mawkish"; "reprieve"; "indigent";
  "arrogate"; "coalesce"; "modulate"; "meticulous"; "quixotic"; "pretense";
  "notorious"; "sycophant"; "empirical"; "disperse"; "paramount"; "allocate";
  "scintillating"; "insurgent"; "pithy"; "hapless"; "indolent"; "corrosive";
  "turgid"; "haughty"; "adverse"; "pulchritude"; "subjugate"; "mendacious";
  "vocation"; "expedient"; "construe"; "adamant"; "consolation"; "ascetic";
  "imperious"; "abase"; "licentious"; "pervasive"; "propagate"; "disgruntled";
  "daunting"; "consensus"; "implement"; "hardy"; "parody"; "hackneyed";
  "auspicious"; "heterogeneous"; "analgesic"; "refurbish"; "audible"; "forum";
  "edict"; "ameliorate"; "enthrall"; "bias"; "liability"; "maudlin"; "grievous";
  "convention"; "irreverence"; "adhere"; "aisle"; "conciliatory"; "salutation";
  "credulity"; "censure"; "assiduous"; "rail"; "antipathy"; "burnish";
  "incisive"; "evince"; "paucity"; "remiss"; "frenetic"; "conflagration";
  "destitute"; "penitent"; "precipice"; "latent"; "enfranchise"; "corpulence";
  "exhort"; "tremulous"; "indefatigable"; "discern"; "protean"; "immerse";
  "denounce"; "juxtaposition"; "devious"; "sophomoric"; "comprehensive";
  "lenient"; "compliant"; "idiosyncratic"; "gratuitous"; "diminutive";
  "antediluvian"; "discursive"; "laconic"; "nocturnal"; "exalt"; "reciprocate";
  "bereft"; "acquiesce"; "impinge"; "constituent"; "captivate"; "wistful";
  "irascible"; "demarcation"; "reputable"; "facade"; "vehemently"; "vex";
  "derivative"; "defer"; "annex"; "sensual"; "philanthropic"; "tome";
  "sobriety"; "venerable"; "obstinate"; "rancor"; "accessible"; "rescind";
  "amorphous"; "enigmatic"; "brusque"; "debauch"; "dogmatic"; "abnegation";
  "dissonance"; "plethora"; "morose"; "infusion"; "dispatch"; "cloying";
  "stagnate"; "conundrum"; "debacle"; "inextricable"; "resilient"; "flaccid";
  "feral"; "beguile"; "morass"; "serendipity"; "forlorn"; "covet"; "abridge";
  "increment"; "impassive"; "cordial"; "innocuous"; "criteria"; "euphoric";
  "prescribe"; "accretion"; "defile"; "insidious"; "archaic"; "prosaic";
  "plausible"; "stingy"; "antagonism"; "ubiquitous"; "collusion"; "terrestrial";
  "anguish"; "presage"; "arboreal"; "utopia"; "aggrieved"; "pellucid"; "dour";
  "obsequious"; "vapid"; "canvas"; "exonerate"; "deface"; "veracity";
  "resplendent"; "intractable"; "revoke"; "adorn"; "epistolary"; "meager";
  "commensurate"; "impecunious"; "esoteric"; "pathology"; "decorous";
  "apocryphal"; "zephyr"; "mutable"; "saccharine"; "raze"; "instigate"; "atone";
  "perfunctory"; "paradox"; "surmise"; "renown"; "wily"; "precocious";
  "moderate"; "artisan"; "taciturn"; "distend"; "abstain"; "wanton";
  "choreography"; "elicit"; "candor"; "egregious"; "quell"; "ignominious";
  "profane"; "conformist"; "implicit"; "duplicity"; "panacea"; "bane";
  "disaffected"; "tractable"; "aberration"; "goad"; "catalyze"; "larceny";
  "platitude"; "calumny"; "attain"; "avarice"; "palliate"; "cumulative";
  "combustion"; "putrid"; "eclectic"; "transmute"; "aggrandize"; "vindictive";
  "defunct"; "negligent"; "aerial"; "noxious"; "usurp"; "intrepid";
  "circumlocution"; "congregation"; "interject"; "fathom"; "florid";
  "anarchist"; "forbearance"; "repose"; "recalcitrant"; "linchpin"; "orthodox";
  "penurious"; "tirade"; "abdicate"; "cacophony"; "amicable"; "coerce";
  "maelstrom"; "wrath"; "profuse"; "confection"; "emote"; "dynamic";
  "meritorious"; "bequeath"; "adulation"; "nonchalant"; "counteract";
  "propitious"; "eminent"; "fickle"; "dilatory"; "uncanny"; "circumspect";
  "repudiate"; "malleable"; "accolade"; "pertinacious"; "interminable";
  "anxiety"; "dissent"; "cajole"; "vilify"; "complacency"; "hypothetical";
  "pillage"; "collateral"; "ostensible"; "ballad"; "figurative"; "embezzle";
  "elucidate"; "fetid"; "effervescent"; "resolve"; "incarnate"; "luminous";
  "discrepancy"; "superfluous"; "abscond"; "wallow"; "consumption"; "condone";
  "verdant"; "rash"; "affable"; "bashful"; "transgress"; "aversion"; "mundane";
  "compunction"; "garish"; "fallacious"; "acumen"; "atypical"; "jubilant";
  "malediction"; "extant"; "demure"; "litigant"; "hegemony"; "submissive";
  "regurgitate"; "desiccated"; "clemency"; "contrite"; "perspicacity";
  "clandestine"; "incessant"; "redoubtable"; "viscous"; "exult"; "astute";
  "divulge"; "disparage"; "affluent"; "trepidation"; "emaciated"; "stolid";
  "potentate"; "excursion"; "frugal"; "congeal"; "nebulous"; "compelling";
  "assail"; "accede"; "appropriate"; "chronological"; "expunge"; "connive";
  "concord"; "depravity"; "caustic"; "nadir"; "quaint"; "amenity"; "reprobate";
  "dissemble"; "ethereal"; "lavish"; "cupidity"; "cherish"; "alias";
  "vicissitude"; "abject"; "equanimity"; "repentant"; "compliment"; "carp";
  "chaos"; "derelict"; "blandish"; "arable"; "relish"; "blight"; "poignant";
  "fractious"; "torrid"; "vacuous"; "portent"; "concoct"; "obtuse";
  "scurrilous"; "sagacity"; "inane"; "calibrate"; "agnostic"; "innuendo";
  "grandiloquence"; "revel"; "curt"; "undulate"; "puerile"; "surrogate";
  "ruminate"; "bombastic"; "insipid"; "benevolent"; "arbitrary"; "appease";
  "pungent"; "strident"; "partisan"; "incontrovertible"; "anecdote"; "tenable";
  "divine"; "reconcile"; "inept"; "inquisitor"; "ambiguous"; "speculative";
  "fortitude"; "impervious"; "palatable"; "truculent"; "preponderance";
  "diffident"; "prudence"; "satiate"; "proclivity"; "restive"; "nurture";
  "allege"; "ardor"; "ingenuous"; "altercation"; "diaphanous"; "abhor";
  "cognizant"; "delineate"; "obfuscate"; "inveterate"; "servile"; "clairvoyant";
  "inure"; "inimical"; "effrontery"; "ostracism"; "injunction"; "nomadic";
  "accord"; "manifest"; "excavate"; "oblivious"; "enamor"; "semaphore";
  "capitulate"; "privation"; "culpable"; "onerous"; "elated"; "placate";
  "fecund"; "ribald"; "officious"; "ennui"; "solvent"; "extraneous";
  "aspersion"; "apprehensive"; "foil"; "encumber"; "genial"; "sanctimonious";
  "antiseptic"; "pernicious"; "impudent"; "anomaly"; "temerity"; "oration";
  "discomfit"; "kudos"; "despondent"; "bourgeois"; "convoluted"; "deleterious";
  "tantamount"; "solipsistic"; "vituperate"; "erudite"; "contentious";
  "zealous"; "hierarchy"]


(* You'll next write some helper functions that allow us to more easily
   compute statistics about the texts.

   To begin, write a generic, higher-order function that, given a predicate
   function as input, counts the number of elements of a list that match the
   given predicate.

   (Hint: The functions `list_length` and `filter` from higherOrder.ml may
   come in handy here!) *)

let count_if (pred: 'a -> bool) (l: 'a list) : int =
   list_length(filter pred l) 


(* Now use your `count_if` function to determine the number of words in the
   vocab list that appear in a given text. Note that we represent the vocab
   words as a _list_ so we can recurse over it quickly, while we represent the
   words occurring in the text as a _set_, so we can check membership
   efficiently.  You should not need to convert `vocabulary` to a set or
   convert `text` to a list. *)

let count_words_in_text (vocabulary: string list) (text: string set) : int =
   count_if (fun x -> member x text) vocabulary
(* Recall that we can partially apply our `count_words_in_text` function to
   just some of its arguments, to return another function.

   Now we'll partially apply `count_words_in_text` to the `ocaml_vocab` list
   to create a new function that counts OCaml words. By _partially_ applying
   `count_words_in_text`, we can leave out the last string set argument and
   return a new function that takes in a set and returns an int, bound to the
   name `count_ocaml_words`. *)

let count_ocaml_words: string set -> int =
  count_words_in_text ocaml_vocab

(* Now it's your turn.

   Write a function that counts the number of words that appear in the
   `sat_vocab` list, using partial application in the same way. (Make sure to
   remove the `fun _ ->` so that your solution has the same form as the
   example just above.) *)

let count_sat_words: string set -> int =
  count_words_in_text sat_vocab

(* Before moving on, write several small tests (with answers you can easily
   calculate by hand) to make sure that count_sat_words is working when called
   on an empty set, on a set containing just one or two from the sat_vocab
   list, on a nonempty set containing no words at all from the sat_vocab list,
   etc. *)

let test () : bool =
   let a = add "apple" empty in
   count_sat_words a = 0
   ;; run_test "no sat_vocab in list" test

let test () : bool =
   let a = empty in
   count_sat_words a = 0
   ;; run_test "sat_vocab call on empty" test

let test () : bool =
   let a = add "strident" ( add "inept" empty) in
   count_sat_words a = 2
   ;; run_test "sat_vocab call on 2 words in list" test
   
(* It will be useful to be able to print out statistics about the three texts,
   so we need a way to convert vocab count information to a table suitable for
   display.  Here, we provide you a function that takes a `title` string and a
   `counter` function that calculates the count for a given text. It outputs a
   string representation of the table. Note how it uses `transform` and
   `String.concat` (which is just a library function that concatenates strings
   with a given separator). *)

let string_of_counts (title: string) (counter: string set -> int) : string =
  ("----- " ^ title ^ " -----\n") ^
    String.concat "\n" (transform
      (fun (name, s) -> name ^ (string_of_int (counter s)))
      texts)

(* We've already converted `count_words_in_text` from a two-argument
   function to a one-argument function, which is the proper type for
   `string_of_counts`. Uncomment the line below once you have implemented
   `count_sat_words` to print out the answers for each text. Re-comment it
   _before_ you submit! (Very slow implementations of your set operations
   might cause the grading server to time out. *)

(* ;; print_endline (string_of_counts "SAT WORDS" count_sat_words) *)

(* Replace the 0s below with the number of SAT words that appear in each of
   the texts (the answers will be printed to the console when you run
   vocab.exe). You must replace these with the correct answers to receive
   credit for this component of the assignment. *)

let modest_sat_words  : int = 7
let weak_sat_words    : int = 14
let twentyk_sat_words : int = 31


(* Now use `count_if` to determine the number of words in the given set of
   strings that are of length greater than 7. You may use `String.length` here
   (documentation for OCaml's String module is available at
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html). *)

let count_length_gt7 (s: string set) : int =
   count_if (fun x -> String.length x > 7) (list_of_set s)

(* Uncomment the function call below once you have implemented
   `count_length_gt7` to print out the answers for each text. Re-comment it
   _before_ you submit! *)

(*;; print_endline (string_of_counts "LENGTH > 7" count_length_gt7) *)

(* Replace the 0's below with the number of words of length greater than 7 that
   appear in each of the texts. *)

let modest_length_gt7  : int = 331
let weak_length_gt7    : int = 1009
let twentyk_length_gt7 : int = 1920


(******************************************************************************)
(* The rest of this file is KUDOS ONLY, but you are encouraged to give
   it a try.  *)

(* The real power of higher-order programming shows when we combine many small
   operationsf to compute interesting results.  Let's break down the coarse
   vocabulary count above by creating a table that contains, for each book, a
   count of the number of `sat_vocab` words starting with each letter of the
   alphabet. We'll call it the `frequency_table`.  he first few rows of this
   table are shown below:

        Modest Weak   TwentyK
     a: 0      4      7
     b: 0      1      0
     c: 2      2      1         *)

(* We already have almost everything we need. First, let's create a helper
   function that determines whether a string starts with a given character. *)

let starts_with (c: char) (s: string) : bool =
  String.get s 0 = c

(* Using `starts_with`, judicious partial application, and the `filter`
   function from higherOrder.ml, create a function that counts the number of
   words which are in both an inputted string set and `sat_vocab` and start
   with a given character `c`. Your solution should be only one line 
   long. *)

let count_sat_vocab_starting_with (c:char) : string set -> int =
  failwith "KUDOS count_sat_vocab_starting_with: unimplemented"

(* Here is a list containing each letter of the alphabet as a character *)

let abcs: char list = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k';
  'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']

(* The frequency table is an association list that associates each character
   of the alphabet to a list of counts (each list will have three elements,
   one for each text).  Build the table by nested uses of
   `HigherOrder.transform` and suitable anonymous functions.

   Hints: - the outer `transform` needs to process the `abcs`
          - the inner `transform` needs to `count_sat_vocab_starting_with` *)

let frequency_table: (char * int list) list =
  [] (* TODO: define this *)

(* Finally, let's print out the table as a string. *)

let table_as_string: string =
  let header = "   " ^
    (String.concat "" (transform (fun (name, _) -> name) texts))
    ^ "\n"
  in
  let row (c, l) =
    (Char.escaped c) ^ ": " ^
    (String.concat "      " (transform string_of_int l))
  in
  header ^
  (String.concat "\n" (transform row frequency_table))

(* Uncomment the line below to print out the frequency table.
   Re-comment before you submit. *)

(* ;; print_endline table_as_string *)
