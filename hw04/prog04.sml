(* 
 * Name: 
 * Time spent on assignment: 
 * Collaborators: 
 *)

(* **************************************** *)
(* **************************************** *)

exception Unimplemented of string

fun fst (x, _) = x

fun snd (_, y) = y

fun foldl f b l =
  case l of
     [] => b
   | h::t => foldl f (f h b) t

fun foldr f b l =
  case l of
     [] => b
   | h::t => f h (foldr f b t)

(* **************************************** *)
(* **************************************** *)


(* Part A (unzip/zip) *)

(* A.a *)
(* DEFINE unzip HERE *)
fun unzip l = 
    case l
        of nil => (nil, nil)
        | (a,b)::t1 =>
            let 
              val (l1, l2) = unzip t1
            in 
              (a::l1, b::l2) 
            end

(* A.b *)
(* DEFINE zip HERE *)


(* **************************************** *)
(* **************************************** *)


(* Part B *)

(* DEFINE compound HERE *)


(* **************************************** *)
(* **************************************** *)


(* Part C *)

(* DEFINE exp HERE *)


(* **************************************** *)
(* **************************************** *)


(* Part D *)

(* DEFINE existsUnique HERE *)


(* **************************************** *)
(* **************************************** *)


(* Part E *)

(* DEFINE allAlt HERE *)


(* **************************************** *)
(* **************************************** *)


(* Part F *)
(* DEFINE separate HERE *)
fun separate (k: int, x: 'a, l: 'a list) : 'a list =
   raise Unimplemented "separate"

fun onesSeparate i =
   implode (separate (1, #",", explode (Int.toString i)))
fun tensSeparate i =
   implode (separate (2, #",", explode (Int.toString i)))
fun thousandsSeparate i =
   implode (separate (3, #",", explode (Int.toString i)))

(* **************************************** *)
(* **************************************** *)

type name = string
fun nameCompare (n1: name, n2: name) : order = String.compare (n1, n2)
fun nameEqual (n1: name, n2: name) : bool =
   case nameCompare (n1, n2) of EQUAL => true | _ => false
exception NotFound of name

(* list representation for environments *)
type 'a lenv = (name * 'a) list
val lenvEmpty = []
fun lenvFind (name, rho) =
   case rho of
      [] => raise NotFound name
    | (n, d)::tail =>
         if nameEqual (name, n) then d else lenvFind (name, tail)
fun lenvBind (name, data, rho) = (name, data) :: rho


(* **************************************** *)
(* **************************************** *)


(* Part G (binary search tree representation for environments) *)

datatype 'a btree = Leaf | Node of 'a btree * 'a * 'a btree

fun btreeInsert cmp =
   let
      fun ins (x, btree) =
         case btree of
            Leaf => Node (Leaf, x, Leaf)
          | Node (lt, y, rt) =>
               (case cmp (x, y) of
                   LESS => Node (ins (x, lt), y, rt)
                 | EQUAL => Node (lt, x, rt)
                 | GREATER => Node (lt, y, ins (x, rt)))
   in
      ins
   end
val _ : ('a * 'a -> order) -> ('a * 'a btree) -> 'a btree = btreeInsert

fun btreeLookup cmp =
   let
      fun lkup (x, btree) =
         case btree of
            Leaf => NONE
          | Node (lt, y, rt) =>
               (case cmp (x, y) of
                   LESS => lkup (x, lt)
                 | EQUAL => SOME y
                 | GREATER => lkup (x, rt))
   in
      lkup
   end
val _ : ('a * 'a -> order) -> ('a * 'a btree) -> 'a option = btreeLookup
val _ : ('a * 'b -> order) -> ('a * 'b btree) -> 'b option = btreeLookup


type 'a tenv = (name * 'a) btree


(* G.a *)
(* DEFINE tenvEmpty HERE *)

(* G.b *)
(* DEFINE tenvFind HERE *)

(* G.c *)
(* DEFINE tenvBind HERE *)


(* **************************************** *)
(* **************************************** *)


(* Part H (function representation for environments) *)

type 'a fenv = name -> 'a


(* H.a *)
(* DEFINE fenvEmpty HERE *)

(* H.b *)
(* DEFINE fenvFind HERE *)

(* H.c *)
(* DEFINE fenvBind HERE *)


(* **************************************** *)
(* **************************************** *)


(* Part I (append lists) *)

datatype 'a alistNN = Sing of 'a | Append of 'a alistNN * 'a alistNN
datatype 'a alist = Nil | NonNil of 'a alistNN


(* I.a *)
(* DEFINE alistAppend HERE *)
fun alistAppend (xs: 'a alist, ys: 'a alist): 'a alist =
   raise Unimplemented "alistAppend"

(* I.b *)
(* DEFINE alistCons HERE *)
fun alistCons (x: 'a, xs: 'a alist): 'a alist =
   raise Unimplemented "alistCons"


fun alistUncons (xs: 'a alist) : ('a * 'a alist) option =
   case xs of
      Nil => NONE
    | NonNil xs =>
         let
            fun unconsNN (xs: 'a alistNN) : 'a * 'a alist =
               case xs of
                  Sing x => (x, Nil)
                | Append (ys, zs) =>
                     let
                        val (w, ws) = unconsNN ys
                     in
                        (w, alistAppend (ws, NonNil zs))
                     end
         in
            SOME (unconsNN xs)
         end


(* I.c *)
(* DEFINE alistSnoc HERE *)
fun alistSnoc (xs: 'a alist, x: 'a): 'a alist =
   raise Unimplemented "alistSnoc"

(* I.d *)
(* DEFINE alistUnsnoc HERE *)
fun alistUnsnoc (xs: 'a alist): ('a alist * 'a) option =
   raise Unimplemented "alistUnsnoc"

(* I.e *)
(* DEFINE alistMap HERE *)
fun alistMap (f: 'a -> 'b) (xs: 'a alist): 'b alist =
   raise Unimplemented "alistMap"

(* I.f *)
(* DEFINE alistFilter HERE *)
fun alistFilter (f: 'a -> bool) (xs: 'a alist): 'a alist =
   raise Unimplemented "alistFilter"


fun alistFoldr (f: 'a * 'b -> 'b) (b: 'b) (xs: 'a alist) : 'b =
   case xs of
      Nil => b
    | NonNil xs =>
         let
            fun foldrNN (b: 'b) (xs: 'a alistNN): 'b =
               case xs of
                  Sing x => f (x, b)
                | Append (ys, zs) => foldrNN (foldrNN b zs) ys
         in
            foldrNN b xs
         end


(* I.g *)
(* DEFINE alistFoldl HERE *)
fun alistFoldl (f: 'a * 'b -> 'b) (b: 'b) (xs: 'a alist) : 'b =
   raise Unimplemented "alistFoldl"

(* I.h *)
(* DEFINE alistToList HERE *)
fun alistToList (xs: 'a alist) : 'a list =
   raise Unimplemented "alistToList"


(* **************************************** *)
(* **************************************** *)


(* Part J (propositional-logic formulas) *)

datatype fmla =
   F_Var of string
 | F_Not of fmla
 | F_And of fmla * fmla
 | F_Or of fmla * fmla

type 'a env = 'a lenv
val envEmpty = lenvEmpty
val envFind = lenvFind
val envBind = lenvBind


(* J.a *)
(* DEFINE fmlaSize HERE *)

(* J.b *)
(* DEFINE fmlaVarsOf HERE *)

(* J.c *)
(* DEFINE fmlaEval HERE *)

(* J.d !bonus!*)
(* DEFINE fmlaTautology HERE *)
