
(* Problem 1 *)

type ilist =
    E
  | L of int * ilist

(* sum : ilist -> int *)

let rec sum l =
  match l with
  | E -> 0
  | L(h, t) -> h + sum t
  
sum (L(1, L(3, L(3, E)))) // Should return 7
sum E // Should return 0

(* elem : int -> ilist -> int *)

let rec elem n l =
  if n > 0 
  then match l with
       | E -> failwith "Index out of bound"
       | L(h,t) -> if n = 1 then h else elem (n-1) t
  else failwith "Value of n (index) is non-positive"

elem 2 (L(3, L(21, L(11, E)))) // Should return 21
elem 4 (L(3, L(21, L(11, E)))) // Should return "Index out of bound"
elem -1 (L(3, L(21, L(11, E)))) // Should return "Value of n (index) is non-positive"

(* isIn : int -> ilist -> bool *)
let rec isIn x l =
  match l with
  | E -> false
  | L (h,t) -> if x = h then true else isIn x t

isIn 3 (L(3, L(21, L(11, E)))) // Should return true
isIn 4 (L(3, L(21, L(11, E)))) // Should return false

(* remove: int -> ilist -> ilist *)
let rec remove x l =
  match l with
  | E -> E
  | L (h,t) -> if x = h then remove x t else L(h, remove x t)

remove 2 (L(1, L(2, L(3, L(3, L(2, E)))))) // Should return L(1, L(3, L(3, E)))
remove 5 (L(1, L(2, L(3, E)))) // (L(1, L(2, L(3, E))))


(* move : ilist -> ilist -> ilist *)

let rec move l1 l2 =
  match l1 with 
  | E -> l2
  | L (h, t) -> move t (L(h, l2))

move (L(1, L(2, L(3, E)))) (L(4, L(5, E))) // Should return L(3, L(2, L(1, L(4, L(5, E)))))

(* reverse : ilist -> ilist *)
let reverse l =
  move l E

reverse (L(1, L(2, L(3, E))))

(* Problem 2 *)

type btree = | Leaf of int 
             | Node of btree * int * btree
 
type finding = 
  | NotFound 
  | Found of int

(* size: btree -> int *)

let size t =
  let rec loop sum tree =
    match tree with
    | Leaf _ -> sum + 1
    | Node (btree1,_,btree2) -> sum + loop 0 btree1 + loop 0 btree2
  loop -1 t


size (Node (Node (Leaf 4, 2, Leaf 5), 1, Node (Leaf 6, 3, Leaf 7)))// Size 3
size (Node (Leaf 5, 1, Node (Leaf 6, 3, Node (Leaf 8, 7, Leaf 9))))// Size 3


(* leftmost_nl: btree -> finding *)

let rec leftmst_nl t =
  match t with
  | Leaf(_) ->  NotFound
  | Node(Leaf(_), x, _) -> Found x
  | Node(leftTree, _, _) -> leftmst_nl leftTree


leftmst_nl (Node (Node (Leaf 4, 2, Leaf 5), 1, Node (Leaf 6, 3, Node (Leaf 8, 7, Leaf 9))))//should return 2

(* mirror: btree -> btree *)
let rec mirror t =
  match t with
  | Leaf(x) -> Leaf (x)
  | Node(Leaf a, x, Leaf b) ->  Node(Leaf b, x , Leaf a)
  | Node(a, x, b) ->  Node (mirror a, x, mirror b)

mirror (Node(Leaf 1, 3, Leaf 2))
mirror (Node (Node (Leaf 0, 1, Leaf 1), 3, Node (Leaf 0, 4, Node (Leaf 1, 7, Leaf 2))))

(* leftReplace: int -> btree -> btree *)

let rec leftReplace x t =
  match t with
  | Node(Leaf a, b, c) -> Node (Leaf x, b, c)
  | Node(leftTree, b, c) -> Node(leftReplace x leftTree, b, c)

leftReplace 9  (Node (Node (Leaf 0, 1, Leaf 1), 3, Node (Leaf 0, 4, Node (Leaf 1, 7, Leaf 2))))

(* toString_nz: btree -> string *)
let rec toString_nz t = 
  match t with
  | Leaf (x) -> string x
  | Node (leftTree,y,rightTree) -> string y + " " + toString_nz leftTree + " " + toString_nz rightTree
 
toString_nz (Node (Node (Leaf (9), 1, Leaf (1)), 3, Node (Leaf (0), 4, Node (Leaf (1), 7, Leaf (2)))))
