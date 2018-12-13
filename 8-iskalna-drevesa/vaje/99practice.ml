type 'a binary_tree = 
  | Empty 
  | Node of 'a * 'a binary_tree * 'a binary_tree

  let leaf x = Node (x, Empty, Empty)
  let test_tree = 
      let left = Node (2, leaf 0 , Empty)  
      and right = Node (7, leaf 6, leaf 11)
      in Node (5, left, right)


let is_symmetric b_tree =       
  let rec mirror = function 
  | Empty -> Empty 
  | Node (x,left,right) -> 
  let new_right = mirror left  in
  let new_left = mirror right in 
  Node (x, new_left, new_right)
in b_tree = (mirror b_tree)  


(* !!! USEFUL !!! *)
let construct l = 
let rec insert b_tree x = match b_tree with 
| Empty -> leaf x 
| Node (a, left, right) -> 
if a = x then b_tree 
else if a > x then Node (a, insert left x, right) 
else Node (a, left, insert right x) 
in List.fold_left insert Empty l 


(* Build all trees with given [left] and [right] subtrees.  *)
let build_trees_with left right all = 
  let add_right_tree all l = 
    List.fold_left (fun a r -> Node ('x', l, r) :: a) all right 
    in List.fold_left build_with_right all left 


    