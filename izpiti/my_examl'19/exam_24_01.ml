(* Naloga 1 *)

(* a *)
let podvoji_vsoto a b = 2 * (a + b)

(* b *)
let rec povsod_vecji (a, b, c) (a', b', c') = 
  (a > a') && (b > b') && (c > c')

(* c *)  
let uporabi_ce_lahko f x = match x with 
  | None -> None 
  | Some x -> Some (f x)

(* d *)  
let pojavi_dvakrat k a_list = 
  let rec count acc a_list = match a_list with 
    | [] -> acc 
    | x :: xs -> if x = k then count (acc + 1) xs 
                else count acc xs 
  in (count 0 a_list) = 2

(* e *) 
let izracunaj_v_tocki k f_list = 
  let rec aux acc f_list = match f_list with 
    | [] -> acc 
    | f :: fs -> aux ((f k) :: acc) fs 
  in List.rev(aux [] f_list)

(* f *)  
let eksponent =
  let rec aux a x n =
    if n = 0 then a else 
    aux (a * (if n mod 2 = 0 then 1 else x)) (x * x) (n / 2) 
  in aux 1

(* Naloga 2 ---------------------------------------------------------------- *)

(* a *)
type 'a mm_drevo = 
  | Empty 
  | Node of 'a mm_drevo * ('a * int) * 'a mm_drevo 

let test_mm_drevo = 
  Node (Node (Empty, (1, 3), Empty),(2, 2), Node (Node (Empty, (4, 1), Empty),
  (5, 1), Node (Empty, (8, 2), Empty)))

(* b *) 
let rec vstavi m_set k = match m_set with 
  | Empty -> Node (Empty, (k, 1), Empty)
  | Node (l_set, (h, c), r_set) -> 
    if k = h then Node (l_set, (h, c+1), r_set)
    else if k < h then let new_left = vstavi l_set k in 
    Node (new_left, (h, c), r_set) 
    else let new_right = vstavi r_set k in
    Node (l_set, (h, c), new_right)


(* c *)     
let mm_iz_sez a_list = List.fold_left vstavi Empty a_list 

(* d *)
let rec velikost_mm m_set = match m_set with 
    | Empty -> 0 
    | Node (l_set, (h, c), r_set) -> 
      let left_size = velikost_mm l_set in 
      let right_size = velikost_mm r_set in 
      c + left_size + right_size  

(* e *)    
let rec sez_iz_mm m_set = match m_set with 
  | Empty -> [] 
  | Node (l_set, (h, c), r_set) -> 
      let head_list k i =
        if i <= 0 then failwith "nope" else 
        let rec aux acc i = 
          match i with 
          | 1 -> k :: acc
          | i -> aux (k :: acc) (i - 1)
        in aux [] c 
        in 
    (sez_iz_mm l_set) @ (head_list h c) @ (sez_iz_mm r_set)


