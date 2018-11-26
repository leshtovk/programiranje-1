(* 1 *)

let rec last = function 
 | [] -> None
 | x :: [] -> x 
 | x :: xs -> last xs 

(* 2 *)

let rec last_two = function 
 | [] | [_] -> None 
 | [x;y] -> Some (x,y) 
 | x :: xs -> last_two xs 

(* 3 *)

let rec at k = function 
 | [] -> None 
 | _ when k < 0 -> failwith "You ain't ready to get that low boy"
 | x :: xs when k = 0 -> Some x 
 | x :: xs when k > 0 -> at (k-1) xs 

(* 4 *)

let length = 
  let rec length_aux acc = function 
   | [] -> acc 
   | x :: xs -> length_aux (acc + 1) xs 
  in length_aux 0  

(* 5 *) 

let reverse =  
  let rec reverse_aux l = function  
 | [] -> failwith "Nothing to reverse" 
 | [x] -> x :: l
 | x :: xs -> reverse_aux (x :: l) xs
in reverse_aux [] 

(* 6 *)

let is_palindrome list = reverse list = list 

(* 7 *) 

type 'a node = 
  | One of 'a 
  | Many of 'a node list 

let flatten list = 
  let rec separate acc = function 
   | One x :: tail -> separate (x :: acc) tail 
   | Many xs :: tail -> separate (separate acc xs) tail 
in List.rev (separate [] list)  

(* 8 *)

let rec compress = function 
| [] -> failwith "Nothing to compress" 
| [t] -> [t] 
| x :: y :: xs -> if x = y then compress (y :: xs) else x :: compress (y :: xs) 
 
(* 9 *) 

let pack list =
  let rec aux current acc = function
    | [] -> []   
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as tail) ->
       if a = b then aux (a :: current) acc tail
       else aux [] ((a :: current) :: acc) tail  in
  List.rev (aux [] [] list)

(* 10 *) 

let encode list = 
  let rec aux count acc = function 
  | [] -> []      (* last step *)
  | [x] -> (count + 1, x) :: acc 
  | x :: (y :: _ as tail) -> 
  if x = y then aux (count + 1) acc tail 
  else aux 0 ((count + 1, x) :: acc) tail 
  in List.rev(aux 0 [] list)   

(* 11 *)

type 'a element = 
  | One of 'a 
  | Many of int * 'a 

let encode_evolved list = 
  let create_tuple cnt elem = 
    if cnt = 1 then One elem 
    else Many (cnt, elem) 
    in 
    let rec encode_aux count acc = function 
    | [] -> [] 
    | [x] -> (create_tuple (count + 1) x) :: acc 
    | x :: (y :: _ as tail) -> 
    if x = y then encode_aux (count + 1) acc tail 
    else encode_aux 0 ((create_tuple (count + 1) x) :: acc) tail 
    in List.rev(encode_aux 0 [] list)

(* 12 *)

type 'a element = 
  | One of 'a 
  | Many of int * 'a 

  let decode list =
    let rec many acc n x =
      if n = 0 then acc else many (x :: acc) (n-1) x in
    let rec aux acc = function
      | [] -> acc
      | One x :: t -> aux (x :: acc) t
      | Many (n,x) :: t -> aux (many acc n x) t  in
    aux [] (List.rev list)

(* 15 *)

let replicate k list =
 let rec prepare acc n x = 
    if n = 0 then acc 
    else prepare (x :: acc) (n-1) x 
  in 
  let rec aux acc = function 
    | [] -> [] 
    | x :: xs -> (prepare acc k x) @ aux acc xs  
  in aux [] list
