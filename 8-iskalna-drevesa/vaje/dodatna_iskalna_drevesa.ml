(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 DODATNE VAJE 
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree = 
  | Empty 
  | Node of 'a * 'a tree * 'a tree

  let leaf x = Node (x, Empty, Empty)
  let test_tree = 
    let left = Node (2, leaf 0 , Empty)  
    and right = Node (7, leaf 6, leaf 11)
    in Node (5, left, right)

(*----------------------------------------------------------------------------*]
 Funkcija [bst_of_list] iz seznama naredi dvojiško iskalno drevo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)

let bst_of_list a_list = 
  let rec insert_el b_tree k = match b_tree with 
    | Empty -> Node (k, Empty, Empty) 
    | Node (h, left_st, right_st) -> 
      if k < h then 
        Node (h, insert_el left_st k, right_st) 
      else if k > h then 
        Node (h, left_st, insert_el right_st k) 
      else (* k = h *)
        Node (h, left_st, right_st) 
  in List.fold_left insert_el Empty a_list

    (*----------------------------------------------------------------------------*]
 Funkcija [tree_sort] uredi seznam s pomočjo pretvorbe v bst in nato nazaj
 v seznam.

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)

let tree_sort a_list = 
  let rec tree_to_list = function 
    | Empty -> [] 
    | Node (h, left_st, right_st) -> 
      tree_to_list left_st @ [h] @ tree_to_list right_st 
  in 
  tree_to_list (bst_of_list a_list) 

(*----------------------------------------------------------------------------*]
 Funkcija [follow directions tree] tipa [direction list -> 'a tree -> 'a option]
 sprejme seznam navodil za premikanje po drevesu in vrne vozlišče do katerega 
 vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v drevesu
 vrne rezultat kot [option] tip. Ne pozabite definirati tipa [directions].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)

type 'a tree = 
  | Empty 
  | Node of 'a * 'a tree * 'a tree

let test_tree = Node (5, Node (2, Node (0, Empty, Empty), Empty),
  Node (7, Node (6, Empty, Empty), Node (11, Empty, Empty)))

type directions = Left | Right 

let rec follow dir_list tree = match (dir_list, tree) with 
  | [], Node (h, left_st, right_st) -> Some h 
  | [], Empty -> None
  | a_list, Empty -> None 
  | x :: xs, Node (h, left_st, right_st) -> 
      if x = Right then follow xs right_st
      else (* x = Left *) follow xs left_st   
                         
(*----------------------------------------------------------------------------*]
 Funkcija [prune directions tree] poišče vozlišče v drevesu glede na navodila,
 ter izbriše poddrevo, ki se začne v izbranem vozlišču.

 Opozorilo: Pri uporabi [Some Node(l, x, r)] se OCaml pritoži, saj to razume 
 kot [(Some Node)(l, x, r)], zato pravilno postavite potrebne oklepaje.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)

let rec prune dir_list tree = match dir_list, tree with
  | [], a_tree -> Some Empty
  | a_list, Empty -> None
  | x :: xs, Node(h, left_st, right_st) ->
    if x = Left then 
      (match prune xs left_st with
        | None -> None
        | Some new_left_st -> Some (Node(h, new_left_st, right_st)))
    else 
      (match prune xs right_st with
        |None -> None
        | Some new_right_st -> Some (Node(h, left_st, new_right_st)))  

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 PHANTOM TREES

 Druga možnost pri brisanju podatkov je, da spremenimo tip s katerim
 predstavljamo drevo. Definirate nov tip fantomskega drevesa, ki poleg podatka,
 levega in desnega poddrevesa hrani še dodatno informacijo o stanju [state], ki
 je bodisi [Exists] če je vozlišče še prisotno in pa [Ghost] če je vozlišče v
 drevesu izbrisano in ga upoštevamo le še kot delitveno vozlišče. Še vedno
 predpostavljamo, da imajo drevesa obliko BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree =    (* CAUTION: new definition *)
  | Empty                
  | Node of 'a tree * 'a * 'a tree  

type state = Exists | Ghost 

type 'a phantom_tree = 
  | P_Empty 
  (* ! See what works better for state ! *)
  | P_Node of 'a phantom_tree * 'a * state * 'a phantom_tree 

(*----------------------------------------------------------------------------*]
 Funkcija [phantomize] tipa ['a tree -> 'a phantom_tree] navadnemu drevesu
 priredi ekvivalentno fantomsko drevo.
 Funkcija [kill x ptree] izbriše element [x] v fantomskem drevesu tako, da 
 njegovo stanje nastavi na [Ghost].
 Predpostavite lahko, da v drevesu ni ponovitev elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
[*----------------------------------------------------------------------------*)

let rec phantomize = function  
  | Empty -> P_Empty 
  | Node (left_st, h, right_st) -> 
      P_Node (phantomize left_st, h, Exists, phantomize right_st) 

(*----------------------------------------------------------------------------*]
 Funkcija [unphantomize] tipa ['a phantom_tree -> 'a tree] fantomskemu drevesu 
 priredi navadno drevo, ki vsebuje zgolj vozlišča, ki še obstajajo. Vrstni red
 vozlišč v končnem drevesu ni pomemben.

 Namig: Lahko uporabite vmesni prehodom na drugo podatkovno strukturo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)
