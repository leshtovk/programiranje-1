(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree = 
    | Empty 
    | Node of 'a * 'a tree * 'a tree

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let leaf x = Node (x, Empty, Empty)
let test_tree = 
    let left = Node (2, leaf 0 , Empty)  
    and right = Node (7, leaf 6, leaf 11)
    in Node (5, left, right)
 
(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror = function
| Empty -> Empty
| Node (x, left, right) -> 
let new_left = mirror right in 
let new_right = mirror left 
in 
Node (x, new_left, new_right)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec size = function  
| Empty -> 0 
| Node (x, left, right) -> 1 + size left + size right


let rec height = function 
| Empty -> 0 
| Node (x, l, r) -> 1 + max (height l) (height r)


(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f = function 
| Empty -> Empty 
| Node (x, left, right) -> Node (f x, map_tree f left, map_tree f right) 

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec to_list = function 
    | Empty -> []
    | Node (x, left, right) -> to_list left @ x :: [] @ to_list right  
    
(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let is_bst tree = 
let rec cheat = function 
| [] -> true 
| [a] -> true
| a :: (b :: tail as t) -> if a < b then cheat t 
else false 
in cheat (to_list tree) 

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
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
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec member k = function 
| Empty -> false 
| Node (x, left, right) -> if k = x then true 
else if k < x then member k left 
else member k right  

let rec insert k = function 
| Empty -> leaf k 
| Node (x, left, right) -> if k < x then Node(x, insert k left, right) 
else if k > x then Node(x, left, insert k right) 
else failwith "It's already in"

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 k = function
    | Empty -> false 
    | Node (x, left_st, right_st) -> 
        if k = x then true 
        else member2 k left_st || member2 k right_st 

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let succ tree =
    let first_move = function 
    | Node (x, left, right) -> right 
    in  
    let rec remaining = function 
    | Empty -> None 
    | Node (x, Empty, _) -> Some x 
    | Node (x, left, right) -> remaining left  
    in remaining (first_move tree)
 
let pred tree = 
    let first_move = function 
    | Node (x, left, right) -> left
    in  
    let rec remaining = function 
    | Empty -> None 
    | Node (x, _, Empty) -> Some x 
    | Node (x, left, right) -> remaining right  
    in remaining (first_move tree)
 
(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

(* idiot solution *)

let filter k list = 
  let rec filter' k acc = function 
    | [] -> acc 
    | x :: xs -> if x = k then acc @ xs 
    else filter' k (x :: acc) xs
  in filter' k [] list

let to_tree list = 
  let rec insert tree x = match tree with 
    | Empty -> Node (x, Empty, Empty)
    | Node (a, left, right) -> 
    if a = x then tree 
    else if a > x then Node (a, insert left x, right) 
    else Node (a, left, insert right x) 
  in List.fold_left insert Empty list 

let delete x tree = 
    let t_list = to_list tree 
    in 
    let filtered = filter x t_list 
    in to_tree filtered  

(* non-idiot solution *)

let rec delete x = function 
    | Empty -> Empty 
    | (Node (a, left, right) as t ) ->
        if x < a then 
            Node (a, delete x left, right) 
        else if x > a then  
            Node (a, left, delete x right) 
        else (* x = a *) 
            match succ t with 
                | None -> left 
                | Some s -> let right_without_s = delete s right in 
                                     Node (s, left, right_without_s)  

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree =  
    | Empty 
    | Node of 'a * 'a tree * 'a tree

type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict : (string, int) dict = 
    Node (("b",1), Node (("a", 0), Empty, Empty),
     Node (("d", 2), Node (("c", -2), Empty, Empty),Empty))

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec get_dict k = function 
| Empty -> None 
| Node ((k', v), l, r) -> if k = k' then Some v 
                       else if k < k' then get_dict k l 
                       else get_dict k r
      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

(* test out :
print_newline 
print_string 
print_endline
print_int 
string_of_int
Philipp says they're important *)

let rec print_dict = function 
| Empty -> ()
| Node ((k, v), l, r) -> 
    (print_dict l;
    print_endline (k ^ " : " ^ (string_of_int v)); 
    print_dict r 
    )

(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec dict_insert k v = function 
    | Empty -> Node ((k, v), Empty, Empty)
    | Node ((k', v') as x, left_st, right_st) -> 
        (* assuming lexicographic order *)
        if k < k' then 
            Node (x, dict_insert k v left_st, right_st) 
        else if k > k' then
            Node (x, left_st, dict_insert k v right_st) 
        else 
            Node ((k, v), left_st, right_st)