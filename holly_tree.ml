type noeud = {c : int; f : int; suiv : holly_tree}
	   and
	     holly_tree = noeud list;;


  
let rec noeud_l (l : holly_tree) (f : int) (c : int) (n : int) : holly_tree =
  match n with
  | 12 -> List.rev l
  | z ->
     (match abs(f - n) with
	0 | 2 | 4 | 5 | 7 ->
			 (match abs(c - n) with 
			    0 | 1 | 2 | 3 | 4 | 5 | 7 ->
						     (noeud_l ({c = n ;
							       f = f ;
							       suiv = []}::l) f c (n + 1))
			 | z -> (noeud_l l f c (n + 1))
			 )
	| z -> (noeud_l l f c (n + 1))
     )
;;

let noeud_possible f c =
  noeud_l [] f c 4
;;	    

  

let rec arbre_p (listnoeud : holly_tree) (listchant : int list ) : holly_tree =
  match listchant with
  | [] -> []
  | i::l' ->
     (match listnoeud with
      | [] -> []
      | n::nd' ->
	 (
	   let r = (noeud_possible i n.c) in
	   let l3 = arbre_p r (List.tl listchant) in
	   let l4 = (arbre_p nd' listchant) in
	 {c = n.c; f = n.f; suiv = l3}::l4
	 )
     )
;;
  
let arbre_possible (l : int list) : holly_tree =
  (arbre_p (noeud_possible  0 (List.hd l)) (List.tl l@(0::[])))
;;

  arbre_possible [0;3];;

  
