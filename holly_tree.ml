type noeud = {c : int; f : int; suiv : holly_tree}
	   and
	     holly_tree = noeud list;;


  
let rec noeud_l l f c n =
  match n with
  | 12 -> List.rev l
  | z ->
     (match abs(f - n) with
	0 | 2 | 4 | 5 | 7 ->
			 (match abs(c - n) with 
			    0 | 1 | 2 | 3 | 4 | 5 | 7 ->
						     noeud_l ({c = n ;
							       f = f ;
							       suiv = []}::l) f c (n + 1)
			    | n -> noeud_l  l f c (n + 1))
	| z -> noeud_l l f c (n + 1))
;;

let noeud_possible f c =
  noeud_l [] f c 4
;;	  
