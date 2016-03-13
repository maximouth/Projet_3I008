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


let rec nettoyage (prec : noeud) (cour : holly_tree) (n : int) : holly_tree =
  match cour with
  | [] -> []
  | {c = a ; f = b; suiv = []}::l' ->
     (if (n = 0) then
	{c = a ; f = b; suiv = []}::(nettoyage prec (List.tl cour) n)
      else
	(nettoyage prec (List.tl cour) n)
     )
  | {c = a ; f = b; suiv = s}::l' ->
     {c = a ; f = b; suiv = (nettoyage {c = a ; f = b; suiv = s} s (n - 1))}::(nettoyage prec (List.tl cour) n)
;;

let rec nettoyer_arbre (abr : holly_tree) (n : int)  : holly_tree =
  match abr with
  | [] -> []
  | {c = a ; f = b ; suiv = s}::abr' ->
     {c = a ; f = b ; suiv = (nettoyage {c = a ; f = b ; suiv = s} s n )}::(nettoyer_arbre abr' n)
;;
    

let rec alea (abr : holly_tree) (res : int list) (n : int) =
  match abr with
  | [] -> List.rev res
  | [a] -> (List.rev (a.c::res))
  | a::abr' ->
     (if n < 1 then
	(alea a.suiv (a.c::res) (Random.int (List.length a.suiv - 1)))
      else
	(alea abr' res (n - 1))
     )
;;
		
  
let chant_alea (list : int list) : int list =  
(*  let abr = (nettoyer_arbre (arbre_possible list) ((List.length list) -1) ) in
 *)
  let abr = arbre_possible list in
  (alea abr [] (List.length abr))
;;

  chant_alea [0;3;0];;


  
