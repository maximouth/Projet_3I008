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
  | {c = a ; f = b; suiv = []}::l' -> (
     (if (n <> 0) then
	{c = a ; f = b; suiv = []}::(nettoyage prec (List.tl cour) n)
      else
	(nettoyage prec (List.tl cour) n)
     )
  )
  | {c = a ; f = b; suiv = s}::l' ->
     let suiv = (nettoyage prec (List.tl cour) n) in
     let dessous = (nettoyage {c = a ; f = b; suiv = s} s (n - 1)) in
     {c = a ; f = b; suiv = dessous }::suiv
;;

let rec nettoyer_arbre (abr : holly_tree) (n : int)  : holly_tree =
  match abr with
  | [] -> []
  | {c = a ; f = b ; suiv = s}::abr' ->
     {c = a ; f = b ; suiv = (nettoyage {c = a ; f = b ; suiv = s} s n )}::(nettoyer_arbre abr' n)
;;
    

let rec alea (abr : holly_tree) (res : int list) (n : int) : int list=

  Random.self_init ();
  match abr with
  | [] -> List.rev res
  | [a] -> alea a.suiv (a.c::res) (Random.int ((List.length a.suiv) + 1) )
  | a::abr' -> (
    let length = List.length a.suiv + 1 in         
     (if n < 1 then
	(
	  (alea a.suiv (a.c::res) (Random.int length )
	  )
	)
      else
	(
	  (alea abr' res (n - 1))
	)
     )
  )
;;
		
    
let rec affiche_list l =
  match l  with
  |[] -> Printf.printf "\n"
  | a::l' -> (
    Printf.printf "%d " a;
    affiche_list l'
  )
;;

  
let chant_alea (l : int list) : int list =
  Random.self_init ();
  let length = (List.length l) in
  let abr = (nettoyer_arbre (arbre_possible l) length ) in
  alea abr [] (Random.int length)
;;

(*affiche_list (chant_alea [0;3;5;3]);;
 *)
