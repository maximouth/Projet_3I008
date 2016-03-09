module ReadScore :
  sig
    exception INVALID_F
    val readScore : string -> char list
  end
  =
  struct
    (* renvouyé si le fichier n'est pas ouvrable  *)
    exception INVALID_F ;;

    (* boucle lecture d'un fichier
       retourne la liste des elements 
     *)
    let rec lire f l =
      try 
	match input_char f with
	| ';' -> lire f l
	| '\n' -> lire f l
	| '\r' -> lire f l
	| n -> lire f (n::l)
      with
	End_of_file -> List.rev l
    ;;

    (* test si le fichier a le bon format  *)
    let test  nomF =
      let  i = ref 0 in
      while (nomF.[!i] != '.') && (!i < (String.length nomF - 1))
      do
	Printf.printf "i = %d\n" !i;
	i := !i + 1
      done;
	
      (if (!i >= String.length (nomF))
       then
	 raise INVALID_F
       else
	 (if (nomF.[!i+1] == 'c' && nomF.[!i+2] == 's' && nomF.[!i+3] == 'v' )
	  then 0
	  else
	    raise INVALID_F
	 )
	)
    ;;      

    (* Lecture d'un fichier CSV, retourne une liste des éléments ou 
       Une exception INVALID_F si le fichier n'est pas ouvrable  
     *)
    let readScore nomF =
      if (test (nomF) <> 0)
      then
	raise INVALID_F;
      let f = open_in (nomF) in
      let res = lire f [] in
      close_in f;
      res
    ;;
      
  end;;
