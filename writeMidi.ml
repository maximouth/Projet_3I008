module WriteMidi
    sig
      exception ListeIncorrecte of string
      val ecrire_partition : int list -> int list -> string -> unit
    end 
  =
  struct
    (* convertie une note en entier en  pitch midi  *)
    let midi_from_map c =
      match c with
      | 0 -> 62
      | 1 -> 64 
      | 2 -> 65 
      | 3 -> 67 
      | 4 -> 69 
      | 5 -> 71 
      | 6 -> 72 
      | 7 -> 74 
      | 8 -> 76 
      | 9 -> 77 
      | 10 -> 79 
      | 11 -> 81 
      | _  -> raise (AmbitusWrong)
    ;;

      

  end;
