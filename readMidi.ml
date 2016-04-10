open MIDI;;
  
module ReadMidi :
  sig
    exception AmbitusWrong
    val readMidi : string -> char list
    val wroteMidi 
      
  end
  =
  struct

    exception AmbitusWrong;;
    
    (* convertie une note en pitch midi en entier  *)
    let map_from_mid c =
      match c with
      | 62 -> 0
      | 64 -> 1
      | 65 -> 2
      | 67 -> 3
      | 69 -> 4
      | 71 -> 5
      | 72 -> 6
      | 74 -> 7
      | 76 -> 8
      | 77 -> 9
      | 79 -> 10
      | 81 -> 11
      | _  -> raise (AmbitusWrong)
    ;;

      

    (* lit un fichier midi et renvoie la liste des élements *)
    let readMidi (nomF : string) : char list =
      let f = MIDI.read nomF in

      let rec recur (l : MIDI.track list) res =
	match l with
	| [] -> List.rev res
	| (_,_,MIDI.NoteON(pitch,0))::l' ->
	   recur l' res
	| (_,_,MIDI.NoteOFF(pitch,0))::l' ->
	   recur l' res
	| (_,_,MIDI.NoteON(pitch,_))::l' ->
	   recur l' ((map_from_mid pitch)::res)
      in

      match f with
      | i,o ->
	 recur o [];

    ;;




      
  end;;

    
