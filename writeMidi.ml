open MIDI;;

module WriteMidi :
    sig
      exception ListeIncorrecte of string
      val ecrire_partition : int list -> int list -> string -> unit
    end 
  =
  struct
    exception ListeIncorrecte of string
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
      | _  -> raise ( ListeIncorrecte ("prout\n"))
    ;;

    let rec convert cantus contre time res =
      match cantus, contre with
      | [], [] -> (List.rev res)
      | a::l',b::m' ->
	 convert l' m' (time + 200) ( (time,0,MIDI.NoteON (midi_from_map(a), 127))::(time,0,MIDI.NoteON (midi_from_map(b), 127))::(time,0,MIDI.NoteOFF (midi_from_map(a), 0))::(time,0,MIDI.NoteOFF (midi_from_map(b), 0))::res )
      | _,_ -> raise ( ListeIncorrecte ("prout\n"))
    ;;
    
    let ecrire_partition cantus contre nomf  =
      let struc_midi = (convert cantus contre 0 []) in 
      MIDI.write (0,(struc_midi::[])) nomf 
    ;;

  end;;

