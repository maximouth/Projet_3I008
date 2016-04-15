open MIDI;;
  
module ReadMidi :
  sig
    exception AmbitusWrong
    val readMidi : string -> int list
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
(*    let readMidi (nomF : string) : char list =
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
      | i,o::l ->
	 recur o [];

    ;;
 *)
    let notes_track t=
      let rec loop acc mid =
	match mid with 
	|[]->acc
	|(_,_,MIDI.NoteON(p1,_))::(_,_,MIDI.NoteOFF(p2,_))::ns-> if p1 = p2 
						       then loop (acc@[map_from_mid p1]) ns
						       else raise AmbitusWrong
	|(_,_,MIDI.NoteON(p1,h1))::(_,_,MIDI.NoteON(p2,h2))::ns->(match (p1,h1,p2,h2) with
							|(p1,_,p2,0)->if p1 = p2 
								      then loop (acc@[map_from_mid p1]) ns
								      else raise AmbitusWrong
							|_->raise AmbitusWrong)	
	|n::ns->loop acc ns
      in
      loop [] t
    let intList_of_midi midi =
      let (_,mi) = midi in
      let rec loop acc t =
	match t with
	|[]->acc
	|x::xs->notes_track x :: loop acc xs
      in
      loop [] mi	   
    let readMidi midi =
      let open MIDI in
      let m = read midi in
      let x = intList_of_midi m in
      match x with
	|[]->[]
	|x::xs->x


      
  end;;

    
