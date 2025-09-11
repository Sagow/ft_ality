let get_file_content (f : string) : string list =
  let rec get_next_line (i : in_channel) (l : string list) : string list =
    try
      begin
        let new_line = input_line i in
        get_next_line i (l@[new_line])
      end
    with
    | End_of_file -> l
  in
  try
    let i = open_in f in
    get_next_line i []
  with
    | Sys_error(msg) -> print_endline ("Sys_error: " ^ msg); []

let check_validity_of_mapping (l : (Utils.move * Utils.key) list) : unit =
  let split = List.split l in
  let nb_moves = List.length(List.sort_uniq (fun x y -> compare x y) (fst split)) in
  let nb_keys = List.length(List.sort_uniq (fun x y -> compare x y) (snd split)) in
  if nb_moves < 12 then
    begin
      Key_mapping.print_pair l;
    failwith "At least one move is undefined in the key mapping"
    end
  else if nb_keys < List.length (snd split) then
    failwith "At least one key has been used twice when defining moves"

let () =
  if (Array.length Sys.argv) <> 2 then
    print_endline ("Usage : " ^ Sys.executable_name ^ " grammar_file")
  else
    let l = get_file_content Sys.argv.(1) in
    if l <> [] then
      try
        begin
          let data_key = Key_mapping.find_key_mapping l in
          check_validity_of_mapping data_key;
          Key_mapping.print_pair data_key;
          let data_combo = Combo_mapping.find_combo_mapping l in
          Combo_mapping.print_combo_list data_combo;
          let table = StateMachine.build data_combo in
          ();
          (* ignore(StateMachine.perform_transition table 17 Utils.FP) *)
          Ft_ality.root ()
        end 
      with
      | Failure msg -> print_endline msg
      | _ -> print_endline "An unknown exception occured"
    else ()
