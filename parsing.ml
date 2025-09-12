module KeyMoveMap = Key_mapping.KeyMoveMap

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

let check_validity_of_mapping (key_move_map : KeyMoveMap.t) : unit =
  let split = List.split key_move_map in
  let nb_moves = List.length(List.sort_uniq (fun x y -> compare x y) (fst split)) in
  let nb_keys = List.length(List.sort_uniq (fun x y -> compare x y) (snd split)) in
  if nb_moves < 12 then
    begin
      Key_mapping.print_pair key_move_map;
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
          let key_move_map = Key_mapping.find_key_mapping l in
          check_validity_of_mapping key_move_map;
          Key_mapping.print_pair key_move_map;
          let data_combo = Combo_mapping.find_combo_mapping l in
          (* Combo_mapping.print_combo_list data_combo; *)
          let table = StateMachine.build data_combo in
          let window = Ft_ality.init_sdl () in
          print_endline "----------------------";
          print_endline "";
          begin
            try
              let rec loop (state : int) : unit =
                let move = Ft_ality.wait_for_next_move key_move_map in
                print_endline ("Input: " ^ (Utils.move_to_string move));
                let (combo_list, next_state) = StateMachine.perform_transition table state move in
                StateMachine.print_combo_list combo_list;
                print_endline ("Transition to state " ^ (string_of_int next_state));
                print_endline "";
                loop next_state
              in
              loop 0
            with
            | Failure msg -> print_endline msg
            | _ -> print_endline "An unknown exception occured"
          end;
          Tsdl.Sdl.destroy_window window;
          Tsdl.Sdl.quit ();
          (* ignore(StateMachine.perform_transition table 17 Utils.FP) *)
        end 
      with
      | Failure msg -> print_endline msg
      | _ -> print_endline "An unknown exception occured"
    else ()
