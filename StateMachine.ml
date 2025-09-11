type character_name = string
type combo_name = string
type combo_data = character_name * combo_name

module TransitionEntryKV : (FtMap.KEYVALUE with type key_t = Utils.move and type value_t = (combo_data list * int)) = struct
  type key_t = Utils.move
  type value_t = combo_data list * int
  let cmp = compare
end

module TransitionMap : (FtMap.MAP with type key_t := TransitionEntryKV.key_t and type value_t := TransitionEntryKV.value_t) = FtMap.Make (TransitionEntryKV)

type transition_entry = TransitionMap.t
type transition_table = transition_entry list

module ComboKV : (FtMap.KEYVALUE with type key_t = Utils.move list and type value_t = combo_data list) = struct
  type key_t = Utils.move list
  type value_t = combo_data list
  let cmp l1 l2 =
    let rec cmp_values l1 l2 =
      match (l1, l2) with
      | ([], []) -> 0
      | (h1 :: t1, h2 :: t2) ->
        let h1_h2_cmp_res = compare h1 h2 in
        if h1_h2_cmp_res = 0
        then cmp_values t1 t2
        else h1_h2_cmp_res
      | _ -> failwith "This should not happen.\n"
    in
    let lengths_cmp_res = List.compare_lengths l1 l2 in
    if lengths_cmp_res = 0
    then cmp_values l1 l2
    else lengths_cmp_res
end

module ComboMap : (FtMap.MAP with type key_t := ComboKV.key_t and type value_t := ComboKV.value_t) = FtMap.Make (ComboKV)

module Debug = struct

  let rec string_of_move_pattern
    (lst : Utils.move list)
    : string =
      match lst with
      | h :: [] -> Utils.move_to_string h
      | h :: t -> Utils.move_to_string h ^ "-" ^ string_of_move_pattern t
      | [] -> ""

  let rec string_of_combo_data_list
    (lst : combo_data list)
    : string =
      match lst with
      | (character, combo)::[] -> Printf.sprintf "%s(%s)" combo character
      | (character, combo)::t -> Printf.sprintf "%s(%s)" combo character ^ " - " ^ string_of_combo_data_list t
      | [] -> ""

  let string_of_combomap_pair
    (pair : ComboMap.pair_t)
    : string =
    let (move_pattern, combo_data_list) = pair in
    string_of_move_pattern move_pattern ^ ": " ^ string_of_combo_data_list combo_data_list

  let print_combomap
    (map : ComboMap.t)
    : unit =
    List.iter (fun x -> print_endline (string_of_combomap_pair x)) (ComboMap.to_list map)

  let print_splitted_combo_map
    (splitted_map : ComboMap.t list)
    : unit =
    List.iter print_combomap splitted_map

  let string_of_numbered_combomap_pair
    (numbered_pair : (ComboMap.pair_t * int))
    : string =
    let pair, state_number = numbered_pair in
    "State " ^ (string_of_int state_number) ^ ": " ^ (string_of_combomap_pair pair)

  let print_numbered_combomap
    (map : (ComboMap.pair_t * int) list)
    : unit =
    List.iter (fun x -> print_endline (string_of_numbered_combomap_pair x)) map

  let print_splitted_numbered_combomap
    (splitted_map : ((ComboMap.pair_t * int) list) list)
    : unit =
    List.iter print_numbered_combomap splitted_map

  let string_of_transition_entry
    (entry : TransitionMap.pair_t)
    : string =
    let (move, (combo_data_list, next_state)) = entry in
    Printf.sprintf "%s -> %d: %s" (Utils.move_to_string move) next_state (string_of_combo_data_list combo_data_list)

  let print_transitionmap
    (map : TransitionMap.t)
    : unit =
    List.iter (fun entry -> print_endline (string_of_transition_entry entry)) map
  
  let print_transition_table
    (table : TransitionMap.t list)
    : unit =
    List.iteri (fun i x -> Printf.printf "\nSTATE %d:\n" i; print_transitionmap (TransitionMap.to_list x)) table

end

let create_combo_map
  (raw_combos : Combo.Combo.t list)
  : ComboMap.t =
  let formated_list = List.map (fun (combo : Combo.Combo.t) -> (combo.move_pattern, [(combo.character, combo.attack_name)])) raw_combos in
  ComboMap.of_list formated_list

let split_by_input_count
  (combo_map : ComboMap.t)
  : ComboMap.t list =
  let rec loop
    (combo_list : ComboMap.pair_t list)
    (acc : ComboMap.t list)
    (cur_acc : ComboMap.pair_t list)
    (cur_len : int)
    : ComboMap.pair_t list list =
    match combo_list with
    | [] -> (ComboMap.of_list cur_acc) :: acc
    | head :: tail ->
      let (inputs, _) = head in
      let input_len = List.length inputs in
      if input_len = cur_len
      then loop tail acc (head :: cur_acc) cur_len
      else loop combo_list ((ComboMap.of_list cur_acc) :: acc) [] (cur_len + 1)
  in
  match ComboMap.to_list combo_map with
  | [] -> []
  | not_empty -> loop not_empty [] [] 1

let add_intermediate_states
  (splitted_map : ComboMap.t list)
  : ComboMap.t list =
  let rec loop
    (lst : ComboMap.t list)
    (current : ComboMap.t)
    (acc : ComboMap.t list)
    : ComboMap.t list =
    let get_anterior_state
      ((move_set, _): ComboMap.pair_t)
      : ComboMap.pair_t =
      match List.rev move_set with
      | head :: [] -> failwith "This function must not be called with 1 move elements.\n"
      | head :: tail -> (List.rev tail, [])
      | [] -> failwith "This function must not be called with 0 move elements.\n"
    in
    match lst with
    | head :: tail -> loop tail (head |> ComboMap.merge (ComboMap.map get_anterior_state current) |> ComboMap.merge_duplicates (@)) (current :: acc)
    | [] -> current :: acc
  in
  match splitted_map with
  | head :: tail -> loop tail head []
  | [] -> []

let rec list_starts_with
  (startswith : 'a list)
  (lst : 'a list)
  : bool =
  match (lst, startswith) with
  | (_, []) -> true
  | ([], _) -> false
  | (lst_h :: lst_t, startswith_h :: startswith_t) when lst_h = startswith_h ->
    list_starts_with startswith_t lst_t
  | (_, _) -> false

let mark_end_states
  (splitted_map : ComboMap.t list)
  : ((ComboMap.pair_t * bool) list) list =
  let rec foreach_map
    (lst : ComboMap.t list)
    (current : ComboMap.t)
    (acc : ((ComboMap.pair_t * bool) list) list)
    : ((ComboMap.pair_t * bool) list) list =
    let rec foreach_entry
      (current : ComboMap.pair_t list)
      (next_moves : ComboMap.pair_t list)
      (acc : (ComboMap.pair_t * bool) list)
      : (ComboMap.pair_t * bool) list =
      match current with
      | [] -> acc
      | head :: tail ->
        let (move_set, combo_list) = head in
        if List.exists (fun (nm_move_set, _) -> list_starts_with move_set nm_move_set) next_moves
        then foreach_entry tail next_moves ((head, true) :: acc)
        else foreach_entry tail next_moves ((head, false) :: acc)
    in
    match lst with
    | head :: tail -> foreach_map tail (ComboMap.to_list head) ((List.rev (foreach_entry (ComboMap.to_list current) (ComboMap.to_list head) [])) :: acc)
    | [] -> (List.rev (foreach_entry (ComboMap.to_list current) [] [])) :: acc
  in
  match splitted_map with
  | head :: tail -> List.rev (foreach_map tail head [])
  | [] -> []

let attribute_state_numbers
  (splitted_map : ComboMap.t list)
  : ((ComboMap.pair_t * int) list) list =
  let states = mark_end_states splitted_map in
  let rec loop
    (states : ((ComboMap.pair_t * bool) list) list)
    (current_states : (ComboMap.pair_t * bool) list)
    (current_acc : (ComboMap.pair_t * int) list)
    (acc : ((ComboMap.pair_t * int) list) list)
    (counter : int)
    : ((ComboMap.pair_t * int) list) list =
    match current_states with
    | (head_pair, is_finite) :: tail ->
      if is_finite
      then loop states tail ((head_pair, counter) :: current_acc) acc (counter + 1)
      else loop states tail ((head_pair, 0) :: current_acc) acc counter
    | [] -> begin
      match states with
      | head :: tail -> loop tail head [] ((List.rev current_acc) :: acc) counter
      | [] -> ((List.rev current_acc) :: acc)
    end
  in
  match states with
  | head :: tail -> List.rev (loop tail head [] [] 1)
  | [] -> []

let rec strip_prefix
  (prefix : 'a list)
  (lst : 'a list)
  : 'a list option =
  match (lst, prefix) with
  | (lst, []) -> Some(lst)
  | ([], _) -> None
  | (lst_h :: lst_t, prefix_h :: prefix_t) when lst_h = prefix_h ->
    strip_prefix prefix_t lst_t
  | (_, _) -> None

let build_initial_state_entry
  (one_event_list : ((ComboMap.pair_t * int) list))
  : TransitionMap.t =
  let default_state_entry = TransitionMap.of_list [(Block, ([], 0));(Down, ([], 0));(FlipStance, ([], 0));(Left, ([], 0));(Right, ([], 0));(Tag, ([], 0));(Throw, ([], 0));(Up, ([], 0));(BK, ([], 0));(BP, ([], 0));(FK, ([], 0));(FP, ([], 0))] in
  let replace_event_entry
    (map : TransitionMap.t)
    (entry : ComboMap.pair_t * int)
    : TransitionMap.t =
    let (raw_combo, next_state) = entry in
    let (moves, combo_list) = raw_combo in
    match moves with
    | head :: [] -> TransitionMap.replace head (combo_list, next_state) map
    | _ -> failwith "This should not happen.\n"
  in
  List.fold_left replace_event_entry default_state_entry one_event_list

let build_table
  (combo_maps : ((ComboMap.pair_t * int) list) list)
  : TransitionMap.t list =
  match combo_maps with
  | [] -> [build_initial_state_entry []]
  | head :: tail -> begin
    let initial_state = build_initial_state_entry head in
    let rec add_all_entries
      (table : TransitionMap.t list)
      (current_combos_map : (ComboMap.pair_t * int) list)
      (further_combos_maps : ((ComboMap.pair_t * int) list) list)
      : TransitionMap.t list =
      match further_combos_maps with
      | [] -> table
      | head :: tail -> begin
        let add_entries_of_combo_map
          (acc : TransitionMap.t list)
          (elem : (ComboMap.pair_t * int))
          : TransitionMap.t list =
          let (raw_combo, state_number) = elem in
          if state_number = 0
          then acc
          else begin
            let (move_lst, combo_lst) = raw_combo in
            let build_new_entry
              (acc : TransitionMap.t)
              (elem : (ComboMap.pair_t * int))
              : TransitionMap.t =
              let (elem_raw_combo, elem_state_number) = elem in
              let (elem_move_lst, elem_combo_lst) = elem_raw_combo in
              let combo_end = strip_prefix move_lst elem_move_lst in
              match combo_end with
              | None -> acc
              | Some(head :: []) -> TransitionMap.replace head (elem_combo_lst, elem_state_number) acc
              | Some(_) -> failwith "This should not happen.\n"
            in
            let new_entry = List.fold_left build_new_entry initial_state head in
            new_entry :: acc
          end
        in
        let new_table = List.fold_left add_entries_of_combo_map table current_combos_map in
        add_all_entries new_table head tail
      end
    in
    List.rev (add_all_entries [initial_state] head tail)
  end

let build
  (raw_combos : Combo.Combo.t list)
  : TransitionMap.t list =
  let combo_map = create_combo_map raw_combos in
  (* Debug.print_combomap combo_map; *)
  let no_duplicates = ComboMap.merge_duplicates (@) combo_map in
  (* Debug.print_combomap no_duplicates; *)
  let splitted_map = split_by_input_count no_duplicates in
  (* Debug.print_splitted_combo_map splitted_map; *)
  let with_intermediate_states = add_intermediate_states splitted_map in
  (* Debug.print_splitted_combo_map with_intermediate_states; *)
  let numbered_combomap = attribute_state_numbers with_intermediate_states in
  Debug.print_splitted_numbered_combomap numbered_combomap;
  let table = build_table numbered_combomap in
  Debug.print_transition_table table;
  table
  (* Transform Combo.Combo.t list to map (Utils.move list * combo_data list) - combo list containing only one element at the time *)
  (* Use merge_duplicates to merge combos with same move set in same list *)
  (* Take sorted (sort by list length before elements) list out of map *)
  (* Subdivide list in a list and then a map for each different number of moves *)
  (* Browse lists from the the highest number of moves to the lowest number of moves:
   - For each element in the list of n element, look for the previous move in the list of (n - 1) elements
   - Create intermediate states with no combo for each missing element
   - Optimize ignoring elements with the same start ? Might create a fold function, add new states in an accumulator list,
    the first of the list is the last created, which is enought to check as the elements are sorted in the map *)
  (* Create the root element and complete its transition table *)
  (* For each state in the lists (join or not elements in the same list), for each move, find the index of the next state, or, if not found, copy the one from root *)

let perform_transition
  (table : TransitionMap.t list)
  (state : int)
  (input : Utils.move)
  : combo_data list * int =
  let table_entry = List.nth table state in
  match TransitionMap.find_opt table_entry input with
  | Some(combos, next_state) -> begin
    Printf.printf "%s\nTransition to state %d\n" (Debug.string_of_combo_data_list combos) next_state;
    (combos, next_state)
  end
  | None -> failwith ("It should not happen")
