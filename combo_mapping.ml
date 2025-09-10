let find_index f lst =
  let rec loop i = function
    | [] -> None
    | head :: tail -> if f head then Some i else loop (i + 1) tail
  in
  loop 0 lst

let rec get_moves (s : string) : Utils.move list =
  let split = (String.trim s |> String.split_on_char ' ' |> List.map String.trim) in
  let res = List.map Utils.string_to_move split in
  if List.exists (fun (x : Utils.move) -> x = Utils.None) res then
    begin
      let rec print_res res =
        match res with
        | h::t -> print_endline (Utils.move_to_string h); print_res t
        | [] -> ()
      in
      print_res res;
      let i = find_index (fun (x : Utils.move) -> x = Utils.None) res in
      failwith ("invalid move in " ^ s ^ " index " ^ (string_of_int (Option.get i)))
    end
  else if List.length res = 0 then failwith "Combos must contain at least one move"
  else res

let rec find_combo_mapping (l : string list) : Combo.Combo.t list =
  let rec map_combo (l : string list) : Combo.Combo.t list =
    match l with
    | h::t when (String.trim h) = "" -> map_combo t
    | h::t -> begin
      let split = String.split_on_char ',' h in
      if List.length split <> 3 then failwith ("This combo line is incorrect : " ^ h)
      else
        (Combo.Combo.newCombo (List.nth split 0) (List.nth split 1) (get_moves (List.nth split 2)))::map_combo t
    end
    | [] -> []
  in
  match l with
  | h::t when (String.trim h) = "combo definitions" -> map_combo t
  | h::t -> find_combo_mapping t
  | [] -> []

let rec print_combo_list (l : Combo.Combo.t list) : unit =
  match l with
  | h::t -> print_endline (Combo.Combo.combo_to_string h); print_combo_list t
  | [] -> ()
