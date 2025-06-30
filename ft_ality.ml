type key = Up | Down | Left | Right | Other
(* à remplacer par les codes sdlkey *)
type move = Block | Tag | Throw
type combo = ((move list) * string)

let is_mapped (k : key) : move =
  (* regarder si c'est dans le tableau des keys associées, renvoyer le move associé *)
  Block

let rec wait_for_valid_entry () : move =
  let entry = Up in
  (* ici il faut faire l'aquisition avec sdlkey *)
  match entry with
  | Up -> is_mapped Up
  | Down -> is_mapped Down
  | Left -> is_mapped Left
  | Right -> is_mapped Right
  | _ -> wait_for_valid_entry ()

let compatible_with (m : move) (c : combo) : bool =
  match fst c with
  | h::t when m = h -> true
  | _ -> false

let shorten_combo (c : combo) : combo =
  match fst c with
  | h::t -> (t, snd c)
  | _ -> ([], snd c)

let rec searching_for_combo (m : move) (possible_combos : combo list) (original_combo_list : combo list) : unit =
  match possible_combos with
  | h::t when compatible_with m h ->
    begin
      match (shorten_combo h) with
      | ([], name) -> print_endline name
      | res -> searching_for_combo m (res::original_combo_list) original_combo_list
    end
  | h::t -> searching_for_combo m t original_combo_list
  | [] -> root ()
    
let rec root () : unit =
  let original_combo_list = [] in
  let input = wait_for_valid_entry () in
  searching_for_combo input original_combo_list original_combo_list
