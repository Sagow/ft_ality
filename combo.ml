module Combo = struct
  type t =
  {
    character : string;
    attack_name : string;
    move_pattern : Utils.move list;
  }

  let newCombo (c : string) (a : string) (m : Utils.move list) : t =
  {
    character = String.trim c;
    attack_name = String.trim a;
    move_pattern = m;      
  }

  let combo_to_string (c : t) : string =
    let rec move_pattern_to_string (l : Utils.move list) : string =
      match l with
      | h::[] -> Utils.move_to_string h
      | h::t -> Utils.move_to_string h ^ "-" ^ move_pattern_to_string t
      | [] -> ""
    in
    "Character " ^ c.character ^ " can launch " ^ c.attack_name ^ " by doing " ^ (move_pattern_to_string c.move_pattern)

end