
let rec print_pair (l : (Utils.move * Utils.key) list) =
  match l with 
  | h::t -> print_endline (Utils.move_to_string (fst h) ^ " is " ^ Utils.key_to_string (snd h)); print_pair t
  | [] -> ()

let rec find_key_mapping (l : string list) : (Utils.move * Utils.key) list =
  let rec map_key l =
    let recognize_key (s: string) : Utils.key =
      let first_word (s : string) : string =
        String.split_on_char ' ' s |> List.hd
      in
      let k = Utils.string_to_key (first_word s) in 
      match k with
      | Utils.None -> failwith ("the key wasn't recognized : " ^ (first_word s))
      | _ -> k
    in
    let last_word (s: string) : string =
      let rec get_last (l : string list) : string =
        match l with
        | h::[] -> h
        | h::t -> get_last t
        | [] -> ""
      in
      String.split_on_char ' ' s |> get_last
    in
    match l with
    | h::t when (String.trim h) = "combo definitions" -> []
    | h::t -> begin
      let lw = last_word h in
      if (String.length lw = 0)then map_key t
      else
        begin
        let m = last_word h |> Utils.string_to_move in
        if m = Utils.None then map_key t
        else (m, recognize_key h)::(map_key t)
        end
    end
    | [] -> []
  in
match l with 
| h::t when (String.trim h) = "key mapping" -> map_key t
| h::t -> find_key_mapping t
| [] -> print_endline "key mapping not found. Did you forget to start with \"key mapping\" keyword?"; []
