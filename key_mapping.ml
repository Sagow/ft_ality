module KeyMoveKV : (FtMap.KEYVALUE with type key_t = Utils.key and type value_t = Utils.move) = struct
  type key_t = Utils.key
  type value_t = Utils.move
  let cmp = Int.compare
end

module KeyMoveMap : (FtMap.MAP with type key_t := KeyMoveKV.key_t and type value_t := KeyMoveKV.value_t) = FtMap.Make (KeyMoveKV)

let rec print_pair (l : KeyMoveMap.pair_t list) =
  match l with 
  | h::t -> print_endline (Utils.move_to_string (snd h) ^ " is " ^ Utils.key_to_string (fst h)); print_pair t
  | [] -> ()

let rec find_key_mapping (l : string list) : KeyMoveMap.t =
  let rec map_key l : (Utils.key * Utils.move) list =
    let recognize_key (s: string) : Utils.key =
      let first_word (s : string) : string =
        String.split_on_char ' ' s |> List.hd
      in
      Utils.string_to_key (first_word s)
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
        (recognize_key h, m)::(map_key t)
        end
    end
    | [] -> []
  in
match l with 
| h::t when (String.trim h) = "key mapping" -> KeyMoveMap.of_list (map_key t)
| h::t -> find_key_mapping t
| [] -> failwith ("key mapping not found. Did you forget to start with \"key mapping\" keyword?")
