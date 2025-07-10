module type KEYVALUE = sig
  type key_t
  type value_t
  val cmp : key_t -> key_t -> int
end

module type MAP = sig
  type key_t
  type value_t
  type pair_t = (key_t * value_t)
  type t = pair_t list

  val from_list : pair_t list -> t
  val find_opt : t -> key_t -> value_t option
  val contains : t -> key_t -> bool
  val iter : (pair_t -> unit) -> t -> unit
  val merge_duplicates : (value_t -> value_t -> value_t) -> t -> t
end

module type MAKEMAP =
  functor (KV : KEYVALUE) -> MAP with
    type key_t = KV.key_t and
    type value_t = KV.value_t

module MakeMap : MAKEMAP =
  functor (KV : KEYVALUE) -> struct
    type key_t = KV.key_t
    type value_t = KV.value_t
    type pair_t = (key_t * value_t)
    type t = pair_t list

    let from_list lst =
      List.sort (fun e1 e2 -> KV.cmp (fst e1) (fst e2)) lst

    let find_opt
      (map : t)
      (to_find : key_t)
      : value_t option =
      let rec find_opt_rec
        (start : int)
        (len : int)
        : value_t option =
          if len = 0
          then None
          else begin
            let middle = len / 2 + start in
            let mid_elem = List.nth map middle in
            let cmp_res : int = KV.cmp to_find (fst mid_elem) in
            match cmp_res with
            | n when n > 0 -> find_opt_rec (middle + 1) ((len - 1) / 2)
            | n when n < 0 -> find_opt_rec start (len / 2)
            | _ -> Some(snd mid_elem) (* case 0 *)
          end
      in
      find_opt_rec 0 (List.length map)

    let contains
      (map : t)
      (to_find : key_t)
      : bool =
      match find_opt map to_find with
      | Some(_) -> true
      | None -> false

    let iter = List.iter

    let merge_duplicates
      (f : (value_t -> value_t -> value_t))
      (m : t)
      : t =
      let rec loop
        (acc : pair_t list)
        (cur : pair_t)
        (to_merge : pair_t list)
        : pair_t list =
        match to_merge with
        | [] -> cur :: acc
        | head :: tail ->
          if KV.cmp (fst cur) (fst head) = 0
          then
            let new_value = f (snd cur) (snd head) in
            let new_pair = ((fst cur), new_value) in
            loop acc new_pair tail
          else
            loop (cur :: acc) head tail
      in
      match m with
      | [] -> m
      | head :: tail -> List.rev (loop [] head tail)
  end

module KVStringInt : (KEYVALUE with type key_t = string and type value_t = int) = struct
  type key_t = string
  type value_t = int
  let cmp = String.compare
end

module StringIntMap : (MAP with type key_t := KVStringInt.key_t and type value_t := KVStringInt.value_t) = MakeMap (KVStringInt)

let () =
  let m = StringIntMap.from_list [("Banana", 3);("Banana", 7);("Apple", 12);("Peaches", 4);("Pineapple", 6)] in
  StringIntMap.iter (fun (key, value) -> Printf.printf "%s: %d\n" key value) m; print_endline "";
  let m2 = StringIntMap.merge_duplicates (+) m in
  StringIntMap.iter (fun (key, value) -> Printf.printf "%s: %d\n" key value) m2; print_endline "";
  match StringIntMap.find_opt m2 "Banana" with
  | Some(n) -> Printf.printf "Found %d bananas.\n" n
  | None -> Printf.printf "Found no banana.\n"
