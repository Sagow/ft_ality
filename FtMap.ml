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

  val cmp : pair_t -> pair_t -> int
  val of_list : pair_t list -> t
  val to_list : t -> pair_t list
  val empty : t
  val merge : pair_t list -> t -> t
  val add : key_t -> value_t -> t -> t
  val find_opt : t -> key_t -> value_t option
  val contains : t -> key_t -> bool
  val iter : (pair_t -> unit) -> t -> unit
  val merge_duplicates : (value_t -> value_t -> value_t) -> t -> t
end

module type MAKE =
  functor (KV : KEYVALUE) -> MAP with
    type key_t = KV.key_t and
    type value_t = KV.value_t

module Make : MAKE =
  functor (KV : KEYVALUE) -> struct
    type key_t = KV.key_t
    type value_t = KV.value_t
    type pair_t = (key_t * value_t)
    type t = pair_t list

    let cmp
      (e1 : pair_t)
      (e2 : pair_t)
      : int =
        KV.cmp (fst e1) (fst e2)

    let of_list
      (lst : pair_t list)
      : t =
      List.sort cmp lst

    let to_list
      (map : t)
      : pair_t list =
      map

    let empty
      : t =
      []

    let merge
      (lst : pair_t list)
      (map : t)
      : t =
      List.merge cmp map lst

    let add
      (key : key_t)
      (value : value_t)
      (map : t)
      : t =
      List.merge cmp map [(key, value)]

    let find_opt
      (map : t)
      (to_find : key_t)
      : value_t option =
      match List.find_opt (fun x -> KV.cmp (fst x) to_find = 0) map with
      | Some(key, value) -> Some(value)
      | None -> None

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

module StringIntMap : (MAP with type key_t := KVStringInt.key_t and type value_t := KVStringInt.value_t) = Make (KVStringInt)

let () =
  let m = StringIntMap.of_list [("Banana", 3);("Banana", 7);("Apple", 12);("Peaches", 4);("Pineapple", 6)] in
  StringIntMap.iter (fun (key, value) -> Printf.printf "%s: %d\n" key value) m; print_endline "";
  let m2 = StringIntMap.merge_duplicates (+) m in
  StringIntMap.iter (fun (key, value) -> Printf.printf "%s: %d\n" key value) m2; print_endline "";
  match StringIntMap.find_opt m2 "Banana" with
  | Some(n) -> Printf.printf "Found %d bananas.\n" n
  | None -> Printf.printf "Found no banana.\n"
