
let check_validity_of_file (f : string) : bool =
  if (Sys.file_exists (f) = false) then
    begin
      print_endline ("The file " ^ f ^ " doesn't exist");
      false
    end
  else if (Sys.is_regular_file (f) = false) then
    begin
      print_endline ("The file " ^ f ^ " isn't a regular file");
      false
    end
  else true

let get_file_content (f : string) : string list =
  let rec get_next_line (i : in_channel) (l : string list) : string list =
    try
      begin
        let new_line = input_line i in
        get_next_line i (l@[new_line])
      end
    with
    | End_of_file -> l
    | _ -> failwith "An error occured during the file reading process"
  in
  try
    let i = open_in f in
    get_next_line i []
  with _ -> failwith ("Failed to open the file " ^ f)



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
    begin
    (* let rec loop l =
      match l with
      | h::t -> print_endline (key_to_string h); loop t
      | [] -> ()
    in
    loop (snd split);
    loop (List.sort_uniq (fun x y -> compare x y) (snd split)); *)
    failwith "At least one key has been used twice when defining moves"
    end

let () =
  if (Array.length Sys.argv) <> 2 then
    print_endline ("Usage : " ^ Sys.executable_name ^ " grammar_file")
  else if (check_validity_of_file Sys.argv.(1)) then
    let l = get_file_content Sys.argv.(1) in
    if l <> [] then
      try
        begin
          let data = Key_mapping.find_key_mapping l in
          check_validity_of_mapping data;
          Key_mapping.print_pair data
        end 
      with
      | Failure msg -> print_endline msg
      | _ -> print_endline "An unknown exception occured"
    else ()
  else ()
