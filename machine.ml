type input = string
type character = string
type combo = string
type combo_data = (combo * character)

type event_entry = (input * combo_data list * event_table_opt) and
  event_table = event_entry list and
  event_table_opt = Some of event_table | None

type f = F of (input -> (f * combo_data list))

let rec machine_apply_event (root : event_table) (current : event_table) (event : input) : (f * combo_data list) =
  let rec find_event_entry (event_entries : event_table) (event : input) : event_entry option =
    match event_entries with
    | [] -> None
    | head :: tail -> let (entry_event, _, _) = head in
      if entry_event = event
        then Some(head)
        else find_event_entry tail event
  in
  match find_event_entry current event with (* Use find_event_entry to find entry matching the event in the list *)
  | Some((_, combos, next_table_opt)) -> begin (* If an entry was found *)
    match next_table_opt with (* Do we have a table to continue the combo or is this the last possible hit ? *)
    | Some(next_table) -> (F(machine_apply_event root next_table), combos) (* We can continue, return the function with next table *)
    | None -> (F(machine_apply_event root root), combos) (* It is the end, return fhe function with current = root *)
  end
  | None -> begin (* We did not find any entry for this event *)
    if current == root (* Avoid infinite loop *)
      then (F(machine_apply_event root root), []) (* Just return the function with current = root and no combo *)
      else machine_apply_event root root event (* Try to use event from root for a new combo *)
  end
