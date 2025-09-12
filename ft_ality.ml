type key = Utils.key

type move = Utils.move
type combo = ((move list) * string)

module KeyMoveMap = Key_mapping.KeyMoveMap

let rec init_sdl unit : Tsdl.Sdl.window =
  match Tsdl.Sdl.init Tsdl.Sdl.Init.events with
  | Error _ -> failwith ("Error while initiating Sdl")
  | Ok () ->
    match Tsdl.Sdl.create_window~w:100 ~h:100 "TTsdl.sdl - keyboard" Tsdl.Sdl.Window.windowed with
      | Error (`Msg e) -> Tsdl.Sdl.quit (); failwith (Printf.sprintf "Window error : %s" e)
      | Ok window -> window

let wait_for_next_move (key_move_map : KeyMoveMap.t) : move =
  let event = Tsdl.Sdl.Event.create () in
  let rec loop () =
    if Tsdl.Sdl.poll_event (Some event) then begin
      match Tsdl.Sdl.Event.(get event typ) with
      | t when t = Tsdl.Sdl.Event.quit -> failwith ("Window closed")
      | t when t = Tsdl.Sdl.Event.key_down ->
          let keycode = Tsdl.Sdl.Event.(get event keyboard_keycode) in
          (* Tsdl.Sdl.log "Key pressed : %d" keycode; *)
          if keycode = Tsdl.Sdl.K.escape then
            failwith ("Escape key detected. Quit.")
          else
            let move = KeyMoveMap.find_opt key_move_map keycode in begin
              match move with
              | Some(m) -> m
              | None -> loop ()
            end
          
      | _ -> loop ()
    end
    else
      loop ()
  in
  loop ()
