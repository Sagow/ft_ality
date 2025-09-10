let rec event_loop () =
  let event = Tsdl.Sdl.Event.create () in
  let rec loop () =
    if Tsdl.Sdl.poll_event (Some event) then begin
      match Tsdl.Sdl.Event.(get event typ) with
      | t when t = Tsdl.Sdl.Event.quit -> (* Fenêtre fermée *)
          Tsdl.Sdl.log "Fermeture de la fenêtre...";
          ()
      | t when t = Tsdl.Sdl.Event.key_down ->
          let keycode = Tsdl.Sdl.Event.(get event keyboard_keycode) in
          Tsdl.Sdl.log "Touche appuyée : %d" keycode;
          if keycode = Tsdl.Sdl.K.escape then (
            Tsdl.Sdl.log "Touche Escape détectée. Quit.";
            ()
          ) else loop ()
      | _ -> loop ()
    end else
      (* Pas d'événement : attendre un peu et continuer *)
      let () = Tsdl.Sdl.delay 10l in
      loop ()
  in
  loop ()

let () =
  match Tsdl.Sdl.init Tsdl.Sdl.Init.video with
  | Error (`Msg e) -> Tsdl.Sdl.log "Erreur d'init Tsdl.SDL : %s" e
  | Ok () ->
      match Tsdl.Sdl.create_window ~w:100 ~h:100 "TTsdl.sdl - clavier" Tsdl.Sdl.Window.windowed with
      | Error (`Msg e) -> Tsdl.Sdl.log "Erreur fenêtre : %s" e; Tsdl.Sdl.quit ()
      | Ok window ->
          event_loop ();
          Tsdl.Sdl.destroy_window window;
          Tsdl.Sdl.quit ()
