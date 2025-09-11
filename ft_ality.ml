type key = Utils.key

type move = Utils.move
type combo = ((move list) * string)

let is_mapped (k : key) : move =
  (* regarder si c'est dans le tableau des keys associées, renvoyer le move associé *)
  match Utils.all_moves with
  | head :: _ -> head
  | [] -> failwith ("Utils.all_moves contain no move !")

let rec wait_for_valid_entry () : move =
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
          match keycode with
          | Tsdl.Sdl.K.Backspace -> is_mapped BACKSPACE
          | Tsdl.Sdl.K.Unknown -> is_mapped None
          | Tsdl.Sdl.K.Tab -> is_mapped TAB
          | Tsdl.Sdl.K.Return -> is_mapped RETURN
          | Tsdl.Sdl.K.Space -> is_mapped SPACE
          | Tsdl.Sdl.K.Exclaim -> is_mapped EXCLAIM
          | Tsdl.Sdl.K.Quotedbl -> is_mapped QUOTEDBL
          | Tsdl.Sdl.K.Hash -> is_mapped HASH
          | Tsdl.Sdl.K.Dollar -> is_mapped DOLLAR
          | Tsdl.Sdl.K.Percent -> is_mapped PERCENT
          | Tsdl.Sdl.K.Ampersand -> is_mapped AMPERSAND
          | Tsdl.Sdl.K.Quote -> is_mapped QUOTE
          | Tsdl.Sdl.K.Leftparen -> is_mapped LEFTPAREN
          | Tsdl.Sdl.K.Rightparen -> is_mapped RIGHTPAREN
          | Tsdl.Sdl.K.Asterisk -> is_mapped ASTERISK
          | Tsdl.Sdl.K.Plus -> is_mapped PLUS
          | Tsdl.Sdl.K.Comma -> is_mapped COMMA
          | Tsdl.Sdl.K.Minus -> is_mapped MINUS
          | Tsdl.Sdl.K.Period -> is_mapped PERIOD
          | Tsdl.Sdl.K.Slash -> is_mapped SLASH
          | Tsdl.Sdl.K.K0 -> is_mapped K0
          | Tsdl.Sdl.K.K1 -> is_mapped K1
          | Tsdl.Sdl.K.K2 -> is_mapped K2
          | Tsdl.Sdl.K.K3 -> is_mapped K3
          | Tsdl.Sdl.K.K4 -> is_mapped K4
          | Tsdl.Sdl.K.K5 -> is_mapped K5
          | Tsdl.Sdl.K.K6 -> is_mapped K6
          | Tsdl.Sdl.K.K7 -> is_mapped K7
          | Tsdl.Sdl.K.K8 -> is_mapped K8
          | Tsdl.Sdl.K.K9 -> is_mapped K9
          | Tsdl.Sdl.K.Colon -> is_mapped COLON
          | Tsdl.Sdl.K.Semicolon -> is_mapped SEMICOLON
          | Tsdl.Sdl.K.Less -> is_mapped LESS
          | Tsdl.Sdl.K.Equals -> is_mapped EQUALS
          | Tsdl.Sdl.K.Greater -> is_mapped GREATER
          | Tsdl.Sdl.K.Question -> is_mapped QUESTION
          | Tsdl.Sdl.K.At -> is_mapped AT
          | Tsdl.Sdl.K.Leftbracket -> is_mapped LEFTBRACKET
          | Tsdl.Sdl.K.Backslash -> is_mapped BACKSLASH
          | Tsdl.Sdl.K.Rightbracket -> is_mapped RIGHTBRACKET
          | Tsdl.Sdl.K.Caret -> is_mapped CARET
          | Tsdl.Sdl.K.Underscore -> is_mapped UNDERSCORE
          | Tsdl.Sdl.K.Backquote -> is_mapped BACKQUOTE
          | Tsdl.Sdl.K.A -> is_mapped A
          | Tsdl.Sdl.K.B -> is_mapped B
          | Tsdl.Sdl.K.C -> is_mapped C
          | Tsdl.Sdl.K.D -> is_mapped D
          | Tsdl.Sdl.K.E -> is_mapped E
          | Tsdl.Sdl.K.F -> is_mapped F
          | Tsdl.Sdl.K.G -> is_mapped G
          | Tsdl.Sdl.K.H -> is_mapped H
          | Tsdl.Sdl.K.I -> is_mapped I
          | Tsdl.Sdl.K.J -> is_mapped J
          | Tsdl.Sdl.K.K -> is_mapped K
          | Tsdl.Sdl.K.L -> is_mapped L
          | Tsdl.Sdl.K.M -> is_mapped M
          | Tsdl.Sdl.K.N -> is_mapped N
          | Tsdl.Sdl.K.O -> is_mapped O
          | Tsdl.Sdl.K.P -> is_mapped P
          | Tsdl.Sdl.K.Q -> is_mapped Q
          | Tsdl.Sdl.K.R -> is_mapped R
          | Tsdl.Sdl.K.S -> is_mapped S
          | Tsdl.Sdl.K.T -> is_mapped T
          | Tsdl.Sdl.K.U -> is_mapped U
          | Tsdl.Sdl.K.V -> is_mapped V
          | Tsdl.Sdl.K.W -> is_mapped W
          | Tsdl.Sdl.K.X -> is_mapped X
          | Tsdl.Sdl.K.Y -> is_mapped Y
          | Tsdl.Sdl.K.Z -> is_mapped Z
          | Tsdl.Sdl.K.F1 -> is_mapped F1
          | Tsdl.Sdl.K.F2 -> is_mapped F2
          | Tsdl.Sdl.K.F3 -> is_mapped F3
          | Tsdl.Sdl.K.F4 -> is_mapped F4
          | Tsdl.Sdl.K.F5 -> is_mapped F5
          | Tsdl.Sdl.K.F6 -> is_mapped F6
          | Tsdl.Sdl.K.F7 -> is_mapped F7
          | Tsdl.Sdl.K.F8 -> is_mapped F8
          | Tsdl.Sdl.K.F9 -> is_mapped F9
          | Tsdl.Sdl.K.F10 -> is_mapped F10
          | Tsdl.Sdl.K.F11 -> is_mapped F11
          | Tsdl.Sdl.K.F12 -> is_mapped F12
          | Tsdl.Sdl.K.Insert -> is_mapped INSERT
          | Tsdl.Sdl.K.Home -> is_mapped HOME
          | Tsdl.Sdl.K.Pageup -> is_mapped PAGEUP
          | Tsdl.Sdl.K.Delete -> is_mapped DELETE
          | Tsdl.Sdl.K.End -> is_mapped END
          | Tsdl.Sdl.K.Pagedown -> is_mapped PAGEDOWN
          | Tsdl.Sdl.K.Right -> is_mapped RIGHT
          | Tsdl.Sdl.K.Left -> is_mapped LEFT
          | Tsdl.Sdl.K.Down -> is_mapped DOWN
          | Tsdl.Sdl.K.Up -> is_mapped UP
          | Tsdl.Sdl.K.Kp_divide -> is_mapped KP_DIVIDE
          | Tsdl.Sdl.K.Kp_multiply -> is_mapped KP_MULTIPLY
          | Tsdl.Sdl.K.Kp_minus -> is_mapped KP_MINUS
          | Tsdl.Sdl.K.Kp_plus -> is_mapped KP_PLUS
          | Tsdl.Sdl.K.Kp_enter -> is_mapped KP_ENTER
          | Tsdl.Sdl.K.Kp_1 -> is_mapped KP_1
          | Tsdl.Sdl.K.Kp_2 -> is_mapped KP_2
          | Tsdl.Sdl.K.Kp_3 -> is_mapped KP_3
          | Tsdl.Sdl.K.Kp_4 -> is_mapped KP_4
          | Tsdl.Sdl.K.Kp_5 -> is_mapped KP_5
          | Tsdl.Sdl.K.Kp_6 -> is_mapped KP_6
          | Tsdl.Sdl.K.Kp_7 -> is_mapped KP_7
          | Tsdl.Sdl.K.Kp_8 -> is_mapped KP_8
          | Tsdl.Sdl.K.Kp_9 -> is_mapped KP_9
          | Tsdl.Sdl.K.Kp_0 -> is_mapped KP_0

          | Tsdl.Sdl.K.Escape ->
            begin
              Tsdl.Sdl.log "Touche Escape détectée. Quit.";
              (* Quitter proprement *)
            end
          
      | _ -> loop ()
    end else
      (* Pas d'événement : attendre un peu et continuer *)
      let () = Tsdl.Sdl.delay 10l in
      loop ()
  in
  loop ()

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
  | [] -> ()
    
let rec root () : unit =
  match Tsdl.Sdl.init Tsdl.Sdl.Init.events with
  | Error _ -> Tsdl.Sdl.log "Error while init"
  | Ok () ->
    match Tsdl.Sdl.create_window~w:100 ~h:100 "TTsdl.sdl - clavier" Tsdl.Sdl.Window.windowed with
      | Error (`Msg e) -> Tsdl.Sdl.log "Erreur fenêtre : %s" e; Tsdl.Sdl.quit ()
      | Ok window ->
        begin
          wait_for_valid_entry  ();
          Tsdl.Sdl.destroy_window window;
          Tsdl.Sdl.quit ()
        end
