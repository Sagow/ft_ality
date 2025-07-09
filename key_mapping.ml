
let rec print_pair (l : (Utils.move * Utils.key) list) =
  match l with 
  | h::t -> print_endline (Utils.move_to_string (fst h) ^ " is " ^ Utils.key_to_string (snd h)); print_pair t
  | [] -> ()

let rec find_key_mapping (l : string list) : (Utils.move * Utils.key) list =
  let rec map_key l =
    let recognize_key (s: string) : Utils.key =
      match s with 
      | x when (String.starts_with ~prefix:"BACKSPACE -> " x) -> SDLK_BACKSPACE
      | x when (String.starts_with ~prefix:"TAB -> " x) -> SDLK_TAB
      | x when (String.starts_with ~prefix:"CLEAR -> " x) -> SDLK_CLEAR
      | x when (String.starts_with ~prefix:"RETURN -> " x) -> SDLK_RETURN
      | x when (String.starts_with ~prefix:"PAUSE -> " x) -> SDLK_PAUSE
      | x when (String.starts_with ~prefix:"ESCAPE -> " x) -> SDLK_ESCAPE
      | x when (String.starts_with ~prefix:"SPACE -> " x) -> SDLK_SPACE
      | x when (String.starts_with ~prefix:"EXCLAIM -> " x) -> SDLK_EXCLAIM
      | x when (String.starts_with ~prefix:"QUOTEDBL -> " x) -> SDLK_QUOTEDBL
      | x when (String.starts_with ~prefix:"HASH -> " x) -> SDLK_HASH
      | x when (String.starts_with ~prefix:"DOLLAR -> " x) -> SDLK_DOLLAR
      | x when (String.starts_with ~prefix:"AMPERSAND -> " x) -> SDLK_AMPERSAND
      | x when (String.starts_with ~prefix:"QUOTE -> " x) -> SDLK_QUOTE
      | x when (String.starts_with ~prefix:"LEFTPAREN -> " x) -> SDLK_LEFTPAREN
      | x when (String.starts_with ~prefix:"RIGHTPAREN -> " x) -> SDLK_RIGHTPAREN
      | x when (String.starts_with ~prefix:"ASTERISK -> " x) -> SDLK_ASTERISK
      | x when (String.starts_with ~prefix:"PLUS -> " x) -> SDLK_PLUS
      | x when (String.starts_with ~prefix:"COMMA -> " x) -> SDLK_COMMA
      | x when (String.starts_with ~prefix:"MINUS -> " x) -> SDLK_MINUS
      | x when (String.starts_with ~prefix:"PERIOD -> " x) -> SDLK_PERIOD
      | x when (String.starts_with ~prefix:"SLASH -> " x) -> SDLK_SLASH
      | x when (String.starts_with ~prefix:"0 -> " x) -> SDLK_0
      | x when (String.starts_with ~prefix:"1 -> " x) -> SDLK_1
      | x when (String.starts_with ~prefix:"2 -> " x) -> SDLK_2
      | x when (String.starts_with ~prefix:"3 -> " x) -> SDLK_3
      | x when (String.starts_with ~prefix:"4 -> " x) -> SDLK_4
      | x when (String.starts_with ~prefix:"5 -> " x) -> SDLK_5
      | x when (String.starts_with ~prefix:"6 -> " x) -> SDLK_6
      | x when (String.starts_with ~prefix:"7 -> " x) -> SDLK_7
      | x when (String.starts_with ~prefix:"8 -> " x) -> SDLK_8
      | x when (String.starts_with ~prefix:"9 -> " x) -> SDLK_9
      | x when (String.starts_with ~prefix:"COLON -> " x) -> SDLK_COLON
      | x when (String.starts_with ~prefix:"SEMICOLON -> " x) -> SDLK_SEMICOLON
      | x when (String.starts_with ~prefix:"LESS -> " x) -> SDLK_LESS
      | x when (String.starts_with ~prefix:"EQUALS -> " x) -> SDLK_EQUALS
      | x when (String.starts_with ~prefix:"GREATER -> " x) -> SDLK_GREATER
      | x when (String.starts_with ~prefix:"QUESTION -> " x) -> SDLK_QUESTION
      | x when (String.starts_with ~prefix:"AT -> " x) -> SDLK_AT
      | x when (String.starts_with ~prefix:"LEFTBRACKET -> " x) -> SDLK_LEFTBRACKET
      | x when (String.starts_with ~prefix:"BACKSLASH -> " x) -> SDLK_BACKSLASH
      | x when (String.starts_with ~prefix:"RIGHTBRACKET -> " x) -> SDLK_RIGHTBRACKET
      | x when (String.starts_with ~prefix:"CARET -> " x) -> SDLK_CARET
      | x when (String.starts_with ~prefix:"UNDERSCORE -> " x) -> SDLK_UNDERSCORE
      | x when (String.starts_with ~prefix:"BACKQUOTE -> " x) -> SDLK_BACKQUOTE
      | x when (String.starts_with ~prefix:"a -> " x) -> SDLK_a
      | x when (String.starts_with ~prefix:"b -> " x) -> SDLK_b
      | x when (String.starts_with ~prefix:"c -> " x) -> SDLK_c
      | x when (String.starts_with ~prefix:"d -> " x) -> SDLK_d
      | x when (String.starts_with ~prefix:"e -> " x) -> SDLK_e
      | x when (String.starts_with ~prefix:"f -> " x) -> SDLK_f
      | x when (String.starts_with ~prefix:"g -> " x) -> SDLK_g
      | x when (String.starts_with ~prefix:"h -> " x) -> SDLK_h
      | x when (String.starts_with ~prefix:"i -> " x) -> SDLK_i
      | x when (String.starts_with ~prefix:"j -> " x) -> SDLK_j
      | x when (String.starts_with ~prefix:"k -> " x) -> SDLK_k
      | x when (String.starts_with ~prefix:"l -> " x) -> SDLK_l
      | x when (String.starts_with ~prefix:"m -> " x) -> SDLK_m
      | x when (String.starts_with ~prefix:"n -> " x) -> SDLK_n
      | x when (String.starts_with ~prefix:"o -> " x) -> SDLK_o
      | x when (String.starts_with ~prefix:"p -> " x) -> SDLK_p
      | x when (String.starts_with ~prefix:"q -> " x) -> SDLK_q
      | x when (String.starts_with ~prefix:"r -> " x) -> SDLK_r
      | x when (String.starts_with ~prefix:"s -> " x) -> SDLK_s
      | x when (String.starts_with ~prefix:"t -> " x) -> SDLK_t
      | x when (String.starts_with ~prefix:"u -> " x) -> SDLK_u
      | x when (String.starts_with ~prefix:"v -> " x) -> SDLK_v
      | x when (String.starts_with ~prefix:"w -> " x) -> SDLK_w
      | x when (String.starts_with ~prefix:"x -> " x) -> SDLK_x
      | x when (String.starts_with ~prefix:"y -> " x) -> SDLK_y
      | x when (String.starts_with ~prefix:"z -> " x) -> SDLK_z
      | x when (String.starts_with ~prefix:"DELETE -> " x) -> SDLK_DELETE
      | x when (String.starts_with ~prefix:"KP0 -> " x) -> SDLK_KP0
      | x when (String.starts_with ~prefix:"KP1 -> " x) -> SDLK_KP1
      | x when (String.starts_with ~prefix:"KP2 -> " x) -> SDLK_KP2
      | x when (String.starts_with ~prefix:"KP3 -> " x) -> SDLK_KP3
      | x when (String.starts_with ~prefix:"KP4 -> " x) -> SDLK_KP4
      | x when (String.starts_with ~prefix:"KP5 -> " x) -> SDLK_KP5
      | x when (String.starts_with ~prefix:"KP6 -> " x) -> SDLK_KP6
      | x when (String.starts_with ~prefix:"KP7 -> " x) -> SDLK_KP7
      | x when (String.starts_with ~prefix:"KP8 -> " x) -> SDLK_KP8
      | x when (String.starts_with ~prefix:"KP9 -> " x) -> SDLK_KP9
      | x when (String.starts_with ~prefix:"KP_PERIOD -> " x) -> SDLK_KP_PERIOD
      | x when (String.starts_with ~prefix:"KP_DIVIDE -> " x) -> SDLK_KP_DIVIDE
      | x when (String.starts_with ~prefix:"KP_MULTIPLY -> " x) -> SDLK_KP_MULTIPLY
      | x when (String.starts_with ~prefix:"KP_MINUS -> " x) -> SDLK_KP_MINUS
      | x when (String.starts_with ~prefix:"KP_PLUS -> " x) -> SDLK_KP_PLUS
      | x when (String.starts_with ~prefix:"KP_ENTER -> " x) -> SDLK_KP_ENTER
      | x when (String.starts_with ~prefix:"KP_EQUALS -> " x) -> SDLK_KP_EQUALS
      | x when (String.starts_with ~prefix:"UP -> " x) -> SDLK_UP
      | x when (String.starts_with ~prefix:"DOWN -> " x) -> SDLK_DOWN
      | x when (String.starts_with ~prefix:"RIGHT -> " x) -> SDLK_RIGHT
      | x when (String.starts_with ~prefix:"LEFT -> " x) -> SDLK_LEFT
      | x when (String.starts_with ~prefix:"INSERT -> " x) -> SDLK_INSERT
      | x when (String.starts_with ~prefix:"HOME -> " x) -> SDLK_HOME
      | x when (String.starts_with ~prefix:"END -> " x) -> SDLK_END
      | x when (String.starts_with ~prefix:"PAGEUP -> " x) -> SDLK_PAGEUP
      | x when (String.starts_with ~prefix:"PAGEDOWN -> " x) -> SDLK_PAGEDOWN
      | x when (String.starts_with ~prefix:"F1 -> " x) -> SDLK_F1
      | x when (String.starts_with ~prefix:"F2 -> " x) -> SDLK_F2
      | x when (String.starts_with ~prefix:"F3 -> " x) -> SDLK_F3
      | x when (String.starts_with ~prefix:"F4 -> " x) -> SDLK_F4
      | x when (String.starts_with ~prefix:"F5 -> " x) -> SDLK_F5
      | x when (String.starts_with ~prefix:"F6 -> " x) -> SDLK_F6
      | x when (String.starts_with ~prefix:"F7 -> " x) -> SDLK_F7
      | x when (String.starts_with ~prefix:"F8 -> " x) -> SDLK_F8
      | x when (String.starts_with ~prefix:"F9 -> " x) -> SDLK_F9
      | x when (String.starts_with ~prefix:"F10 -> " x) -> SDLK_F10
      | x when (String.starts_with ~prefix:"F11 -> " x) -> SDLK_F11
      | x when (String.starts_with ~prefix:"F12 -> " x) -> SDLK_F12
      | x when (String.starts_with ~prefix:"F13 -> " x) -> SDLK_F13
      | x when (String.starts_with ~prefix:"F14 -> " x) -> SDLK_F14
      | x when (String.starts_with ~prefix:"F15 -> " x) -> SDLK_F15
      | x when (String.starts_with ~prefix:"NUMLOCK -> " x) -> SDLK_NUMLOCK
      | x when (String.starts_with ~prefix:"CAPSLOCK -> " x) -> SDLK_CAPSLOCK
      | x when (String.starts_with ~prefix:"SCROLLOCK -> " x) -> SDLK_SCROLLOCK
      | x when (String.starts_with ~prefix:"RSHIFT -> " x) -> SDLK_RSHIFT
      | x when (String.starts_with ~prefix:"LSHIFT -> " x) -> SDLK_LSHIFT
      | x when (String.starts_with ~prefix:"RCTRL -> " x) -> SDLK_RCTRL
      | x when (String.starts_with ~prefix:"LCTRL -> " x) -> SDLK_LCTRL
      | x when (String.starts_with ~prefix:"RALT -> " x) -> SDLK_RALT
      | x when (String.starts_with ~prefix:"LALT -> " x) -> SDLK_LALT
      | x when (String.starts_with ~prefix:"RMETA -> " x) -> SDLK_RMETA
      | x when (String.starts_with ~prefix:"LMETA -> " x) -> SDLK_LMETA
      | x when (String.starts_with ~prefix:"LSUPER -> " x) -> SDLK_LSUPER
      | x when (String.starts_with ~prefix:"RSUPER -> " x) -> SDLK_RSUPER
      | x when (String.starts_with ~prefix:"MODE -> " x) -> SDLK_MODE
      | x when (String.starts_with ~prefix:"HELP -> " x) -> SDLK_HELP
      | x when (String.starts_with ~prefix:"PRINT -> " x) -> SDLK_PRINT
      | x when (String.starts_with ~prefix:"SYSREQ -> " x) -> SDLK_SYSREQ
      | x when (String.starts_with ~prefix:"BREAK -> " x) -> SDLK_BREAK
      | x when (String.starts_with ~prefix:"MENU -> " x) -> SDLK_MENU
      | x when (String.starts_with ~prefix:"POWER -> " x) -> SDLK_POWER
      | x when (String.starts_with ~prefix:"EURO -> " x) -> SDLK_EURO
      | x -> failwith ("the key wasn't recognized : " ^ x)
    in
    match l with
    | h::t when String.ends_with ~suffix:"Block" h -> (Utils.Block, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Down" h -> (Utils.Down, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Flip Stance" h -> (Utils.FlipStance, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Left" h -> (Utils.Left, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Right" h -> (Utils.Right, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Tag" h -> (Utils.Tag, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Throw" h -> (Utils.Throw, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Up" h -> (Utils.Up, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"[BK]" h -> (Utils.BK, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"[BP]" h -> (Utils.BP, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"[FK]" h -> (Utils.FK, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"[FP]" h -> (Utils.FP, recognize_key h)::(map_key t)
    | h::t when (String.trim h) = "combo definitions" -> []
    | h::t -> map_key t
    | [] -> []
  in
match l with 
| h::t when (String.trim h) = "key mapping" -> map_key t
| h::t -> find_key_mapping t
| [] -> print_endline "key mapping not found. Did you forget to start with \"key mapping\" keyword?"; []
