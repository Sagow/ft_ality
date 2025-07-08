type move = Block | Down | FlipStance | Left | Right | Tag | Throw | Up | BK | BP | FK | FP | None
type key = SDLK_BACKSPACE | SDLK_TAB | SDLK_CLEAR | SDLK_RETURN | SDLK_PAUSE | SDLK_ESCAPE | SDLK_SPACE | SDLK_EXCLAIM | SDLK_QUOTEDBL | SDLK_HASH | SDLK_DOLLAR | SDLK_AMPERSAND | SDLK_QUOTE | SDLK_LEFTPAREN | SDLK_RIGHTPAREN | SDLK_ASTERISK | SDLK_PLUS | SDLK_COMMA | SDLK_MINUS | SDLK_PERIOD | SDLK_SLASH | SDLK_0 | SDLK_1 | SDLK_2 | SDLK_3 | SDLK_4 | SDLK_5 | SDLK_6 | SDLK_7 | SDLK_8 | SDLK_9 | SDLK_COLON | SDLK_SEMICOLON | SDLK_LESS | SDLK_EQUALS | SDLK_GREATER | SDLK_QUESTION | SDLK_AT | SDLK_LEFTBRACKET | SDLK_BACKSLASH | SDLK_RIGHTBRACKET | SDLK_CARET | SDLK_UNDERSCORE | SDLK_BACKQUOTE | SDLK_a | SDLK_b | SDLK_c | SDLK_d | SDLK_e | SDLK_f | SDLK_g | SDLK_h | SDLK_i | SDLK_j | SDLK_k | SDLK_l | SDLK_m | SDLK_n | SDLK_o | SDLK_p | SDLK_q | SDLK_r | SDLK_s | SDLK_t | SDLK_u | SDLK_v | SDLK_w | SDLK_x | SDLK_y | SDLK_z | SDLK_DELETE | SDLK_KP0 | SDLK_KP1 | SDLK_KP2 | SDLK_KP3 | SDLK_KP4 | SDLK_KP5 | SDLK_KP6 | SDLK_KP7 | SDLK_KP8 | SDLK_KP9 | SDLK_KP_PERIOD | SDLK_KP_DIVIDE | SDLK_KP_MULTIPLY | SDLK_KP_MINUS | SDLK_KP_PLUS | SDLK_KP_ENTER | SDLK_KP_EQUALS | SDLK_UP | SDLK_DOWN | SDLK_RIGHT | SDLK_LEFT | SDLK_INSERT | SDLK_HOME | SDLK_END | SDLK_PAGEUP | SDLK_PAGEDOWN | SDLK_F1 | SDLK_F2 | SDLK_F3 | SDLK_F4 | SDLK_F5 | SDLK_F6 | SDLK_F7 | SDLK_F8 | SDLK_F9 | SDLK_F10 | SDLK_F11 | SDLK_F12 | SDLK_F13 | SDLK_F14 | SDLK_F15 | SDLK_NUMLOCK | SDLK_CAPSLOCK | SDLK_SCROLLOCK | SDLK_RSHIFT | SDLK_LSHIFT | SDLK_RCTRL | SDLK_LCTRL | SDLK_RALT | SDLK_LALT | SDLK_RMETA | SDLK_LMETA | SDLK_LSUPER | SDLK_RSUPER | SDLK_MODE | SDLK_HELP | SDLK_PRINT | SDLK_SYSREQ | SDLK_BREAK | SDLK_MENU | SDLK_POWER | SDLK_EURO

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

let move_to_string (m : move) : string =
  match m with
  | Block -> "block"
  | Down -> "down"
  | FlipStance -> "flip stance"
  | Left -> "left"
  | Right -> "right"
  | Tag -> "tag"
  | Throw -> "throw"
  | Up -> "up"
  | BK -> "BK"
  | BP -> "BP"
  | FK -> "FK"
  | FP -> "FP"
  | _ -> "none"

let key_to_string (k : key) : string =
  match k with
  | SDLK_BACKSPACE -> "backspace"
  | SDLK_TAB -> "tab"
  | SDLK_CLEAR -> "clear"
  | SDLK_RETURN -> "return"
  | SDLK_PAUSE -> "pause"
  | SDLK_ESCAPE -> "escape"
  | SDLK_SPACE -> "space"
  | SDLK_EXCLAIM -> "exclaim"
  | SDLK_QUOTEDBL -> "quotedbl"
  | SDLK_HASH -> "hash"
  | SDLK_DOLLAR -> "dollar"
  | SDLK_AMPERSAND -> "ampersand"
  | SDLK_QUOTE -> "quote"
  | SDLK_LEFTPAREN -> "leftparen"
  | SDLK_RIGHTPAREN -> "rightparen"
  | SDLK_ASTERISK -> "asterisk"
  | SDLK_PLUS -> "plus"
  | SDLK_COMMA -> "comma"
  | SDLK_MINUS -> "minus"
  | SDLK_PERIOD -> "period"
  | SDLK_SLASH -> "slash"
  | SDLK_0 -> "0"
  | SDLK_1 -> "1"
  | SDLK_2 -> "2"
  | SDLK_3 -> "3"
  | SDLK_4 -> "4"
  | SDLK_5 -> "5"
  | SDLK_6 -> "6"
  | SDLK_7 -> "7"
  | SDLK_8 -> "8"
  | SDLK_9 -> "9"
  | SDLK_COLON -> "colon"
  | SDLK_SEMICOLON -> "semicolon"
  | SDLK_LESS -> "less"
  | SDLK_EQUALS -> "equals"
  | SDLK_GREATER -> "greater"
  | SDLK_QUESTION -> "question"
  | SDLK_AT -> "at"
  | SDLK_LEFTBRACKET -> "leftbracket"
  | SDLK_BACKSLASH -> "backslash"
  | SDLK_RIGHTBRACKET -> "rightbracket"
  | SDLK_CARET -> "caret"
  | SDLK_UNDERSCORE -> "underscore"
  | SDLK_BACKQUOTE -> "backquote"
  | SDLK_a -> "a"
  | SDLK_b -> "b"
  | SDLK_c -> "c"
  | SDLK_d -> "d"
  | SDLK_e -> "e"
  | SDLK_f -> "f"
  | SDLK_g -> "g"
  | SDLK_h -> "h"
  | SDLK_i -> "i"
  | SDLK_j -> "j"
  | SDLK_k -> "k"
  | SDLK_l -> "l"
  | SDLK_m -> "m"
  | SDLK_n -> "n"
  | SDLK_o -> "o"
  | SDLK_p -> "p"
  | SDLK_q -> "q"
  | SDLK_r -> "r"
  | SDLK_s -> "s"
  | SDLK_t -> "t"
  | SDLK_u -> "u"
  | SDLK_v -> "v"
  | SDLK_w -> "w"
  | SDLK_x -> "x"
  | SDLK_y -> "y"
  | SDLK_z -> "z"
  | SDLK_DELETE -> "delete"
  | SDLK_KP0 -> "kp0"
  | SDLK_KP1 -> "kp1"
  | SDLK_KP2 -> "kp2"
  | SDLK_KP3 -> "kp3"
  | SDLK_KP4 -> "kp4"
  | SDLK_KP5 -> "kp5"
  | SDLK_KP6 -> "kp6"
  | SDLK_KP7 -> "kp7"
  | SDLK_KP8 -> "kp8"
  | SDLK_KP9 -> "kp9"
  | SDLK_KP_PERIOD -> "kp_period"
  | SDLK_KP_DIVIDE -> "kp_divide"
  | SDLK_KP_MULTIPLY -> "kp_multiply"
  | SDLK_KP_MINUS -> "kp_minus"
  | SDLK_KP_PLUS -> "kp_plus"
  | SDLK_KP_ENTER -> "kp_enter"
  | SDLK_KP_EQUALS -> "kp_equals"
  | SDLK_UP -> "up"
  | SDLK_DOWN -> "down"
  | SDLK_RIGHT -> "right"
  | SDLK_LEFT -> "left"
  | SDLK_INSERT -> "insert"
  | SDLK_HOME -> "home"
  | SDLK_END -> "end"
  | SDLK_PAGEUP -> "pageup"
  | SDLK_PAGEDOWN -> "pagedown"
  | SDLK_F1 -> "f1"
  | SDLK_F2 -> "f2"
  | SDLK_F3 -> "f3"
  | SDLK_F4 -> "f4"
  | SDLK_F5 -> "f5"
  | SDLK_F6 -> "f6"
  | SDLK_F7 -> "f7"
  | SDLK_F8 -> "f8"
  | SDLK_F9 -> "f9"
  | SDLK_F10 -> "f10"
  | SDLK_F11 -> "f11"
  | SDLK_F12 -> "f12"
  | SDLK_F13 -> "f13"
  | SDLK_F14 -> "f14"
  | SDLK_F15 -> "f15"
  | SDLK_NUMLOCK -> "numlock"
  | SDLK_CAPSLOCK -> "capslock"
  | SDLK_SCROLLOCK -> "scrollock"
  | SDLK_RSHIFT -> "rshift"
  | SDLK_LSHIFT -> "lshift"
  | SDLK_RCTRL -> "rctrl"
  | SDLK_LCTRL -> "lctrl"
  | SDLK_RALT -> "ralt"
  | SDLK_LALT -> "lalt"
  | SDLK_RMETA -> "rmeta"
  | SDLK_LMETA -> "lmeta"
  | SDLK_LSUPER -> "lsuper"
  | SDLK_RSUPER -> "rsuper"
  | SDLK_MODE -> "mode"
  | SDLK_HELP -> "help"
  | SDLK_PRINT -> "print"
  | SDLK_SYSREQ -> "sysreq"
  | SDLK_BREAK -> "break"
  | SDLK_MENU -> "menu"
  | SDLK_POWER -> "power"
  | SDLK_EURO -> "euro"


let rec test_print_list (l : string list) =
  match l with
  | h::t -> print_endline h; test_print_list t
  | [] -> print_endline "end of file"

let rec print_pair (l : (move * key) list) =
  match l with 
  | h::t -> print_endline (move_to_string (fst h) ^ " is " ^ key_to_string (snd h)); print_pair t
  | [] -> ()

let rec find_key_mapping (l : string list) : (move * key) list =
  let rec map_key l =
    let recognize_key (s: string) : key =
      match s with 
      | x when (String.starts_with ~prefix:"BACKSPACE" x) -> SDLK_BACKSPACE
      | x when (String.starts_with ~prefix:"TAB" x) -> SDLK_TAB
      | x when (String.starts_with ~prefix:"CLEAR" x) -> SDLK_CLEAR
      | x when (String.starts_with ~prefix:"RETURN" x) -> SDLK_RETURN
      | x when (String.starts_with ~prefix:"PAUSE" x) -> SDLK_PAUSE
      | x when (String.starts_with ~prefix:"ESCAPE" x) -> SDLK_ESCAPE
      | x when (String.starts_with ~prefix:"SPACE" x) -> SDLK_SPACE
      | x when (String.starts_with ~prefix:"EXCLAIM" x) -> SDLK_EXCLAIM
      | x when (String.starts_with ~prefix:"QUOTEDBL" x) -> SDLK_QUOTEDBL
      | x when (String.starts_with ~prefix:"HASH" x) -> SDLK_HASH
      | x when (String.starts_with ~prefix:"DOLLAR" x) -> SDLK_DOLLAR
      | x when (String.starts_with ~prefix:"AMPERSAND" x) -> SDLK_AMPERSAND
      | x when (String.starts_with ~prefix:"QUOTE" x) -> SDLK_QUOTE
      | x when (String.starts_with ~prefix:"LEFTPAREN" x) -> SDLK_LEFTPAREN
      | x when (String.starts_with ~prefix:"RIGHTPAREN" x) -> SDLK_RIGHTPAREN
      | x when (String.starts_with ~prefix:"ASTERISK" x) -> SDLK_ASTERISK
      | x when (String.starts_with ~prefix:"PLUS" x) -> SDLK_PLUS
      | x when (String.starts_with ~prefix:"COMMA" x) -> SDLK_COMMA
      | x when (String.starts_with ~prefix:"MINUS" x) -> SDLK_MINUS
      | x when (String.starts_with ~prefix:"PERIOD" x) -> SDLK_PERIOD
      | x when (String.starts_with ~prefix:"SLASH" x) -> SDLK_SLASH
      | x when (String.starts_with ~prefix:"0" x) -> SDLK_0
      | x when (String.starts_with ~prefix:"1" x) -> SDLK_1
      | x when (String.starts_with ~prefix:"2" x) -> SDLK_2
      | x when (String.starts_with ~prefix:"3" x) -> SDLK_3
      | x when (String.starts_with ~prefix:"4" x) -> SDLK_4
      | x when (String.starts_with ~prefix:"5" x) -> SDLK_5
      | x when (String.starts_with ~prefix:"6" x) -> SDLK_6
      | x when (String.starts_with ~prefix:"7" x) -> SDLK_7
      | x when (String.starts_with ~prefix:"8" x) -> SDLK_8
      | x when (String.starts_with ~prefix:"9" x) -> SDLK_9
      | x when (String.starts_with ~prefix:"COLON" x) -> SDLK_COLON
      | x when (String.starts_with ~prefix:"SEMICOLON" x) -> SDLK_SEMICOLON
      | x when (String.starts_with ~prefix:"LESS" x) -> SDLK_LESS
      | x when (String.starts_with ~prefix:"EQUALS" x) -> SDLK_EQUALS
      | x when (String.starts_with ~prefix:"GREATER" x) -> SDLK_GREATER
      | x when (String.starts_with ~prefix:"QUESTION" x) -> SDLK_QUESTION
      | x when (String.starts_with ~prefix:"AT" x) -> SDLK_AT
      | x when (String.starts_with ~prefix:"LEFTBRACKET" x) -> SDLK_LEFTBRACKET
      | x when (String.starts_with ~prefix:"BACKSLASH" x) -> SDLK_BACKSLASH
      | x when (String.starts_with ~prefix:"RIGHTBRACKET" x) -> SDLK_RIGHTBRACKET
      | x when (String.starts_with ~prefix:"CARET" x) -> SDLK_CARET
      | x when (String.starts_with ~prefix:"UNDERSCORE" x) -> SDLK_UNDERSCORE
      | x when (String.starts_with ~prefix:"BACKQUOTE" x) -> SDLK_BACKQUOTE
      | x when (String.starts_with ~prefix:"a" x) -> SDLK_a
      | x when (String.starts_with ~prefix:"b" x) -> SDLK_b
      | x when (String.starts_with ~prefix:"c" x) -> SDLK_c
      | x when (String.starts_with ~prefix:"d" x) -> SDLK_d
      | x when (String.starts_with ~prefix:"e" x) -> SDLK_e
      | x when (String.starts_with ~prefix:"f" x) -> SDLK_f
      | x when (String.starts_with ~prefix:"g" x) -> SDLK_g
      | x when (String.starts_with ~prefix:"h" x) -> SDLK_h
      | x when (String.starts_with ~prefix:"i" x) -> SDLK_i
      | x when (String.starts_with ~prefix:"j" x) -> SDLK_j
      | x when (String.starts_with ~prefix:"k" x) -> SDLK_k
      | x when (String.starts_with ~prefix:"l" x) -> SDLK_l
      | x when (String.starts_with ~prefix:"m" x) -> SDLK_m
      | x when (String.starts_with ~prefix:"n" x) -> SDLK_n
      | x when (String.starts_with ~prefix:"o" x) -> SDLK_o
      | x when (String.starts_with ~prefix:"p" x) -> SDLK_p
      | x when (String.starts_with ~prefix:"q" x) -> SDLK_q
      | x when (String.starts_with ~prefix:"r" x) -> SDLK_r
      | x when (String.starts_with ~prefix:"s" x) -> SDLK_s
      | x when (String.starts_with ~prefix:"t" x) -> SDLK_t
      | x when (String.starts_with ~prefix:"u" x) -> SDLK_u
      | x when (String.starts_with ~prefix:"v" x) -> SDLK_v
      | x when (String.starts_with ~prefix:"w" x) -> SDLK_w
      | x when (String.starts_with ~prefix:"x" x) -> SDLK_x
      | x when (String.starts_with ~prefix:"y" x) -> SDLK_y
      | x when (String.starts_with ~prefix:"z" x) -> SDLK_z
      | x when (String.starts_with ~prefix:"DELETE" x) -> SDLK_DELETE
      | x when (String.starts_with ~prefix:"KP0" x) -> SDLK_KP0
      | x when (String.starts_with ~prefix:"KP1" x) -> SDLK_KP1
      | x when (String.starts_with ~prefix:"KP2" x) -> SDLK_KP2
      | x when (String.starts_with ~prefix:"KP3" x) -> SDLK_KP3
      | x when (String.starts_with ~prefix:"KP4" x) -> SDLK_KP4
      | x when (String.starts_with ~prefix:"KP5" x) -> SDLK_KP5
      | x when (String.starts_with ~prefix:"KP6" x) -> SDLK_KP6
      | x when (String.starts_with ~prefix:"KP7" x) -> SDLK_KP7
      | x when (String.starts_with ~prefix:"KP8" x) -> SDLK_KP8
      | x when (String.starts_with ~prefix:"KP9" x) -> SDLK_KP9
      | x when (String.starts_with ~prefix:"KP_PERIOD" x) -> SDLK_KP_PERIOD
      | x when (String.starts_with ~prefix:"KP_DIVIDE" x) -> SDLK_KP_DIVIDE
      | x when (String.starts_with ~prefix:"KP_MULTIPLY" x) -> SDLK_KP_MULTIPLY
      | x when (String.starts_with ~prefix:"KP_MINUS" x) -> SDLK_KP_MINUS
      | x when (String.starts_with ~prefix:"KP_PLUS" x) -> SDLK_KP_PLUS
      | x when (String.starts_with ~prefix:"KP_ENTER" x) -> SDLK_KP_ENTER
      | x when (String.starts_with ~prefix:"KP_EQUALS" x) -> SDLK_KP_EQUALS
      | x when (String.starts_with ~prefix:"UP" x) -> SDLK_UP
      | x when (String.starts_with ~prefix:"DOWN" x) -> SDLK_DOWN
      | x when (String.starts_with ~prefix:"RIGHT" x) -> SDLK_RIGHT
      | x when (String.starts_with ~prefix:"LEFT" x) -> SDLK_LEFT
      | x when (String.starts_with ~prefix:"INSERT" x) -> SDLK_INSERT
      | x when (String.starts_with ~prefix:"HOME" x) -> SDLK_HOME
      | x when (String.starts_with ~prefix:"END" x) -> SDLK_END
      | x when (String.starts_with ~prefix:"PAGEUP" x) -> SDLK_PAGEUP
      | x when (String.starts_with ~prefix:"PAGEDOWN" x) -> SDLK_PAGEDOWN
      | x when (String.starts_with ~prefix:"F1" x) -> SDLK_F1
      | x when (String.starts_with ~prefix:"F2" x) -> SDLK_F2
      | x when (String.starts_with ~prefix:"F3" x) -> SDLK_F3
      | x when (String.starts_with ~prefix:"F4" x) -> SDLK_F4
      | x when (String.starts_with ~prefix:"F5" x) -> SDLK_F5
      | x when (String.starts_with ~prefix:"F6" x) -> SDLK_F6
      | x when (String.starts_with ~prefix:"F7" x) -> SDLK_F7
      | x when (String.starts_with ~prefix:"F8" x) -> SDLK_F8
      | x when (String.starts_with ~prefix:"F9" x) -> SDLK_F9
      | x when (String.starts_with ~prefix:"F10" x) -> SDLK_F10
      | x when (String.starts_with ~prefix:"F11" x) -> SDLK_F11
      | x when (String.starts_with ~prefix:"F12" x) -> SDLK_F12
      | x when (String.starts_with ~prefix:"F13" x) -> SDLK_F13
      | x when (String.starts_with ~prefix:"F14" x) -> SDLK_F14
      | x when (String.starts_with ~prefix:"F15" x) -> SDLK_F15
      | x when (String.starts_with ~prefix:"NUMLOCK" x) -> SDLK_NUMLOCK
      | x when (String.starts_with ~prefix:"CAPSLOCK" x) -> SDLK_CAPSLOCK
      | x when (String.starts_with ~prefix:"SCROLLOCK" x) -> SDLK_SCROLLOCK
      | x when (String.starts_with ~prefix:"RSHIFT" x) -> SDLK_RSHIFT
      | x when (String.starts_with ~prefix:"LSHIFT" x) -> SDLK_LSHIFT
      | x when (String.starts_with ~prefix:"RCTRL" x) -> SDLK_RCTRL
      | x when (String.starts_with ~prefix:"LCTRL" x) -> SDLK_LCTRL
      | x when (String.starts_with ~prefix:"RALT" x) -> SDLK_RALT
      | x when (String.starts_with ~prefix:"LALT" x) -> SDLK_LALT
      | x when (String.starts_with ~prefix:"RMETA" x) -> SDLK_RMETA
      | x when (String.starts_with ~prefix:"LMETA" x) -> SDLK_LMETA
      | x when (String.starts_with ~prefix:"LSUPER" x) -> SDLK_LSUPER
      | x when (String.starts_with ~prefix:"RSUPER" x) -> SDLK_RSUPER
      | x when (String.starts_with ~prefix:"MODE" x) -> SDLK_MODE
      | x when (String.starts_with ~prefix:"HELP" x) -> SDLK_HELP
      | x when (String.starts_with ~prefix:"PRINT" x) -> SDLK_PRINT
      | x when (String.starts_with ~prefix:"SYSREQ" x) -> SDLK_SYSREQ
      | x when (String.starts_with ~prefix:"BREAK" x) -> SDLK_BREAK
      | x when (String.starts_with ~prefix:"MENU" x) -> SDLK_MENU
      | x when (String.starts_with ~prefix:"POWER" x) -> SDLK_POWER
      | x when (String.starts_with ~prefix:"EURO" x) -> SDLK_EURO
      | x -> failwith ("the key wasn't recognized : " ^ x)
    in
    match l with
    | h::t when String.ends_with ~suffix:"Block" h -> (Block, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Down" h -> (Down, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Flip Stance" h -> (FlipStance, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Left" h -> (Left, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Right" h -> (Right, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Tag" h -> (Tag, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Throw" h -> (Throw, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"Up" h -> (Up, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"[BK]" h -> (BK, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"[BP]" h -> (BP, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"[FK]" h -> (FK, recognize_key h)::(map_key t)
    | h::t when String.ends_with ~suffix:"[FP]" h -> (FP, recognize_key h)::(map_key t)
    | h::t when (String.trim h) = "combo definitions" -> []
    | h::t -> map_key t
    | [] -> []
  in
match l with 
| h::t when (String.trim h) = "key mapping" -> map_key t
| h::t -> find_key_mapping t
| [] -> print_endline "key mapping not found. Did you forget to start with \"key mapping\" keyword?"; []

let () =
  if (Array.length Sys.argv) <> 2 then
    print_endline ("Usage : " ^ Sys.executable_name ^ " grammar_file")
  else if (check_validity_of_file Sys.argv.(1)) then
    let l = get_file_content Sys.argv.(1) in
    if l <> [] then
      (* let parsed_data = parsing_list l in *)
      (* test_print_list l *)
      let data = find_key_mapping l in 
      print_pair data
      (* check if all moves are mapped and no key is used twice *)
    else ()
  else ()
