type move = Block | Down | FlipStance | Left | Right | Tag | Throw | Up | BK | BP | FK | FP | None
type key = SDLK_BACKSPACE | SDLK_TAB | SDLK_CLEAR | SDLK_RETURN | SDLK_PAUSE | SDLK_ESCAPE | SDLK_SPACE | SDLK_EXCLAIM | SDLK_QUOTEDBL | SDLK_HASH | SDLK_DOLLAR | SDLK_AMPERSAND | SDLK_QUOTE | SDLK_LEFTPAREN | SDLK_RIGHTPAREN | SDLK_ASTERISK | SDLK_PLUS | SDLK_COMMA | SDLK_MINUS | SDLK_PERIOD | SDLK_SLASH | SDLK_0 | SDLK_1 | SDLK_2 | SDLK_3 | SDLK_4 | SDLK_5 | SDLK_6 | SDLK_7 | SDLK_8 | SDLK_9 | SDLK_COLON | SDLK_SEMICOLON | SDLK_LESS | SDLK_EQUALS | SDLK_GREATER | SDLK_QUESTION | SDLK_AT | SDLK_LEFTBRACKET | SDLK_BACKSLASH | SDLK_RIGHTBRACKET | SDLK_CARET | SDLK_UNDERSCORE | SDLK_BACKQUOTE | SDLK_a | SDLK_b | SDLK_c | SDLK_d | SDLK_e | SDLK_f | SDLK_g | SDLK_h | SDLK_i | SDLK_j | SDLK_k | SDLK_l | SDLK_m | SDLK_n | SDLK_o | SDLK_p | SDLK_q | SDLK_r | SDLK_s | SDLK_t | SDLK_u | SDLK_v | SDLK_w | SDLK_x | SDLK_y | SDLK_z | SDLK_DELETE | SDLK_KP0 | SDLK_KP1 | SDLK_KP2 | SDLK_KP3 | SDLK_KP4 | SDLK_KP5 | SDLK_KP6 | SDLK_KP7 | SDLK_KP8 | SDLK_KP9 | SDLK_KP_PERIOD | SDLK_KP_DIVIDE | SDLK_KP_MULTIPLY | SDLK_KP_MINUS | SDLK_KP_PLUS | SDLK_KP_ENTER | SDLK_KP_EQUALS | SDLK_UP | SDLK_DOWN | SDLK_RIGHT | SDLK_LEFT | SDLK_INSERT | SDLK_HOME | SDLK_END | SDLK_PAGEUP | SDLK_PAGEDOWN | SDLK_F1 | SDLK_F2 | SDLK_F3 | SDLK_F4 | SDLK_F5 | SDLK_F6 | SDLK_F7 | SDLK_F8 | SDLK_F9 | SDLK_F10 | SDLK_F11 | SDLK_F12 | SDLK_F13 | SDLK_F14 | SDLK_F15 | SDLK_NUMLOCK | SDLK_CAPSLOCK | SDLK_SCROLLOCK | SDLK_RSHIFT | SDLK_LSHIFT | SDLK_RCTRL | SDLK_LCTRL | SDLK_RALT | SDLK_LALT | SDLK_RMETA | SDLK_LMETA | SDLK_LSUPER | SDLK_RSUPER | SDLK_MODE | SDLK_HELP | SDLK_PRINT | SDLK_SYSREQ | SDLK_BREAK | SDLK_MENU | SDLK_POWER | SDLK_EURO | None

let string_to_move (s : string) : move =
  match s with 
  | x when s = "Block" -> Block
  | x when s = "Down" -> Down
  | x when s = "FlipStance" -> FlipStance
  | x when s = "Left" -> Left
  | x when s = "Right" -> Right
  | x when s = "Tag" -> Tag
  | x when s = "Throw" -> Throw
  | x when s = "Up" -> Up
  | x when s = "[BK]" -> BK
  | x when s = "[BP]" -> BP
  | x when s = "[FK]" -> FK
  | x when s = "[FP]" -> FP
  | _ -> None

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

let string_to_key(s : string) : key =
  match s with 
  | x when s = "BACKSPACE" -> SDLK_BACKSPACE
  | x when s = "TAB" -> SDLK_TAB
  | x when s = "CLEAR" -> SDLK_CLEAR
  | x when s = "RETURN" -> SDLK_RETURN
  | x when s = "PAUSE" -> SDLK_PAUSE
  | x when s = "ESCAPE" -> SDLK_ESCAPE
  | x when s = "SPACE" -> SDLK_SPACE
  | x when s = "EXCLAIM" -> SDLK_EXCLAIM
  | x when s = "QUOTEDBL" -> SDLK_QUOTEDBL
  | x when s = "HASH" -> SDLK_HASH
  | x when s = "DOLLAR" -> SDLK_DOLLAR
  | x when s = "AMPERSAND" -> SDLK_AMPERSAND
  | x when s = "QUOTE" -> SDLK_QUOTE
  | x when s = "LEFTPAREN" -> SDLK_LEFTPAREN
  | x when s = "RIGHTPAREN" -> SDLK_RIGHTPAREN
  | x when s = "ASTERISK" -> SDLK_ASTERISK
  | x when s = "PLUS" -> SDLK_PLUS
  | x when s = "COMMA" -> SDLK_COMMA
  | x when s = "MINUS" -> SDLK_MINUS
  | x when s = "PERIOD" -> SDLK_PERIOD
  | x when s = "SLASH" -> SDLK_SLASH
  | x when s = "0" -> SDLK_0
  | x when s = "1" -> SDLK_1
  | x when s = "2" -> SDLK_2
  | x when s = "3" -> SDLK_3
  | x when s = "4" -> SDLK_4
  | x when s = "5" -> SDLK_5
  | x when s = "6" -> SDLK_6
  | x when s = "7" -> SDLK_7
  | x when s = "8" -> SDLK_8
  | x when s = "9" -> SDLK_9
  | x when s = "COLON" -> SDLK_COLON
  | x when s = "SEMICOLON" -> SDLK_SEMICOLON
  | x when s = "LESS" -> SDLK_LESS
  | x when s = "EQUALS" -> SDLK_EQUALS
  | x when s = "GREATER" -> SDLK_GREATER
  | x when s = "QUESTION" -> SDLK_QUESTION
  | x when s = "AT" -> SDLK_AT
  | x when s = "LEFTBRACKET" -> SDLK_LEFTBRACKET
  | x when s = "BACKSLASH" -> SDLK_BACKSLASH
  | x when s = "RIGHTBRACKET" -> SDLK_RIGHTBRACKET
  | x when s = "CARET" -> SDLK_CARET
  | x when s = "UNDERSCORE" -> SDLK_UNDERSCORE
  | x when s = "BACKQUOTE" -> SDLK_BACKQUOTE
  | x when s = "a" -> SDLK_a
  | x when s = "b" -> SDLK_b
  | x when s = "c" -> SDLK_c
  | x when s = "d" -> SDLK_d
  | x when s = "e" -> SDLK_e
  | x when s = "f" -> SDLK_f
  | x when s = "g" -> SDLK_g
  | x when s = "h" -> SDLK_h
  | x when s = "i" -> SDLK_i
  | x when s = "j" -> SDLK_j
  | x when s = "k" -> SDLK_k
  | x when s = "l" -> SDLK_l
  | x when s = "m" -> SDLK_m
  | x when s = "n" -> SDLK_n
  | x when s = "o" -> SDLK_o
  | x when s = "p" -> SDLK_p
  | x when s = "q" -> SDLK_q
  | x when s = "r" -> SDLK_r
  | x when s = "s" -> SDLK_s
  | x when s = "t" -> SDLK_t
  | x when s = "u" -> SDLK_u
  | x when s = "v" -> SDLK_v
  | x when s = "w" -> SDLK_w
  | x when s = "x" -> SDLK_x
  | x when s = "y" -> SDLK_y
  | x when s = "z" -> SDLK_z
  | x when s = "DELETE" -> SDLK_DELETE
  | x when s = "KP0" -> SDLK_KP0
  | x when s = "KP1" -> SDLK_KP1
  | x when s = "KP2" -> SDLK_KP2
  | x when s = "KP3" -> SDLK_KP3
  | x when s = "KP4" -> SDLK_KP4
  | x when s = "KP5" -> SDLK_KP5
  | x when s = "KP6" -> SDLK_KP6
  | x when s = "KP7" -> SDLK_KP7
  | x when s = "KP8" -> SDLK_KP8
  | x when s = "KP9" -> SDLK_KP9
  | x when s = "KP_PERIOD" -> SDLK_KP_PERIOD
  | x when s = "KP_DIVIDE" -> SDLK_KP_DIVIDE
  | x when s = "KP_MULTIPLY" -> SDLK_KP_MULTIPLY
  | x when s = "KP_MINUS" -> SDLK_KP_MINUS
  | x when s = "KP_PLUS" -> SDLK_KP_PLUS
  | x when s = "KP_ENTER" -> SDLK_KP_ENTER
  | x when s = "KP_EQUALS" -> SDLK_KP_EQUALS
  | x when s = "UP" -> SDLK_UP
  | x when s = "DOWN" -> SDLK_DOWN
  | x when s = "RIGHT" -> SDLK_RIGHT
  | x when s = "LEFT" -> SDLK_LEFT
  | x when s = "INSERT" -> SDLK_INSERT
  | x when s = "HOME" -> SDLK_HOME
  | x when s = "END" -> SDLK_END
  | x when s = "PAGEUP" -> SDLK_PAGEUP
  | x when s = "PAGEDOWN" -> SDLK_PAGEDOWN
  | x when s = "F1" -> SDLK_F1
  | x when s = "F2" -> SDLK_F2
  | x when s = "F3" -> SDLK_F3
  | x when s = "F4" -> SDLK_F4
  | x when s = "F5" -> SDLK_F5
  | x when s = "F6" -> SDLK_F6
  | x when s = "F7" -> SDLK_F7
  | x when s = "F8" -> SDLK_F8
  | x when s = "F9" -> SDLK_F9
  | x when s = "F10" -> SDLK_F10
  | x when s = "F11" -> SDLK_F11
  | x when s = "F12" -> SDLK_F12
  | x when s = "F13" -> SDLK_F13
  | x when s = "F14" -> SDLK_F14
  | x when s = "F15" -> SDLK_F15
  | x when s = "NUMLOCK" -> SDLK_NUMLOCK
  | x when s = "CAPSLOCK" -> SDLK_CAPSLOCK
  | x when s = "SCROLLOCK" -> SDLK_SCROLLOCK
  | x when s = "RSHIFT" -> SDLK_RSHIFT
  | x when s = "LSHIFT" -> SDLK_LSHIFT
  | x when s = "RCTRL" -> SDLK_RCTRL
  | x when s = "LCTRL" -> SDLK_LCTRL
  | x when s = "RALT" -> SDLK_RALT
  | x when s = "LALT" -> SDLK_LALT
  | x when s = "RMETA" -> SDLK_RMETA
  | x when s = "LMETA" -> SDLK_LMETA
  | x when s = "LSUPER" -> SDLK_LSUPER
  | x when s = "RSUPER" -> SDLK_RSUPER
  | x when s = "MODE" -> SDLK_MODE
  | x when s = "HELP" -> SDLK_HELP
  | x when s = "PRINT" -> SDLK_PRINT
  | x when s = "SYSREQ" -> SDLK_SYSREQ
  | x when s = "BREAK" -> SDLK_BREAK
  | x when s = "MENU" -> SDLK_MENU
  | x when s = "POWER" -> SDLK_POWER
  | x when s = "EURO" -> SDLK_EURO
  | _ -> None

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
  | None -> "error"


let rec test_print_list (l : string list) =
  match l with
  | h::t -> print_endline h; test_print_list t
  | [] -> ()
