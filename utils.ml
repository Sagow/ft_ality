type key = BACKSPACE | None | TAB | RETURN | SPACE | EXCLAIM | QUOTEDBL | HASH | DOLLAR | PERCENT | AMPERSAND | QUOTE | LEFTPAREN | RIGHTPAREN | ASTERISK | PLUS | COMMA | MINUS | PERIOD | SLASH |  K0 |  K1 |  K2 |  K3 |  K4 |  K5 |  K6 |  K7 |  K8 |  K9 | COLON | SEMICOLON | LESS | EQUALS | GREATER | QUESTION | AT | LEFTBRACKET | BACKSLASH |  RIGHTBRACKET | CARET | UNDERSCORE | BACKQUOTE | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z |  F1 |  F2 |  F3 |  F4 |  F5 |  F6 |  F7 |  F8 |  F9 | F10 | F11 | F12 | INSERT | HOME | PAGEUP | DELETE | END | PAGEDOWN | RIGHT | LEFT | DOWN | UP | KP_DIVIDE | KP_MULTIPLY | KP_MINUS | KP_PLUS | KP_ENTER | KP_1 | KP_2 | KP_3 | KP_4 | KP_5 | KP_6 | KP_7 | KP_8 | KP_9 | KP_0
type move = Block | Down | FlipStance | Left | Right | Tag | Throw | Up | BK | BP | FK | FP | None
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
  | x when s = "BACKSPACE" -> BACKSPACE
  | x when s = "TAB" -> TAB
  | x when s = "CLEAR" -> CLEAR
  | x when s = "RETURN" -> RETURN
  | x when s = "PAUSE" -> PAUSE
  | x when s = "ESCAPE" -> ESCAPE
  | x when s = "SPACE" -> SPACE
  | x when s = "EXCLAIM" -> EXCLAIM
  | x when s = "QUOTEDBL" -> QUOTEDBL
  | x when s = "HASH" -> HASH
  | x when s = "DOLLAR" -> DOLLAR
  | x when s = "AMPERSAND" -> AMPERSAND
  | x when s = "QUOTE" -> QUOTE
  | x when s = "LEFTPAREN" -> LEFTPAREN
  | x when s = "RIGHTPAREN" -> RIGHTPAREN
  | x when s = "ASTERISK" -> ASTERISK
  | x when s = "PLUS" -> PLUS
  | x when s = "COMMA" -> COMMA
  | x when s = "MINUS" -> MINUS
  | x when s = "PERIOD" -> PERIOD
  | x when s = "SLASH" -> SLASH
  | x when s = "0" -> K0
  | x when s = "1" -> K1
  | x when s = "2" -> K2
  | x when s = "3" -> K3
  | x when s = "4" -> K4
  | x when s = "5" -> K5
  | x when s = "6" -> K6
  | x when s = "7" -> K7
  | x when s = "8" -> K8
  | x when s = "9" -> K9
  | x when s = "COLON" -> COLON
  | x when s = "SEMICOLON" -> SEMICOLON
  | x when s = "LESS" -> LESS
  | x when s = "EQUALS" -> EQUALS
  | x when s = "GREATER" -> GREATER
  | x when s = "QUESTION" -> QUESTION
  | x when s = "AT" -> AT
  | x when s = "LEFTBRACKET" -> LEFTBRACKET
  | x when s = "BACKSLASH" -> BACKSLASH
  | x when s = "RIGHTBRACKET" -> RIGHTBRACKET
  | x when s = "CARET" -> CARET
  | x when s = "UNDERSCORE" -> UNDERSCORE
  | x when s = "BACKQUOTE" -> BACKQUOTE
  | x when s = "a" -> A
  | x when s = "b" -> B
  | x when s = "c" -> C
  | x when s = "d" -> D
  | x when s = "e" -> E
  | x when s = "f" -> F
  | x when s = "g" -> G
  | x when s = "h" -> H
  | x when s = "i" -> I
  | x when s = "j" -> J
  | x when s = "k" -> K
  | x when s = "l" -> J
  | x when s = "m" -> M
  | x when s = "n" -> N
  | x when s = "o" -> O
  | x when s = "p" -> P
  | x when s = "q" -> Q
  | x when s = "r" -> R
  | x when s = "s" -> S
  | x when s = "t" -> T
  | x when s = "u" -> U
  | x when s = "v" -> V
  | x when s = "w" -> W
  | x when s = "x" -> X
  | x when s = "y" -> Y
  | x when s = "z" -> Z
  | x when s = "DELETE" -> DELETE
  | x when s = "KP0" -> KP0
  | x when s = "KP1" -> KP1
  | x when s = "KP2" -> KP2
  | x when s = "KP3" -> KP3
  | x when s = "KP4" -> KP4
  | x when s = "KP5" -> KP5
  | x when s = "KP6" -> KP6
  | x when s = "KP7" -> KP7
  | x when s = "KP8" -> KP8
  | x when s = "KP9" -> KP9
  | x when s = "KP_PERIOD" -> KP_PERIOD
  | x when s = "KP_DIVIDE" -> KP_DIVIDE
  | x when s = "KP_MULTIPLY" -> KP_MULTIPLY
  | x when s = "KP_MINUS" -> KP_MINUS
  | x when s = "KP_PLUS" -> KP_PLUS
  | x when s = "KP_ENTER" -> KP_ENTER
  | x when s = "KP_EQUALS" -> KP_EQUALS
  | x when s = "UP" -> UP
  | x when s = "DOWN" -> DOWN
  | x when s = "RIGHT" -> RIGHT
  | x when s = "LEFT" -> LEFT
  | x when s = "INSERT" -> INSERT
  | x when s = "HOME" -> HOME
  | x when s = "END" -> END
  | x when s = "PAGEUP" -> PAGEUP
  | x when s = "PAGEDOWN" -> PAGEDOWN
  | x when s = "F1" -> F1
  | x when s = "F2" -> F2
  | x when s = "F3" -> F3
  | x when s = "F4" -> F4
  | x when s = "F5" -> F5
  | x when s = "F6" -> F6
  | x when s = "F7" -> F7
  | x when s = "F8" -> F8
  | x when s = "F9" -> F9
  | x when s = "F10" -> F10
  | x when s = "F11" -> F11
  | x when s = "F12" -> F12
  | x when s = "F13" -> F13
  | x when s = "F14" -> F14
  | x when s = "F15" -> F15
  | x when s = "NUMLOCK" -> NUMLOCK
  | x when s = "CAPSLOCK" -> CAPSLOCK
  | x when s = "SCROLLOCK" -> SCROLLOCK
  | x when s = "RSHIFT" -> RSHIFT
  | x when s = "LSHIFT" -> LSHIFT
  | x when s = "RCTRL" -> RCTRL
  | x when s = "LCTRL" -> LCTRL
  | x when s = "RALT" -> RALT
  | x when s = "LALT" -> LALT
  | x when s = "RMETA" -> RMETA
  | x when s = "LMETA" -> LMETA
  | x when s = "LSUPER" -> LSUPER
  | x when s = "RSUPER" -> RSUPER
  | x when s = "MODE" -> MODE
  | x when s = "HELP" -> HELP
  | x when s = "PRINT" -> PRINT
  | x when s = "SYSREQ" -> SYSREQ
  | x when s = "BREAK" -> BREAK
  | x when s = "MENU" -> MENU
  | x when s = "POWER" -> POWER
  | x when s = "EURO" -> EURO
  | _ -> None

let key_to_string (k : key) : string =
  match k with
  | BACKSPACE -> "backspace"
  | TAB -> "tab"
  | CLEAR -> "clear"
  | RETURN -> "return"
  | PAUSE -> "pause"
  | ESCAPE -> "escape"
  | SPACE -> "space"
  | EXCLAIM -> "exclaim"
  | QUOTEDBL -> "quotedbl"
  | HASH -> "hash"
  | DOLLAR -> "dollar"
  | AMPERSAND -> "ampersand"
  | QUOTE -> "quote"
  | LEFTPAREN -> "leftparen"
  | RIGHTPAREN -> "rightparen"
  | ASTERISK -> "asterisk"
  | PLUS -> "plus"
  | COMMA -> "comma"
  | MINUS -> "minus"
  | PERIOD -> "period"
  | SLASH -> "slash"
  | K0 -> "0"
  | K1 -> "1"
  | K2 -> "2"
  | K3 -> "3"
  | K4 -> "4"
  | K5 -> "5"
  | K6 -> "6"
  | K7 -> "7"
  | K8 -> "8"
  | K9 -> "9"
  | COLON -> "colon"
  | SEMICOLON -> "semicolon"
  | LESS -> "less"
  | EQUALS -> "equals"
  | GREATER -> "greater"
  | QUESTION -> "question"
  | AT -> "at"
  | LEFTBRACKET -> "leftbracket"
  | BACKSLASH -> "backslash"
  | RIGHTBRACKET -> "rightbracket"
  | CARET -> "caret"
  | UNDERSCORE -> "underscore"
  | BACKQUOTE -> "backquote"
  | A -> "a"
  | B -> "b"
  | C -> "c"
  | D -> "d"
  | E -> "e"
  | F -> "f"
  | G -> "g"
  | H -> "h"
  | I -> "i"
  | J -> "j"
  | K -> "k"
  | L -> "l"
  | M -> "m"
  | N -> "n"
  | O -> "o"
  | P -> "p"
  | Q -> "q"
  | R -> "r"
  | S -> "s"
  | T -> "t"
  | U -> "u"
  | V -> "v"
  | W -> "w"
  | X -> "x"
  | Y -> "y"
  | Z -> "z"
  | DELETE -> "delete"
  | KP0 -> "kp0"
  | KP1 -> "kp1"
  | KP2 -> "kp2"
  | KP3 -> "kp3"
  | KP4 -> "kp4"
  | KP5 -> "kp5"
  | KP6 -> "kp6"
  | KP7 -> "kp7"
  | KP8 -> "kp8"
  | KP9 -> "kp9"
  | KP_PERIOD -> "kp_period"
  | KP_DIVIDE -> "kp_divide"
  | KP_MULTIPLY -> "kp_multiply"
  | KP_MINUS -> "kp_minus"
  | KP_PLUS -> "kp_plus"
  | KP_ENTER -> "kp_enter"
  | KP_EQUALS -> "kp_equals"
  | UP -> "up"
  | DOWN -> "down"
  | RIGHT -> "right"
  | LEFT -> "left"
  | INSERT -> "insert"
  | HOME -> "home"
  | END -> "end"
  | PAGEUP -> "pageup"
  | PAGEDOWN -> "pagedown"
  | F1 -> "f1"
  | F2 -> "f2"
  | F3 -> "f3"
  | F4 -> "f4"
  | F5 -> "f5"
  | F6 -> "f6"
  | F7 -> "f7"
  | F8 -> "f8"
  | F9 -> "f9"
  | F10 -> "f10"
  | F11 -> "f11"
  | F12 -> "f12"
  | F13 -> "f13"
  | F14 -> "f14"
  | F15 -> "f15"
  | NUMLOCK -> "numlock"
  | CAPSLOCK -> "capslock"
  | SCROLLOCK -> "scrollock"
  | RSHIFT -> "rshift"
  | LSHIFT -> "lshift"
  | RCTRL -> "rctrl"
  | LCTRL -> "lctrl"
  | RALT -> "ralt"
  | LALT -> "lalt"
  | RMETA -> "rmeta"
  | LMETA -> "lmeta"
  | LSUPER -> "lsuper"
  | RSUPER -> "rsuper"
  | MODE -> "mode"
  | HELP -> "help"
  | PRINT -> "print"
  | SYSREQ -> "sysreq"
  | BREAK -> "break"
  | MENU -> "menu"
  | POWER -> "power"
  | EURO -> "euro"
  | None -> "error"


let rec test_print_list (l : string list) =
  match l with
  | h::t -> print_endline h; test_print_list t
  | [] -> ()
