type key = Tsdl.Sdl.keycode
type move = Block | Down | FlipStance | Left | Right | Tag | Throw | Up | BK | BP | FK | FP

let all_moves = [Block; Down; FlipStance; Left; Right; Tag; Throw; Up; BK; BP; FK; FP]

let string_to_move (s : string) : move =
  match s with 
  | x when x = "Block" -> Block
  | x when x = "Down" -> Down
  | x when x = "FlipStance" -> FlipStance
  | x when x = "Left" -> Left
  | x when x = "Right" -> Right
  | x when x = "Tag" -> Tag
  | x when x = "Throw" -> Throw
  | x when x = "Up" -> Up
  | x when x = "[BK]" -> BK
  | x when x = "[BP]" -> BP
  | x when x = "[FK]" -> FK
  | x when x = "[FP]" -> FP
  | x -> failwith (x ^ " is not a valid move !")

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

let key_to_string (k : key) : string =
  match k with
  | x when x = Tsdl.Sdl.K.backspace -> "Backspace"
  | x when x = Tsdl.Sdl.K.tab -> "Tab"
  | x when x = Tsdl.Sdl.K.return -> "Return"
  | x when x = Tsdl.Sdl.K.space -> "Space"
  | x when x = Tsdl.Sdl.K.exclaim -> "Exclaim"
  | x when x = Tsdl.Sdl.K.quotedbl -> "Quotedbl"
  | x when x = Tsdl.Sdl.K.hash -> "Hash"
  | x when x = Tsdl.Sdl.K.dollar -> "Dollar"
  | x when x = Tsdl.Sdl.K.percent -> "Percent"
  | x when x = Tsdl.Sdl.K.ampersand -> "Ampersand"
  | x when x = Tsdl.Sdl.K.quote -> "Quote"
  | x when x = Tsdl.Sdl.K.leftparen -> "Leftparen"
  | x when x = Tsdl.Sdl.K.rightparen -> "Rightparen"
  | x when x = Tsdl.Sdl.K.asterisk -> "Asterisk"
  | x when x = Tsdl.Sdl.K.plus -> "Plus"
  | x when x = Tsdl.Sdl.K.comma -> "Comma"
  | x when x = Tsdl.Sdl.K.minus -> "Minus"
  | x when x = Tsdl.Sdl.K.period -> "Period"
  | x when x = Tsdl.Sdl.K.slash -> "Slash"
  | x when x = Tsdl.Sdl.K.k0 -> "K0"
  | x when x = Tsdl.Sdl.K.k1 -> "K1"
  | x when x = Tsdl.Sdl.K.k2 -> "K2"
  | x when x = Tsdl.Sdl.K.k3 -> "K3"
  | x when x = Tsdl.Sdl.K.k4 -> "K4"
  | x when x = Tsdl.Sdl.K.k5 -> "K5"
  | x when x = Tsdl.Sdl.K.k6 -> "K6"
  | x when x = Tsdl.Sdl.K.k7 -> "K7"
  | x when x = Tsdl.Sdl.K.k8 -> "K8"
  | x when x = Tsdl.Sdl.K.k9 -> "K9"
  | x when x = Tsdl.Sdl.K.colon -> "Colon"
  | x when x = Tsdl.Sdl.K.semicolon -> "Semicolon"
  | x when x = Tsdl.Sdl.K.less -> "Less"
  | x when x = Tsdl.Sdl.K.equals -> "Equals"
  | x when x = Tsdl.Sdl.K.greater -> "Greater"
  | x when x = Tsdl.Sdl.K.question -> "Question"
  | x when x = Tsdl.Sdl.K.at -> "At"
  | x when x = Tsdl.Sdl.K.leftbracket -> "Leftbracket"
  | x when x = Tsdl.Sdl.K.backslash -> "Backslash"
  | x when x = Tsdl.Sdl.K.rightbracket -> "Rightbracket"
  | x when x = Tsdl.Sdl.K.caret -> "Caret"
  | x when x = Tsdl.Sdl.K.underscore -> "Underscore"
  | x when x = Tsdl.Sdl.K.backquote -> "Backquote"
  | x when x = Tsdl.Sdl.K.a -> "A"
  | x when x = Tsdl.Sdl.K.b -> "B"
  | x when x = Tsdl.Sdl.K.c -> "C"
  | x when x = Tsdl.Sdl.K.d -> "D"
  | x when x = Tsdl.Sdl.K.e -> "E"
  | x when x = Tsdl.Sdl.K.f -> "F"
  | x when x = Tsdl.Sdl.K.g -> "G"
  | x when x = Tsdl.Sdl.K.h -> "H"
  | x when x = Tsdl.Sdl.K.i -> "I"
  | x when x = Tsdl.Sdl.K.j -> "J"
  | x when x = Tsdl.Sdl.K.k -> "K"
  | x when x = Tsdl.Sdl.K.l -> "L"
  | x when x = Tsdl.Sdl.K.m -> "M"
  | x when x = Tsdl.Sdl.K.n -> "N"
  | x when x = Tsdl.Sdl.K.o -> "O"
  | x when x = Tsdl.Sdl.K.p -> "P"
  | x when x = Tsdl.Sdl.K.q -> "Q"
  | x when x = Tsdl.Sdl.K.r -> "R"
  | x when x = Tsdl.Sdl.K.s -> "S"
  | x when x = Tsdl.Sdl.K.t -> "T"
  | x when x = Tsdl.Sdl.K.u -> "U"
  | x when x = Tsdl.Sdl.K.v -> "V"
  | x when x = Tsdl.Sdl.K.w -> "W"
  | x when x = Tsdl.Sdl.K.x -> "X"
  | x when x = Tsdl.Sdl.K.y -> "Y"
  | x when x = Tsdl.Sdl.K.z -> "Z"
  | x when x = Tsdl.Sdl.K.f1 -> "F1"
  | x when x = Tsdl.Sdl.K.f2 -> "F2"
  | x when x = Tsdl.Sdl.K.f3 -> "F3"
  | x when x = Tsdl.Sdl.K.f4 -> "F4"
  | x when x = Tsdl.Sdl.K.f5 -> "F5"
  | x when x = Tsdl.Sdl.K.f6 -> "F6"
  | x when x = Tsdl.Sdl.K.f7 -> "F7"
  | x when x = Tsdl.Sdl.K.f8 -> "F8"
  | x when x = Tsdl.Sdl.K.f9 -> "F9"
  | x when x = Tsdl.Sdl.K.f10 -> "F10"
  | x when x = Tsdl.Sdl.K.f11 -> "F11"
  | x when x = Tsdl.Sdl.K.f12 -> "F12"
  | x when x = Tsdl.Sdl.K.insert -> "Insert"
  | x when x = Tsdl.Sdl.K.home -> "Home"
  | x when x = Tsdl.Sdl.K.pageup -> "Pageup"
  | x when x = Tsdl.Sdl.K.delete -> "Delete"
  | x when x = Tsdl.Sdl.K.kend -> "End"
  | x when x = Tsdl.Sdl.K.pagedown -> "Pagedown"
  | x when x = Tsdl.Sdl.K.right -> "Right"
  | x when x = Tsdl.Sdl.K.left -> "Left"
  | x when x = Tsdl.Sdl.K.down -> "Down"
  | x when x = Tsdl.Sdl.K.up -> "Up"
  | x when x = Tsdl.Sdl.K.kp_divide -> "Kp_divide"
  | x when x = Tsdl.Sdl.K.kp_multiply -> "Kp_multiply"
  | x when x = Tsdl.Sdl.K.kp_minus -> "Kp_minus"
  | x when x = Tsdl.Sdl.K.kp_plus -> "Kp_plus"
  | x when x = Tsdl.Sdl.K.kp_enter -> "Kp_enter"
  | x when x = Tsdl.Sdl.K.kp_1 -> "Kp_1"
  | x when x = Tsdl.Sdl.K.kp_2 -> "Kp_2"
  | x when x = Tsdl.Sdl.K.kp_3 -> "Kp_3"
  | x when x = Tsdl.Sdl.K.kp_4 -> "Kp_4"
  | x when x = Tsdl.Sdl.K.kp_5 -> "Kp_5"
  | x when x = Tsdl.Sdl.K.kp_6 -> "Kp_6"
  | x when x = Tsdl.Sdl.K.kp_7 -> "Kp_7"
  | x when x = Tsdl.Sdl.K.kp_8 -> "Kp_8"
  | x when x = Tsdl.Sdl.K.kp_9 -> "Kp_9"
  | x when x = Tsdl.Sdl.K.kp_0 -> "Kp_0"
  | _ -> "Unknown"

let string_to_key (str : string) : key =
  match str with
  | "Backspace" -> Tsdl.Sdl.K.backspace
  | "Tab" -> Tsdl.Sdl.K.tab
  | "Return" -> Tsdl.Sdl.K.return
  | "Space" -> Tsdl.Sdl.K.space
  | "Exclaim" -> Tsdl.Sdl.K.exclaim
  | "Quotedbl" -> Tsdl.Sdl.K.quotedbl
  | "Hash" -> Tsdl.Sdl.K.hash
  | "Dollar" -> Tsdl.Sdl.K.dollar
  | "Percent" -> Tsdl.Sdl.K.percent
  | "Ampersand" -> Tsdl.Sdl.K.ampersand
  | "Quote" -> Tsdl.Sdl.K.quote
  | "Leftparen" -> Tsdl.Sdl.K.leftparen
  | "Rightparen" -> Tsdl.Sdl.K.rightparen
  | "Asterisk" -> Tsdl.Sdl.K.asterisk
  | "Plus" -> Tsdl.Sdl.K.plus
  | "Comma" -> Tsdl.Sdl.K.comma
  | "Minus" -> Tsdl.Sdl.K.minus
  | "Period" -> Tsdl.Sdl.K.period
  | "Slash" -> Tsdl.Sdl.K.slash
  | "K0" -> Tsdl.Sdl.K.k0
  | "K1" -> Tsdl.Sdl.K.k1
  | "K2" -> Tsdl.Sdl.K.k2
  | "K3" -> Tsdl.Sdl.K.k3
  | "K4" -> Tsdl.Sdl.K.k4
  | "K5" -> Tsdl.Sdl.K.k5
  | "K6" -> Tsdl.Sdl.K.k6
  | "K7" -> Tsdl.Sdl.K.k7
  | "K8" -> Tsdl.Sdl.K.k8
  | "K9" -> Tsdl.Sdl.K.k9
  | "Colon" -> Tsdl.Sdl.K.colon
  | "Semicolon" -> Tsdl.Sdl.K.semicolon
  | "Less" -> Tsdl.Sdl.K.less
  | "Equals" -> Tsdl.Sdl.K.equals
  | "Greater" -> Tsdl.Sdl.K.greater
  | "Question" -> Tsdl.Sdl.K.question
  | "At" -> Tsdl.Sdl.K.at
  | "Leftbracket" -> Tsdl.Sdl.K.leftbracket
  | "Backslash" -> Tsdl.Sdl.K.backslash
  | "Rightbracket" -> Tsdl.Sdl.K.rightbracket
  | "Caret" -> Tsdl.Sdl.K.caret
  | "Underscore" -> Tsdl.Sdl.K.underscore
  | "Backquote" -> Tsdl.Sdl.K.backquote
  | "A" -> Tsdl.Sdl.K.a
  | "B" -> Tsdl.Sdl.K.b
  | "C" -> Tsdl.Sdl.K.c
  | "D" -> Tsdl.Sdl.K.d
  | "E" -> Tsdl.Sdl.K.e
  | "F" -> Tsdl.Sdl.K.f
  | "G" -> Tsdl.Sdl.K.g
  | "H" -> Tsdl.Sdl.K.h
  | "I" -> Tsdl.Sdl.K.i
  | "J" -> Tsdl.Sdl.K.j
  | "K" -> Tsdl.Sdl.K.k
  | "L" -> Tsdl.Sdl.K.l
  | "M" -> Tsdl.Sdl.K.m
  | "N" -> Tsdl.Sdl.K.n
  | "O" -> Tsdl.Sdl.K.o
  | "P" -> Tsdl.Sdl.K.p
  | "Q" -> Tsdl.Sdl.K.q
  | "R" -> Tsdl.Sdl.K.r
  | "S" -> Tsdl.Sdl.K.s
  | "T" -> Tsdl.Sdl.K.t
  | "U" -> Tsdl.Sdl.K.u
  | "V" -> Tsdl.Sdl.K.v
  | "W" -> Tsdl.Sdl.K.w
  | "X" -> Tsdl.Sdl.K.x
  | "Y" -> Tsdl.Sdl.K.y
  | "Z" -> Tsdl.Sdl.K.z
  | "F1" -> Tsdl.Sdl.K.f1
  | "F2" -> Tsdl.Sdl.K.f2
  | "F3" -> Tsdl.Sdl.K.f3
  | "F4" -> Tsdl.Sdl.K.f4
  | "F5" -> Tsdl.Sdl.K.f5
  | "F6" -> Tsdl.Sdl.K.f6
  | "F7" -> Tsdl.Sdl.K.f7
  | "F8" -> Tsdl.Sdl.K.f8
  | "F9" -> Tsdl.Sdl.K.f9
  | "F10" -> Tsdl.Sdl.K.f10
  | "F11" -> Tsdl.Sdl.K.f11
  | "F12" -> Tsdl.Sdl.K.f12
  | "Insert" -> Tsdl.Sdl.K.insert
  | "Home" -> Tsdl.Sdl.K.home
  | "Pageup" -> Tsdl.Sdl.K.pageup
  | "Delete" -> Tsdl.Sdl.K.delete
  | "End" -> Tsdl.Sdl.K.kend
  | "Pagedown" -> Tsdl.Sdl.K.pagedown
  | "Right" -> Tsdl.Sdl.K.right
  | "Left" -> Tsdl.Sdl.K.left
  | "Down" -> Tsdl.Sdl.K.down
  | "Up" -> Tsdl.Sdl.K.up
  | "Kp_divide" -> Tsdl.Sdl.K.kp_divide
  | "Kp_multiply" -> Tsdl.Sdl.K.kp_multiply
  | "Kp_minus" -> Tsdl.Sdl.K.kp_minus
  | "Kp_plus" -> Tsdl.Sdl.K.kp_plus
  | "Kp_enter" -> Tsdl.Sdl.K.kp_enter
  | "Kp_1" -> Tsdl.Sdl.K.kp_1
  | "Kp_2" -> Tsdl.Sdl.K.kp_2
  | "Kp_3" -> Tsdl.Sdl.K.kp_3
  | "Kp_4" -> Tsdl.Sdl.K.kp_4
  | "Kp_5" -> Tsdl.Sdl.K.kp_5
  | "Kp_6" -> Tsdl.Sdl.K.kp_6
  | "Kp_7" -> Tsdl.Sdl.K.kp_7
  | "Kp_8" -> Tsdl.Sdl.K.kp_8
  | "Kp_9" -> Tsdl.Sdl.K.kp_9
  | "Kp_0" -> Tsdl.Sdl.K.kp_0
  | _ -> failwith (str ^ " is not a valid key input !")

let rec test_print_list (l : string list) =
  match l with
  | h::t -> print_endline h; test_print_list t
  | [] -> ()
