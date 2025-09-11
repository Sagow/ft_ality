type move
type key = Tsdl.Sdl.keycode

val all_moves : move list

val move_to_string : move -> string
val string_to_move : string -> move
val key_to_string : key -> string
val string_to_key : string -> key
val test_print_list : string list -> unit

