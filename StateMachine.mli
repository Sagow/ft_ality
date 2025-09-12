type character_name = string
type combo_name = string
type combo_data = character_name * combo_name

module TransitionEntryKV : (FtMap.KEYVALUE with type key_t = Utils.move and type value_t = (combo_data list * int))

module TransitionMap : (FtMap.MAP with type key_t := TransitionEntryKV.key_t and type value_t := TransitionEntryKV.value_t)

val build : Combo.Combo.t list -> TransitionMap.t list

val perform_transition : TransitionMap.t list -> int -> Utils.move -> combo_data list * int

val print_combo_list : combo_data list -> unit
