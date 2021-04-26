
exception Gameover of int

type vs = Gui.viewstate

val dolphin_init : vs -> unit

val dolphin_exit : vs -> unit

val dolphin_except : vs -> exn -> unit

val dolphin_key : vs -> char -> unit

val dolphin_predraw : vs -> unit

val dolphin_step : vs -> unit
