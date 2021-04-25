type game = Dolphin.gamestate

type vs = Gui.viewstate

val dolphin_init : vs -> unit

val dolphin_exit : vs -> unit

val dolphin_except : vs -> unit

val dolphin_key : vs -> unit

val dolphin_step : vs -> unit
