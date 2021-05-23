(** Renders a visual representation of the [Dolphin] game and provides
    handler functions for the execution of the GUI draw loop *)

(** [dolphin_init] initializes the GUI in the GUI draw loop *)
val dolphin_init : Gui.viewstate -> unit

(** [dolphin_exit] cleans up the [Gui.viewstate] after the GUI draw loop
    concludes *)
val dolphin_exit : Gui.viewstate -> unit

(** [dolphin_except] handles all possible exceptions that may be raised
    during the GUI draw loop *)
val dolphin_except : Gui.viewstate -> exn -> unit

(** [dolphin_key] handles user input during the execution of the GUI
    draw loop *)
val dolphin_key : Gui.viewstate -> char -> unit

(** [dolphin_predraw] handles any rendering that must precede the
    rendering of animations from the main GUI draw loop *)
val dolphin_predraw : Gui.viewstate -> unit

(** [dolphin_step] handles any necessary changes to the [Gui.viewstate]
    each frame during execution of the main GUI draw loop *)
val dolphin_step : Gui.viewstate -> unit

(** [draw] launches the custom GUI draw loop with the specific handlers
    for the Dolphin game *)
val draw : unit -> unit
