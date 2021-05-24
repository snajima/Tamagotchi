(** Renders a visual representation of the [Elementals] game and
    provides handler functions for the execution of the GUI draw loop *)

(* Stores score *)
exception Gameover of int

(** [elementals_init] initializes the GUI in the GUI draw loop *)
val elementals_init : Gui.viewstate -> unit

(** [elementals_exit] cleans up the [Gui.viewstate] after the GUI draw
    loop concludes *)
val elementals_exit : Gui.viewstate -> unit

(** [elementals_except] handles all possible exceptions that may be
    raised during the GUI draw loop *)
val elementals_except : Gui.viewstate -> exn -> unit

(** [elementals_key] handles user input during the execution of the GUI
    draw loop *)
val elementals_key : Gui.viewstate -> char -> unit

(** [elementals_step] handles any necessary changes to the
    [Gui.viewstate] each frame during execution of the main GUI draw
    loop *)
val elementals_step : Gui.viewstate -> unit

(** [elementals_predraw] handles any rendering that must precede the
    rendering of animations from the main GUI draw loop *)
val elementals_predraw : Gui.viewstate -> unit

(** [draw] launches the custom GUI draw loop with the specific handlers
    for the Drum game *)
val draw : unit -> unit
