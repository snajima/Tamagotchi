(** Renders a visual representation of the [Drum] game and provides
    handler functions for the execution of the GUI draw loop *)

(** [drum_init] initializes the GUI in the GUI draw loop *)
val drum_init : Gui.viewstate -> unit

(** [drum_exit] cleans up the [Gui.viewstate] after the GUI draw loop
    concludes *)
val drum_exit : Gui.viewstate -> unit

(** [drum_except] handles all possible exceptions that may be raised
    during the GUI draw loop *)
val drum_except : Gui.viewstate -> exn -> unit

(** [drum_key] handles user input during the execution of the GUI
    draw loop *)
val drum_key : Gui.viewstate -> char -> unit

(** [drum_step] handles any necessary changes to the
    [Gui.viewstate] each frame during execution of the main GUI draw
    loop *)
val drum_step : Gui.viewstate -> unit

(** [drum_predraw] handles any rendering that must precede the
    rendering of animations from the main GUI draw loop *)
val drum_predraw : Gui.viewstate -> unit

(** [draw] launches the custom GUI draw loop with the specific handlers
    for the Drum game *)
val draw : unit -> unit