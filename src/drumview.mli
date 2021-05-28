(** Renders a visual representation of the [Drum] game and provides
    handler functions for the execution of the GUI draw loop *)

(** Type [game_vars] represents the variables that the game requires to run:
    - [game] is the [gamestate] of [Drum]
    - [speed] represents how fast the beats move (lower the faster)
    - [beat_speed] represents how spaced aparts the beats are
    - [row_scale] is used for animating (higher the smoother)
    - [lifestage] is a mutable field for storing the lifestage from Homeview,
    used solely for animating *)

type game_vars = {
    mutable game : Drum.gamestate;
    mutable speed : int;
    mutable beat_speed : int;
    row_scale : int;
    mutable lifestage : string;
}

(** [g] is of type game_vars and stores all global game variables *)
val g : game_vars

(** [drum_init] initializes the GUI in the GUI draw loop *)
val drum_init : Gui.viewstate -> unit

(** [drum_exit] cleans up the [Gui.viewstate] after the GUI draw loop
    concludes *)
val drum_exit : Gui.viewstate -> unit

(** [drum_except] handles all possible exceptions that may be raised
    during the GUI draw loop *)
val drum_except : Gui.viewstate -> exn -> unit

(** [drum_key] handles user input during the execution of the GUI draw
    loop *)
val drum_key : Gui.viewstate -> char -> unit

(** [drum_step] handles any necessary changes to the [Gui.viewstate]
    each frame during execution of the main GUI draw loop *)
val drum_step : Gui.viewstate -> unit

(** [drum_predraw] handles any rendering that must precede the rendering
    of animations from the main GUI draw loop *)
val drum_predraw : Gui.viewstate -> unit

(** [draw] launches the custom GUI draw loop with the specific handlers
    for the Drum game *)
val draw : unit -> unit
