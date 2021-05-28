(** Functional game engine that simulates the execution of the dolphin
    game *)

(** The type [lane] represents the three possible positions for the
    Dolphin *)
type lane =
  | Left
  | Middle
  | Right

(** The type [gamestate] represents state of the Dolphin game simulation
    which stores:

    - The lane the player is currently in
    - The number of steps that have occured in the game simulation
    - The positions of all the rocks in the current game simulation

    Note that since adding rocks involves randomness, calling [next] on
    two identical gamestates may not result in the same result *)
type gamestate

(** Raised when a player and rock collision is encountered *)
exception Gameover of int

(** [max_height] is an integer indicating the maximum height of a rock
    in the game. It takes [max_height] number of calls to the [next]
    function in order for a rock to fall from the top to the bottom and
    then subsequently be removed *)
val max_height : int

(** [get_rocks gs] returns a list of coordinates that indicate the
    location of the rocks in [gs] following the format (lane, height).
    Lane is an integer in 1-3 inclusive. 1 is Left, 2 is Middle and 3 is
    Right. Height is a integer between 1 - [max_height] *)
val get_rocks : gamestate -> (int * int) list

(** [get_dolphin_lane gs] returns the [Lane] that the player is
    currently on in the gamestate [gs]*)
val get_dolphin_lane : gamestate -> lane

(** [num_rocks gs] returns the number of rocks in the gamestate [gs]*)
val num_rocks : gamestate -> int

(** [init_game ()] is the initial state of the game when playing the
    Dolphin game. In this state the player (Dolphin) is currently
    located in the middle lane, and there are no rocks. *)
val init_game : unit -> gamestate

(** [process_left gs] returns a new gamestate where the player's lane is
    shifted to the left, if there is one. If the player is currently on
    the left lane in [gs] then [process_left gs] returns the same
    gamestate. *)
val process_left : gamestate -> gamestate

(** [process_middle gs] returns [gs] *)
val process_middle : gamestate -> gamestate

(** [process_right gs] returns a new gamestate where the player's lane
    is shifted to the right, if there is one. If the player is currently
    on the right lane in [gs] then [process_right gs] returns the same
    gamestate. *)
val process_right : gamestate -> gamestate

(** [next gs] returns a new gamestate where the height of all the rocks
    in [gs] is decremented by one.

    Raises: [Gameover] if the player comes in contact (overlapping lane
    and height) with any rock. *)
val next : gamestate -> gamestate

(** [add_rock gs] randomly selects a lane (left, middle, right) and adds
    a rock to the selected lane with [max_height + 1]. Note that the
    rock only enters the game simulation when next is called to have
    consistent behaviour -- all rocks fall for the same number of turns *)
val add_rock : gamestate -> gamestate
