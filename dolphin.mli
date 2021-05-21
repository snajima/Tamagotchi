type lane =
  | Left
  | Middle
  | Right

type gamestate

exception Gameover of int

val max_height : int

val get_rocks : gamestate -> (int * int) list 

val get_dolphin_lane : gamestate -> lane 

val num_rocks : gamestate -> int

(** [init_game ()] is the initial state of the game when playing
    the Dolphin game. In this state the player (Dolphin) is currently located 
    in the middle lane, and there are no rocks. *)
val init_game : unit -> gamestate

(** [process_left gs] returns a new gamestate where the player's lane is 
    shifted to the left, if there is one. If the player is currently on the 
    left lane in [gs] then [process_left gs] returns the same gamestate. *)
val process_left: gamestate -> gamestate

(** [process_middle gs] returns [gs] *)
val process_middle : gamestate -> gamestate

(** [process_right gs] returns a new gamestate where the player's lane is 
    shifted to the right, if there is one. If the player is currently on the 
    right lane in [gs] then [process_right gs] returns the same gamestate. *)
val process_right: gamestate -> gamestate 

(** [next gs] returns a new gamestate where the height of all the rocks in [gs]
    is decremented by one. 
    
    Raises: [Gameover] if the player comes in contact (overlapping lane and 
    height) with any rock. *)
val next : gamestate -> gamestate 

(** [add_rock gs] randomly selects a lane (left, middle, right) and adds a rock 
    to the selected lane with [max_height + 1]. Note that the rock only enters 
    the game simulation when next is called to have consistent behaviour -- all
    rocks fall for the same number of turns  *)
val add_rock: gamestate -> gamestate