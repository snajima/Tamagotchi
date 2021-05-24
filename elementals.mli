(** Functional game engine that simulates the execution of the
    elementals game *)

(** Type [element] represents the following four possible elementents
    that the player or machine can play:

    - Fire
    - Water
    - Leaf
    - Nothing : this is only used for the player, and used as a
      placeholder for when they haven't played yet*)
type element =
  | Fire
  | Water
  | Leaf
  | Nothing

(** Type [gamestate] represents state of the Elementals game simulation
    which stores:

    - ours : a tuple where the first element is the [element] that the
      player is currently playing and the second element is an integer
      represnting the position of element for animating
    - opponent : a tuple where the first element is the [element] that
      the opponent (machine) is currently playing and the second element
      is an integer represnting the position of element for animating
    - wins : an integer representing the number of wins the player
      currently has
    - losses : an integer representing the number of losses the player
      currently has
    - currently_animated : a boolean representing whether or not the
      gamestate is currently being animated or not
    - start_anim : a boolean representing when the animation got started
    - end_anim : a boolean representing whether the explosion is done being
    animated *)
type gamestate = {
  ours : element * int;
  opponent : element * int;
  wins : int;
  losses : int;
  currently_animated : bool;
  start_anim : bool;
  mutable end_anim : bool;
}

(** Raised when either the player losses or wins 2 games*)
exception Gameover of bool

(** Raised when a winner is determined for one game*)
exception WinnerDetermined

(** [init_game ()] is the initial state of the game when playing the
    Elementals game. In this state the player is currently playing
    nothing and the machine is playing a random element. *)
val init_game : unit -> gamestate

(** [win_loss gs] takes in the current game state, checks if the game
    has ended or not and if not updates the machine's element for the
    next game, and updates the win and loss fields of [gs]. *)
val win_loss : gamestate -> gamestate

(** [get_ours gs] returns a tuple where the first element is the
    [element] that the player is currently playing and the second
    element is an integer represnting the position of element for
    animating. *)
val get_ours : gamestate -> element * int

(** [get_opponent gs] returns a tuple where the first element is the
    [element] that the opponent (machine) is currently playing and the
    second element is an integer represnting the position of element for
    animating*)
val get_opponent : gamestate -> element * int

(** [get_wins gs] returns an integer representing the number of wins the
    player currently has*)
val get_wins : gamestate -> int

(** [get_losses gs] returns an integer representing the number of losses
    the player currently has*)
val get_losses : gamestate -> int

(** [get_currently_animated gs] returns a bool representing if the animation
    is currently happening *)
val get_currently_animated : gamestate -> bool 

(** [play_water gs] checks if the game is being animated or not. If it
    isn't then it updates the player's element [gs.ours] to playing
    water.*)
val play_water : gamestate -> gamestate

(** [play_fire gs] checks if the game is being animated or not. If it
    isn't then it updates the player's element [gs.ours] to playing
    fire.*)
val play_fire : gamestate -> gamestate

(** [play_leaf gs] checks if the game is being animated or not. If it
    isn't then it updates the player's element [gs.ours] to playing
    leaf.*)
val play_leaf : gamestate -> gamestate

(** [next gs] checks if the game is being animated or not. If the game
    is being updated, it increments the animation until it raises
    [WinnerDetermined]. Once [WinnerDetermined] is raised, [win_loss gs]
    is called to start the next game. *)
val next : gamestate -> gamestate
