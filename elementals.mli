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
      the opponent is currently playing and the second element is an
      integer represnting the position of element for animating
    - wins : an integer representing the number of wins the player
      currently has
    - losses : an integer representing the number of losses the player
      currently has
    - currently_animated : a boolean representing whether or not the
      gamestate is currently being animated or not*)
type gamestate = {
  ours : element * int;
  opponent : element * int;
  wins : int;
  losses : int;
  currently_animated : bool;
}

(** Raised when either the player losses or wins 2 games*)
exception Gameover of bool

(** Raised when a winner is determined for one game*)
exception WinnerDetermined

val init_game : unit -> gamestate

val win_loss : gamestate -> gamestate

val get_ours : gamestate -> element * int

val get_opponent : gamestate -> element * int

val get_wins : gamestate -> int

val get_losses : gamestate -> int

val play_water : gamestate -> gamestate

val play_fire : gamestate -> gamestate

val play_leaf : gamestate -> gamestate

val next : gamestate -> gamestate
