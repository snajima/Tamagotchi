exception Gameover of bool

type element =
  | Fire
  | Water
  | Leaf
  | Nothing

type gamestate = {
  ours : (element * int);
  opponent : (element * int);
  wins : int;
  losses : int;
  currently_animated : bool;
}

val check_nothing : gamestate -> int -> bool

val end_game : gamestate -> bool

val after_update : gamestate -> gamestate

val win_loss : gamestate -> gamestate

val init_game : unit -> gamestate

val get_ours : gamestate -> (element * int)

val get_opponent : gamestate -> (element * int)

val get_wins : gamestate -> int

val get_losses : gamestate -> int

val play_water : gamestate -> gamestate

val play_fire : gamestate -> gamestate

val play_leaf : gamestate -> gamestate

val next : gamestate -> gamestate
