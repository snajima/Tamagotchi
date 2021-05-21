exception Gameover of bool

type element =
  | Fire
  | Water
  | Leaf
  | Nothing

type gamestate = {
  ours : element;
  opponent : element;
  wins : int;
  losses : int;
}

val check_nothing : gamestate -> int -> bool

val end_game : gamestate -> bool

val after_update : gamestate -> gamestate

val win_loss : gamestate -> gamestate

val init_game : unit -> gamestate

val get_ours : gamestate -> element

val get_opponent : gamestate -> element

val get_wins : gamestate -> int

val get_losses : gamestate -> int

val play_water : gamestate -> gamestate

val play_fire : gamestate -> gamestate

val play_leaf : gamestate -> gamestate
