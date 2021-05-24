open Random

type element =
  | Fire
  | Water
  | Leaf
  | Nothing

exception Gameover of bool

exception WinnerDetermined

type gamestate = {
  ours : element * int;
  opponent : element * int;
  wins : int;
  losses : int;
  currently_animated : bool;
}

(* ----------------------- Internal functions ------------------------- *)
(*[end_game gs] returns a boolean that is true if the elementals game is
  ended. This happens is the player has incurred 2 wins or losses.*)
let end_game (gs : gamestate) : bool = gs.wins = 2 || gs.losses = 2

(*[after_update gs] checks if the elementals game has ended. If not, the
  opponent's current element [gs.opponent] gets updated to a random
  element and the player's current element [gs.ours] gets reset to
  nothing.*)
let after_update (gs : gamestate) : gamestate =
  if end_game gs then raise (Gameover (gs.wins = 2))
  else Random.self_init ();
  match Random.int 3 with
  | 0 ->
      {
        gs with
        opponent = (Fire, 100);
        ours = (Nothing, 0);
        currently_animated = false;
      }
  | 1 ->
      {
        gs with
        opponent = (Water, 100);
        ours = (Nothing, 0);
        currently_animated = false;
      }
  | 2 ->
      {
        gs with
        opponent = (Leaf, 100);
        ours = (Nothing, 0);
        currently_animated = false;
      }
  | _ -> failwith "impossible"

(*[next_helper gs] increments the second value of the [gs.our] and
  [gs.opponent] integers which denotes the location of each animation
  until [WinnerDetermined] is raised.*)
let rec next_helper (gs : gamestate) : gamestate =
  if snd gs.ours > 50 then raise WinnerDetermined
  else
    {
      gs with
      ours = (fst gs.ours, snd gs.ours + 1);
      opponent = (fst gs.opponent, snd gs.opponent - 1);
    }

(* ------------------------ External functions ------------------------- *)

let init_game () : gamestate =
  Random.self_init ();
  match Random.int 3 with
  | 0 ->
      {
        ours = (Nothing, 0);
        opponent = (Fire, 100);
        wins = 0;
        losses = 0;
        currently_animated = false;
      }
  | 1 ->
      {
        ours = (Nothing, 0);
        opponent = (Water, 100);
        wins = 0;
        losses = 0;
        currently_animated = false;
      }
  | 2 ->
      {
        ours = (Nothing, 0);
        opponent = (Leaf, 100);
        wins = 0;
        losses = 0;
        currently_animated = false;
      }
  | _ -> failwith "impossible"

let win_loss (gs : gamestate) : gamestate =
  if
    (fst gs.ours = Water && fst gs.opponent = Leaf)
    || (fst gs.ours = Fire && fst gs.opponent = Water)
    || (fst gs.ours = Leaf && fst gs.opponent = Fire)
  then (
    print_endline "You lost";
    after_update { gs with losses = gs.losses + 1 } )
  else if fst gs.ours = fst gs.opponent then (
    print_endline "You drew";
    after_update gs )
  else (
    print_endline "You won";
    after_update { gs with wins = gs.wins + 1 } )

let get_ours (gs : gamestate) : element * int = gs.ours

let get_opponent (gs : gamestate) : element * int = gs.opponent

let get_wins (gs : gamestate) : int = gs.wins

let get_losses (gs : gamestate) : int = gs.losses

let get_currently_animated (gs : gamestate) : bool =
  gs.currently_animated

let play_water (gs : gamestate) : gamestate =
  if gs.currently_animated then gs
  else { gs with ours = (Water, 20); currently_animated = true }

let play_fire (gs : gamestate) : gamestate =
  if gs.currently_animated then gs
  else { gs with ours = (Fire, 20); currently_animated = true }

let play_leaf (gs : gamestate) : gamestate =
  if gs.currently_animated then gs
  else { gs with ours = (Leaf, 20); currently_animated = true }

let next (gs : gamestate) : gamestate =
  if gs.currently_animated then
    try next_helper gs with WinnerDetermined -> win_loss gs
  else gs