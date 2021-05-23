open Random

type element =
  | Fire
  | Water
  | Leaf
  | Nothing

exception Gameover of bool

exception WinnerDetermined

(* --------------------- Data Vars ----------------------- *)
let max_height = 60

type gamestate = {
  (* --------------------- Data ------------------------ *)
  ours : element * int;
  (* snd of tuple is position of element for animating *)
  opponent : element * int;
  wins : int;
  losses : int;
  currently_animated : bool;
}

(* ----------------- Internal functions ------------------- *)

(**who is 1 if it is checking ours else it is 0*)
let check_nothing (gs : gamestate) (who : int) : bool =
  if who = 0 then
    match fst gs.opponent with Nothing -> true | _ -> false
  else match fst gs.ours with Nothing -> true | _ -> false

let end_game (gs : gamestate) : bool = gs.wins = 2 || gs.losses = 2

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

let rec next_helper (gs : gamestate) : gamestate =
  if snd gs.ours > 50 then raise WinnerDetermined
  else
    {
      gs with
      ours = (fst gs.ours, snd gs.ours + 1);
      opponent = (fst gs.opponent, snd gs.opponent - 1);
    }

(* ----------------- External functions ------------------- *)

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