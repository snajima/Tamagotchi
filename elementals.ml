open Random

type element =
  | Fire
  | Water
  | Leaf
  | Nothing

(* Stores score *)
exception Gameover of bool

exception Offscreen

(* --------------------- Data Vars ----------------------- *)
let max_height = 60

type gamestate = {
  (* --------------------- Time ------------------------ *)
  (* Steps since the game started *)
  (* --------------------- Data ------------------------ *)
  ours : element;
  opponent : element;
  wins : int;
  losses : int;
}

(* ----------------- Internal functions ------------------- *)

(**who is 1 if it is checking ours else it is 0*)
let check_nothing (gs : gamestate) (who : int) : bool =
  if who = 0 then match gs.opponent with Nothing -> true | _ -> false
  else match gs.ours with Nothing -> true | _ -> false

let end_game (gs : gamestate) : bool =
  if gs.wins = 2 || gs.losses = 2 then true else false

let after_update (gs : gamestate) : gamestate =
  if end_game gs then raise (Gameover (gs.wins = 2))
  else
    match Random.int 3 with
    | 0 -> { gs with opponent = Fire; ours = Nothing }
    | 1 -> { gs with opponent = Water; ours = Nothing }
    | 2 -> { gs with opponent = Leaf; ours = Nothing }
    | _ -> failwith "impossible"

let win_loss (gs : gamestate) : gamestate =
  if
    (gs.ours = Water && gs.opponent = Leaf)
    || (gs.ours = Fire && gs.opponent = Water)
    || (gs.ours = Leaf && gs.opponent = Fire)
  then after_update { gs with losses = gs.losses + 1 }
  else if gs.ours = gs.opponent then after_update gs
  else after_update { gs with wins = gs.wins + 1 }

(* ----------------- External functions ------------------- *)

let init_game () : gamestate =
  { ours = Nothing; opponent = Nothing; wins = 0; losses = 0 }

let get_ours (gs : gamestate) : element = gs.ours

let get_opponent (gs : gamestate) : element = gs.opponent

let get_wins (gs : gamestate) : int = gs.wins

let get_losses (gs : gamestate) : int = gs.losses

let play_water (gs : gamestate) : gamestate =
  win_loss { gs with ours = Water }

let play_fire (gs : gamestate) : gamestate =
  win_loss { gs with ours = Fire }

let play_leaf (gs : gamestate) : gamestate =
  win_loss { gs with ours = Leaf }
