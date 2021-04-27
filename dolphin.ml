open Random

type lane =
  | Left
  | Middle
  | Right

(* Stores score *)
exception Gameover of int

exception Offscreen

(* --------------------- Data Vars ----------------------- *)
let max_height = 60

type gamestate = {
  (* --------------------- Time ------------------------ *)
  (* Steps since the game started *)
  step : int;
  (* --------------------- Data ------------------------ *)
  (* Coordinates of the rocks following (lane * height)  *)
  (* Left lane is 0, Middle 1, left 2, Max height is 120 *)
  rocks : (int * int) list;
  current_lane : lane;
}

(* ----------------- Internal functions ------------------- *)

(** Reduce the height of a single rock *)
let fall_rock (level, height) : int * int =
  if height <= 1 then raise Offscreen else (level, height - 1)

(** Reduce the height of a list of rock, removing rocks that fall
    offscreen *)
let rec fall_rocks (rock_lst : (int * int) list) : (int * int) list =
  match rock_lst with
  | [] -> []
  | h :: t -> (
      (* If rocks fall offscreen, exclude from list of rocks *)
      try fall_rock h :: fall_rocks t with Offscreen -> fall_rocks t)

(** Checks if the player is in contact with any of the rocks*)
let game_over (gs : gamestate) : bool =
  let lane_num =
    match gs.current_lane with Left -> 0 | Middle -> 1 | Right -> 2
  in
  let lose_condition (rock_lane, rock_height) =
    rock_height < 5 && rock_height > 2 && lane_num == rock_lane
  in
  List.exists lose_condition gs.rocks

(* ----------------- External functions ------------------- *)

let init_game () : gamestate =
  { step = 0; rocks = []; current_lane = Middle }

let get_rocks (gs : gamestate) = gs.rocks

let get_dolphin_lane (gs : gamestate) : lane = gs.current_lane

let num_rocks (gs : gamestate) : int = List.length (get_rocks gs)

let process_left (gs : gamestate) : gamestate =
  match gs.current_lane with
  | Left -> gs
  | Middle -> { gs with current_lane = Left }
  | Right -> { gs with current_lane = Middle }

let process_middle (gs : gamestate) : gamestate = gs

let process_right (gs : gamestate) : gamestate =
  match gs.current_lane with
  | Left -> { gs with current_lane = Middle }
  | Middle -> { gs with current_lane = Right }
  | Right -> gs

let next (gs : gamestate) : gamestate =
  if game_over gs then raise (Gameover gs.step)
  else { gs with step = gs.step + 1; rocks = fall_rocks gs.rocks }

let add_rock (gs : gamestate) : gamestate =
  let lane = int 3 in
  (* Rocks only enter the frame in the next step *)
  { gs with rocks = (lane, max_height + 1) :: gs.rocks }
