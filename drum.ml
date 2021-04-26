open Random

type color =
  | Don
  | Ka

(* Stores score *)
exception Gameover of int

exception Miss

type gamestate = {
  (* -------------------- Scoring ----------------------- *)
  (* Scoring system *)
  score : int;
  combo : int;
  (* --------------------- Data ------------------------ *)
  (* Coordinates of the beats following (height * color)  *)
  (* Max height is 50 *)
  beats : (int * color) list;
  good_multiplier : float;
  combo_multiplier: int;
  player_x : int;
  good_range : int;
  ok_range : int;
  bad_range : int;
}

type hit = Good | Ok | Bad | OutOfRange

let beats = [
  [( 51, Don )];
  [( 51, Ka )];
  [( 51, Don ); ( 56, Don ); ( 61, Don )];
  [( 51, Ka ); ( 56, Ka ); ( 61, Ka )];
]

(* ----------------- Internal functions ------------------- *)

(** Reduce the height of a single beat *)
let fall_beat (height, c) : int * color =
  if height <= 1 then raise Miss else (height - 1, c)

(** Reduce the height of a list of beats, removing beats that fall
    offscreen *)
let rec fall_beats (beat_lst : (int * color) list) : (int * color) list =
  match beat_lst with
  | [] -> []
  | h :: t -> (
      (* If beats fall offscreen, exclude from list of beats *)
      try fall_beat h :: fall_beats t with Miss -> fall_beats t )

(** Checks if the player is in contact with any of the rocks*)
let game_over (gs : gamestate) : bool =
  gs.score > 100

(* ----------------- External functions ------------------- *)

let init_game () : gamestate =
  { combo = 0; score = 0; beats = []; good_multiplier = 1.6; combo_multiplier = 10; player_x = 10; good_range = 2; ok_range = 5; bad_range = 10 }

let get_beats (gs : gamestate) = gs.beats

(** Returns the type of hit based on range between beat and player x *)
let range (beat_x : int) (gs : gamestate) (right_beat : bool) : hit =
  let diff = Int.abs (beat_x - gs.player_x) in 
  if right_beat then (
    if diff < gs.good_range then Good
    else if diff < gs.ok_range then Ok
    else if diff < gs.bad_range then Bad
    else OutOfRange
  ) else (
    if diff < gs.bad_range then Bad
    else OutOfRange
  )

(** Calculates the score based on hit type *)
let calc_score (hit_type : hit) (gs : gamestate) : int =
  match hit_type with
  | Good -> gs.score + 500 + Float.to_int(gs.good_multiplier *. Int.to_float(gs.combo / gs.combo_multiplier)) * 200
  | Ok -> gs.score + 300 + (gs.combo / gs.combo_multiplier) * 100
  | _ -> gs.score

(** Calculates the combo based on hit type *)
let calc_combo (hit_type : hit) (gs : gamestate) : int =
  match hit_type with
  | Good -> gs.combo + 1
  | Ok -> gs.combo + 1
  | Bad -> 0
  | OutOfRange -> gs.combo

let process_left (gs : gamestate) : gamestate =
  match gs.beats with
  | (ht, Don) :: t -> (
    let hit_type = range ht gs false in
    { 
      gs with 
      score = calc_score hit_type gs;
      combo = calc_combo hit_type gs;
      beats = t
    }
  )
  | (ht, Ka) :: t -> (
    let hit_type = range ht gs true in
    { 
      gs with 
      score = calc_score hit_type gs;
      combo = calc_combo hit_type gs;
      beats = t
    }
  )
  | [] -> gs

let process_middle (gs : gamestate) : gamestate = gs

let process_right (gs : gamestate) : gamestate =
  match gs.beats with
  | (ht, Don) :: t -> (
    let hit_type = range ht gs true in
    { 
      gs with 
      score = calc_score hit_type gs;
      combo = calc_combo hit_type gs;
      beats = t
    }
  )
  | (ht, Ka) :: t -> (
    let hit_type = range ht gs false in
    { 
      gs with 
      score = calc_score hit_type gs;
      combo = calc_combo hit_type gs;
      beats = t
    }
  )
  | [] -> gs

let next (gs : gamestate) : gamestate =
  if game_over gs then raise (Gameover gs.score)
  else { gs with beats = fall_beats gs.beats }

let add_beat (gs : gamestate) : gamestate =
  let beat_set = List.nth beats (Random.int (List.length beats)) in
  (* Beats only enter the frame in the next step *)
  { gs with beats = gs.beats @ beat_set }
