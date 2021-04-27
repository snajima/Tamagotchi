open Random

type color =
  | Don
  | Ka

(* Stores score *)
exception Gameover of int

exception Miss

(* --------------------- Data Vars ----------------------- *)
let max_height = 60

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
  [( max_height + 1, Don )];
  [( max_height + 1, Ka )];
  [( max_height + 1, Don ); ( max_height + 6, Don ); ( max_height + 11, Don )];
  [( max_height + 1, Ka ); ( max_height + 6, Ka ); ( max_height + 11, Ka )];
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
  gs.score > 10000

(* ----------------- External functions ------------------- *)

let init_game () : gamestate =
  { combo = 0; score = 0; beats = []; good_multiplier = 1.6; combo_multiplier = 10; player_x = 10; good_range = 5; ok_range = 8; bad_range = 10 }

let get_beats (gs : gamestate) = gs.beats

let get_score (gs : gamestate) = gs.score

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

let calc_beats (hit_type : hit) (gs : gamestate) : (int * color) list =
  match hit_type with
  | OutOfRange -> gs.beats
  | _ -> List.tl gs.beats

(** Returns beats list with the beat closest to player as the list head *)
(* let rec closest_to_player (min : int) (beats : (int * color) list) (so_far : (int * color) list) (gs : gamestate) : ((int * color) list * (int * color)) = 
  match beats with 
  | h :: t -> (
    let diff = Int.abs ((fst h) - gs.player_x) in
    if min > diff then closest_to_player diff t (so_far @ [h]) gs else (so_far @ (List.tl beats), h)
  )
  | _ -> ([], (0, Ka)) (** TODO: What would be base case? *)

let calc_beats (hit_type : hit) (otherwise : (int * color) list) (gs : gamestate) : (int * color) list =
  match hit_type with
  | OutOfRange -> gs.beats
  | _ -> otherwise
  
let process_left (gs : gamestate) : gamestate =
  let closest = closest_to_player (Int.abs (fst (List.hd gs.beats) - gs.player_x)) (gs.beats) [] gs in
  match (snd closest) with
  | (ht, Don) -> (
    let hit_type = range ht gs false in
    { 
      gs with 
      score = calc_score hit_type gs;
      combo = calc_combo hit_type gs;
      beats = calc_beats hit_type (fst closest) gs
    }
  )
  | (ht, Ka) -> (
    let hit_type = range ht gs true in
    { 
        gs with 
        score = calc_score hit_type gs;
        combo = calc_combo hit_type gs;
        beats = calc_beats hit_type (fst closest) gs
      }
  )

let process_right (gs : gamestate) : gamestate =
  let closest = closest_to_player (Int.abs (fst (List.hd gs.beats) - gs.player_x)) (gs.beats) [] gs in
  match (snd closest) with
  | (ht, Don) -> (
    let hit_type = range ht gs true in
    { 
      gs with 
      score = calc_score hit_type gs;
      combo = calc_combo hit_type gs;
      beats = calc_beats hit_type (fst closest) gs
    }
  )
  | (ht, Ka) -> (
    let hit_type = range ht gs false in
    { 
      gs with 
      score = calc_score hit_type gs;
      combo = calc_combo hit_type gs;
      beats = calc_beats hit_type (fst closest) gs
    }
  ) *)

let process_left (gs : gamestate) : gamestate =
  match gs.beats with
  | (ht, Don) :: t -> (
    let hit_type = range ht gs false in
    { 
      gs with 
      score = calc_score hit_type gs;
      combo = calc_combo hit_type gs;
      beats = calc_beats hit_type gs
    }
  )
  | (ht, Ka) :: t -> (
    let hit_type = range ht gs true in
    { 
        gs with 
        score = calc_score hit_type gs;
        combo = calc_combo hit_type gs;
        beats = calc_beats hit_type gs
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
      beats = calc_beats hit_type gs
    }
  )
  | (ht, Ka) :: t -> (
    let hit_type = range ht gs false in
    { 
      gs with 
      score = calc_score hit_type gs;
      combo = calc_combo hit_type gs;
      beats = calc_beats hit_type gs
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
