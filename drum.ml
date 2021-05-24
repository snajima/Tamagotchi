open Random
open Gui

type color =
  | Don
  | Ka

type beat =
  | Right of int
  | Idle
  | Left of int

exception Gameover of int

(* ------------------------------------------------------------- *)
(* ------------------------ Data Vars -------------------------- *)
(* ------------------------------------------------------------- *)
let max_height = 120

type gamestate = {
  (* -------------------- Scoring ----------------------- *)
  (* Scoring system *)
  score : int;
  combo : int;
  (* --------------------- Data ------------------------ *)
  (* Coordinates of the beats following (height * color)  *)
  (* Max height is 120 *)
  beats : (int * color) list;
  beat_type : beat;
  (* Multiplier for getting a [Good] rated input *)
  good_multiplier : float;
  (* Multiplier for getting a higher combo *)
  combo_multiplier : int;
  (* Player position *)
  player_x : int;
  (* Ranges for hit detection *)
  good_range : int;
  ok_range : int;
  bad_range : int;
  (* Number of beats left in game *)
  num_beats : int;
}

type hit =
  | Good
  | Ok
  | Bad
  | OutOfRange

let beats =
  [
    [ (max_height + 1, Don) ]; [ (max_height + 1, Ka) ];
    [
      (max_height + 1, Don); (max_height + 16, Don);
      (max_height + 31, Don);
    ];
    [
      (max_height + 1, Ka); (max_height + 16, Ka); (max_height + 31, Ka);
    ];
    [
      (max_height + 1, Don); (max_height + 16, Ka);
      (max_height + 31, Don);
    ];
    [
      (max_height + 1, Ka); (max_height + 16, Don); (max_height + 31, Ka);
    ]; [ (max_height + 1, Don); (max_height + 21, Don) ];
    [ (max_height + 1, Ka); (max_height + 21, Ka) ];
  ]

(* ------------------------------------------------------------- *)
(* -------------------- Internal functions --------------------- *)
(* ------------------------------------------------------------- *)

(** Raised when a player misses a beat to reset combo counter. Note that
    this exception is only raised by internal functions, it is not
    included in the out-facing mli *)
exception Miss

(** [fall_beat] reduces the height of a single beat. It is a helper
    function that is to be used as a subprocess of [fall_beats]

    Raises: [Miss] if the height of a beat is less than or equal to 0. *)
let fall_beat (height, c) : int * color =
  if height <= 0 then raise Miss else (height - 1, c)

(** [fall_beats] reduces the height of a list of beats, removing beats
    that fall offscreen, and returns the resulting list of rocks *)
let rec fall_beats (gs : gamestate) : gamestate =
  match gs.beats with
  | [] -> gs
  | h :: t -> (
      (* If beats fall offscreen, exclude from list of beats *)
      try
        {
          gs with
          beats =
            fall_beat h :: (fall_beats { gs with beats = t }).beats;
        }
      with Miss ->
        { (fall_beats { gs with beats = t }) with combo = 0 })

(** [game_over] returns a boolean indicating if the number of beats left
    is 0 *)
let game_over (gs : gamestate) : bool = gs.num_beats <= 0

(** [range_helper] draws the message based on if the hit type was
    [Good], [Ok], or [Bad] *)
let range_helper (message : string) : unit =
  draw_pixels 10 90 25 10 Graphics.white;
  draw_message 50
    ((default_vs.maxy * default_vs.scale / 2) + 120)
    35 Graphics.black message

(** [range] returns the type of hit based on distance between a beat and
    the player's x position. If the distance is less than [good_range],
    the hit type returned is [Good], same logic with [Ok], and [Bad].
    Otherwise, returns a [OutOfRange] to not punish players for hitting
    a button when no beats are nearby, intentionally or not *)
let range (beat_x : int) (gs : gamestate) (right_beat : bool) : hit =
  let diff = Int.abs (beat_x - gs.player_x) in
  if right_beat then
    if diff < gs.good_range then (
      range_helper "Good!";
      Good)
    else if diff < gs.ok_range then (
      range_helper "Ok";
      Ok)
    else if diff < gs.bad_range then (
      range_helper "Bad";
      Bad)
    else OutOfRange
  else if diff < gs.bad_range then (
    range_helper "Bad";
    Bad)
  else OutOfRange

(** Calculates the combo based on hit type

    If hit type is [Good] or [Ok], increase score by a certain amount If
    hit type is [Bad] or [OutOfRange], don't increase score *)
let calc_score (hit_type : hit) (gs : gamestate) : int =
  match hit_type with
  | Good ->
      gs.score + 500
      + Float.to_int
          (gs.good_multiplier
          *. Int.to_float (gs.combo / gs.combo_multiplier))
        * 200
  | Ok -> gs.score + 300 + (gs.combo / gs.combo_multiplier * 100)
  | _ -> gs.score

(** Calculates the combo based on hit type

    If hit type is [Good] or [Ok], continue combo (i.e. return combo +
    1) If hit type is [Bad] reset combo (i.e. return 0) If hit type is
    [OutOfRange], don't punish players and return itself (i.e. return
    combo) *)
let calc_combo (hit_type : hit) (gs : gamestate) : int =
  match hit_type with
  | Good -> gs.combo + 1
  | Ok -> gs.combo + 1
  | Bad -> 0
  | OutOfRange -> gs.combo

(** [closest_to_player] returns a list of beats and a beat in the tuple
    format (beats list, beat) with fst of tuple containing every beat
    but the one closest to the drum and snd of tuple being the element
    that got removed *)
let rec closest_to_player
    (min : int)
    (beats : (int * color) list)
    (so_far : (int * color) list)
    (gs : gamestate) : (int * color) list * (int * color) =
  match beats with
  | h :: t ->
      let diff = Int.abs (fst h - gs.player_x) in
      if min > diff then closest_to_player diff t (so_far @ [ h ]) gs
      else (so_far @ List.tl beats, h)
  | _ -> ([], (0, Ka))

(** [calc_beats] either returns the beats already in the game (if hit
    type is [OutOfRange]), or returns the beats in the parameter
    [otherwise]. This function is used to return the correct list of
    beats depending on if there is a beat within range when a player
    inputs a button *)
let calc_beats
    (hit_type : hit)
    (otherwise : (int * color) list)
    (gs : gamestate) : (int * color) list =
  match hit_type with OutOfRange -> gs.beats | _ -> otherwise

let process_helper
    (bt_type : beat)
    (hit_type : hit)
    (gs : gamestate)
    (bts : (int * color) list) : gamestate =
  {
    gs with
    beat_type = bt_type;
    score = calc_score hit_type gs;
    combo = calc_combo hit_type gs;
    beats = calc_beats hit_type bts gs;
  }

(* ------------------------------------------------------------- *)
(* -------------------- External functions --------------------- *)
(* ------------------------------------------------------------- *)

let init_game () : gamestate =
  {
    combo = 0;
    score = 0;
    beats = [];
    beat_type = Idle;
    good_multiplier = 1.6;
    combo_multiplier = 10;
    player_x = 10;
    good_range = 2;
    ok_range = 4;
    bad_range = 5;
    num_beats = 11;
  }

let get_beats (gs : gamestate) : (int * color) list = gs.beats

let get_num_beats (gs : gamestate) : int = gs.num_beats

let get_combo (gs : gamestate) : int = gs.combo

let get_score (gs : gamestate) : int = gs.score

let get_beat_type (gs : gamestate) : beat = gs.beat_type

let process_left (gs : gamestate) : gamestate =
  if List.length gs.beats = 0 then { gs with beat_type = Left 50 }
  else
    let closest =
      closest_to_player
        (Int.abs (fst (List.hd gs.beats) - gs.player_x))
        gs.beats [] gs
    in
    match snd closest with
    | ht, Don ->
        let hit_type = range ht gs false in
        process_helper (Left 50) hit_type gs (fst closest)
    | ht, Ka ->
        let hit_type = range ht gs true in
        process_helper (Left 50) hit_type gs (fst closest)

let process_middle (gs : gamestate) : gamestate = gs

let process_right (gs : gamestate) : gamestate =
  if List.length gs.beats = 0 then { gs with beat_type = Right 50 }
  else
    let closest =
      closest_to_player
        (Int.abs (fst (List.hd gs.beats) - gs.player_x))
        gs.beats [] gs
    in
    match snd closest with
    | ht, Don ->
        let hit_type = range ht gs true in
        process_helper (Right 50) hit_type gs (fst closest)
    | ht, Ka ->
        let hit_type = range ht gs false in
        process_helper (Right 50) hit_type gs (fst closest)

let next (gs : gamestate) : gamestate =
  if game_over gs then raise (Gameover gs.score)
  else
    match gs.beat_type with
    | Left 0 -> { (fall_beats gs) with beat_type = Idle }
    | Left x -> { (fall_beats gs) with beat_type = Left (x - 1) }
    | Right 0 -> { (fall_beats gs) with beat_type = Idle }
    | Right x -> { (fall_beats gs) with beat_type = Right (x - 1) }
    | Idle -> fall_beats gs

let add_beat (gs : gamestate) : gamestate =
  let beat_set = List.nth beats (Random.int (List.length beats)) in
  (* Beats only enter the frame in the next step *)
  { gs with beats = gs.beats @ beat_set; num_beats = gs.num_beats - 1 }

let add_don (gs : gamestate) : gamestate =
  let beat_set = [ (max_height + 1, Don) ]; in
  (* Beats only enter the frame in the next step *)
  { gs with beats = gs.beats @ beat_set; num_beats = gs.num_beats - 1 }

let add_ka (gs : gamestate) : gamestate =
  let beat_set = [ (max_height + 1, Ka) ]; in
  (* Beats only enter the frame in the next step *)
  { gs with beats = gs.beats @ beat_set; num_beats = gs.num_beats - 1 }
