open Drum
open Gui
open Animation

type vs = Gui.viewstate

type game_vars = {
  mutable game : Drum.gamestate;
  mutable speed : int;
  mutable beat_speed : int;
  row_scale : int;
}

let g =
  {
    game = init_game ();
    speed = 1;
    beat_speed = 150;
    row_scale = 120 / Drum.max_height;
  }

let vs : viewstate = { default_vs with animations = [] }

let lane_width = 25

let game = init_game ()

let drum_init s =
  Graphics.clear_graph ();
  g.game <- init_game ();
  draw_pixels_ll 0 80 120 1 Graphics.black;
  draw_pixels_ll 0 40 120 1 Graphics.black

let drum_exit s =
  (* REPLACE draw user score on screen for a while then return to home
     screen*)
  ()

let drum_except s ex =
  match ex with
  | Drum.Gameover score ->
      Graphics.clear_graph ();
      draw_message
        ((default_vs.maxx * default_vs.scale / 2) + 50)
        ((default_vs.maxy * default_vs.scale) - 20)
        25 Graphics.black "Sets of beats left: 0";
      gameover_screen 500 (get_score g.game) "Game Over"
        { drum_anim with cx = vs.maxx / 2; cy = (vs.maxy / 2) + 20 }
        s
  | _ -> raise ex

let drum_key s c =
  match c with
  | 'a' -> g.game <- process_left g.game
  | 's' -> g.game <- process_middle g.game
  | 'd' -> g.game <- process_right g.game
  | 'x' -> raise End
  | _ -> print_endline "Invalid Key_pressed"

let b_anims_helper
    (height : int)
    (lst_so_far : Animation.animation list)
    (anim : Animation.animation) : Animation.animation list =
  { anim with cx = height * g.row_scale; cy = default_vs.maxy / 2 }
  :: lst_so_far

let rec get_beats_anims
    (beats : (int * Drum.color) list)
    (lst_so_far : Animation.animation list) : Animation.animation list =
  match beats with
  | [] -> lst_so_far
  | (height, Drum.Ka) :: t ->
      get_beats_anims t (b_anims_helper height lst_so_far ka_anim)
  | (height, Drum.Don) :: t ->
      get_beats_anims t (b_anims_helper height lst_so_far don_anim)

let get_player_anims (beat_type : Drum.beat) : Animation.animation =
  match beat_type with
  | Left _ ->
      { left_drum_anim with cx = vs.maxx / 2; cy = (vs.maxy / 2) + 20 }
  | Right _ ->
      { right_drum_anim with cx = vs.maxx / 2; cy = (vs.maxy / 2) + 20 }
  | Idle ->
      {
        idle_drummer_anim with
        cx = vs.maxx / 2;
        cy = (vs.maxy / 2) + 20;
      }

let get_animations (game : Drum.gamestate) : Animation.animation list =
  let beat_anims = get_beats_anims (get_beats g.game) [] in
  let player_anims = get_player_anims (get_beat_type g.game) in
  player_anims :: beat_anims

let drum_step s =
  if s.tick mod g.beat_speed = 0 then g.game <- add_beat g.game;
  (* Step Game *)
  if s.tick mod g.speed = 0 then g.game <- next g.game;
  (* Update Animations *)
  if s.tick mod g.speed = 0 then s.animations <- get_animations g.game;
  s.tick <- (s.tick + 1) mod 4000

let drum_predraw_helper () =
  draw_message
    ((default_vs.maxx * default_vs.scale / 2) + 50)
    ((default_vs.maxy * default_vs.scale) - 20)
    25 Graphics.black
    ("Sets of beats left: " ^ string_of_int (get_num_beats g.game));
  draw_message
    ((default_vs.maxx * default_vs.scale / 2) + 130)
    ((default_vs.maxy * default_vs.scale) - 45)
    25 Graphics.black
    ("Combo: " ^ string_of_int (get_combo g.game));
  draw_message
    ((default_vs.maxx * default_vs.scale / 2) + 130)
    ((default_vs.maxy * default_vs.scale) - 70)
    25 Graphics.black
    ("Score: " ^ string_of_int (get_score g.game))

let drum_predraw s =
  (* TODO: Draw out pixels and calculate inputs of draw_pixels *)
  draw_pixels (default_vs.maxx / 2) (default_vs.maxy / 2)
    default_vs.maxx lane_width Graphics.white;
  draw_pixels 5 60 1 10 Graphics.black;
  draw_pixels 15 60 1 11 Graphics.black;
  draw_pixels 10 65 10 1 Graphics.black;
  draw_pixels 10 55 10 1 Graphics.black;
  draw_pixels 50 105 120 lane_width Graphics.white;
  drum_predraw_helper ()

let draw () =
  draw_loop vs drum_init drum_exit drum_key drum_except drum_step
    drum_predraw

(* For debugging. Uncomment the following line and run [make drumview] *)
(* let _ = draw () *)
