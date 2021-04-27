open Drum
open Gui
open Animation

(* Stores score *)
exception Gameover of int

type vs = Gui.viewstate

type game_var = {
  mutable game : Drum.gamestate;
  mutable speed : int;
  mutable beat_speed : int;
  row_scale : int;
}

let g =
  {
    game = init_game ();
    speed = 5;
    beat_speed = 100;
    row_scale = 120 / Drum.max_height;
  }

let lane_width = 25

let beat_start_x = 115

let game = init_game ()

let drum_init s =
  (* REMOVE LATER *)
  Graphics.open_graph
    (" "
    ^ string_of_int (s.scale * s.maxx)
    ^ "x"
    ^ string_of_int (s.scale * s.maxy));
  draw_pixels_ll 0 80 120 1 Graphics.black;
  draw_pixels_ll 0 40 120 1 Graphics.black

let drum_exit s = 
  (* REPLACE draw user score on screen for a while then return to home
     screen*)
     print_endline "Bye"

let drum_except s ex =
  match ex with
  | Drum.Gameover score ->
      print_endline (string_of_int score);
      s.animations <-
        {
          gg_static with
          cx = default_vs.maxx / 2;
          cy = default_vs.maxy / 2;
        }
        :: s.animations
  | _ -> raise ex

let drum_key s c =
  match c with
  | 'a' -> (print_endline (string_of_int (get_score g.game));
    g.game <- process_left g.game)
  | 's' -> (print_endline (string_of_int (get_score g.game));
    g.game <- process_middle g.game)
  | 'd' -> (print_endline (string_of_int (get_score g.game));
    g.game <- process_right g.game)
  | _ -> print_endline "Invalid Key_pressed"

let rec get_beats_anims
    (beats : (int * Drum.color) list)
    (lst_so_far : Animation.animation list) : Animation.animation list =
  match beats with
  | [] -> lst_so_far
  | (height, Drum.Ka) :: t ->
      get_beats_anims t
        ({ ka_anim with cx = height * g.row_scale; cy = default_vs.maxy / 2 }
         :: lst_so_far)
  | (height, Drum.Don) :: t ->
      get_beats_anims t
        ({ don_anim with cx = height * g.row_scale; cy = default_vs.maxy / 2 }
         :: lst_so_far)

let get_animations (game : Drum.gamestate) : Animation.animation list
    =
  let rock_anims = get_beats_anims (get_beats g.game) [] in
  rock_anims

let drum_step s =
  (* g.game |> get_rocks |> string_of_rocks |> print_endline; *)
  (* Add Rocks *)
  if s.tick mod g.beat_speed = 0 then g.game <- add_beat g.game;
  (* Step Game *)
  if s.tick mod g.speed = 0 then g.game <- next g.game;
  (* Update Animations *)
  if s.tick mod g.speed = 0 then s.animations <- get_animations g.game;
  (* print_endline (string_of_int g.speed); *)
  (* print_endline (string_of_int s.tick); *)
  s.tick <- (s.tick + 1) mod 4000

let drum_predraw s =
(* TODO: Draw out pixels and calculate inputs of draw_pixels *)
  draw_pixels (default_vs.maxx / 2) (default_vs.maxy / 2) default_vs.maxx
    lane_width Graphics.white;
  draw_pixels 5 60 1 10 Graphics.black;
  draw_pixels 15 60 1 11 Graphics.black;
  draw_pixels 10 65 10 1 Graphics.black;
  draw_pixels 10 55 10 1 Graphics.black

let vs : viewstate = { default_vs with animations = [] }

let draw () =
  draw_loop vs drum_init drum_exit drum_key drum_except
    drum_step drum_predraw

(* For debugging. Uncomment the following line and run [make homemode] *)
let _ = draw ()
