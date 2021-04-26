open Dolphin
open Gui
open Animation

(* Stores score *)
exception Gameover of int

type vs = Gui.viewstate

type game_var = {
  mutable game : Dolphin.gamestate;
  mutable speed : int;
  row_scale : int;
}

let g = { game = init_game (); speed = 10; row_scale = 10 }

let lane_width = 25

let dolphin_cy = 20

let rock_start_y = 115

(** [dolphin_cy] calculates the cy position of the dolphin based on the
    lane number.

    - 1 is the Left Lane
    - 2 is the Middle Lane
    - 3 is the Right Lane*)
let lane_cx (lane : int) =
  match lane with
  | 1 -> 20 (* Left lane *)
  | 2 -> 60 (* Middle Lane *)
  | 3 -> 100 (* Right lane *)
  | _ -> failwith "Invalid lane"

(** [clear_lane] clears the graphics context of the lane *)
let clear_lane (lane : int) =
  draw_pixels (lane_cx lane) (default_vs.maxy / 2) lane_width
    default_vs.maxy Graphics.white

let dolphin_init s =
  (* REMOVE LATER *)
  Graphics.open_graph
    (" "
    ^ string_of_int (s.scale * s.maxx)
    ^ "x"
    ^ string_of_int (s.scale * s.maxy));
  (* Reset animations *)
  s.animations <-
    [ { rock_static with cx = lane_cx 1; cy = rock_start_y } ];
  draw_pixels_ll 0 0 5 120 Graphics.black;
  (* draw_pixels 35 0 5 120 Graphics.black; *)
  (* draw_pixels 75 0 5 120 Graphics.black; *)
  draw_pixels_ll 115 0 5 120 Graphics.black

let dolphin_exit s = ()

let dolphin_except s ex =
  match ex with
  | Dolphin.Gameover score -> print_endline (string_of_int score)
  | _ -> failwith "Invalid exception"

let dolphin_key s c =
  match c with
  | 'a' -> g.game <- process_left g.game
  | 's' -> g.game <- process_middle g.game
  | 'd' -> g.game <- process_right g.game
  | _ -> print_endline "Invalid Key_pressed"

let rec process_rocks
    (rocks : (int * int) list)
    (lst_so_far : Animation.animation list) : Animation.animation list =
  match rocks with
  | [] -> lst_so_far
  | (lane, row) :: t ->
      process_rocks t
        ({ rock_static with cx = lane_cx lane; cy = row * g.row_scale }
         :: lst_so_far)

let dolphin_step s =
  g.game <- next g.game;
  if s.tick mod g.speed = 0 then
    s.animations <- process_rocks (get_rocks g.game) [];
  s.tick <- (s.tick + 1) mod 10

let dolphin_predraw s =
  clear_lane 1;
  clear_lane 2;
  clear_lane 3

let vs : viewstate = { default_vs with animations = [] }

let draw () =
  draw_loop vs dolphin_init dolphin_exit dolphin_key dolphin_except
    dolphin_step dolphin_predraw

(* For debugging. Uncomment the following line and run [make homemode] *)
let _ = draw ()
