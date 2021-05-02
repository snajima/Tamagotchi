open Dolphin
open Gui
open Animation

(* Stores score *)
exception Gameover of int

type vs = Gui.viewstate

type game_var = {
  mutable game : Dolphin.gamestate;
  mutable speed : int;
  mutable rock_speed : int;
  row_scale : int;
}

let g =
  {
    game = init_game ();
    speed = 5;
    rock_speed = 50;
    row_scale = 120 / Dolphin.max_height;
  }

let vs : viewstate = { default_vs with animations = [] }

(* Lower the number, the faster the speed. Thus this value is a lower
   bound on the speed *)
let max_speed = 1

let lane_width = 25

let dolphin_cy = 5

let rock_start_y = 115

(** [dolphin_cy] calculates the cy position of the dolphin based on the
    lane number.

    - 0 is the Left Lane
    - 1 is the Middle Lane
    - 2 is the Right Lane*)
let lane_cx (lane : int) =
  match lane with
  | 0 -> 20 (* Left lane *)
  | 1 -> 60 (* Middle Lane *)
  | 2 -> 100 (* Right lane *)
  | _ -> failwith ("Invalid lane: " ^ string_of_int lane)

let speed_up (speed : int) : int =
  if speed <= max_speed then max_speed else speed - 1

(** [clear_lane] clears the graphics context of the lane *)
let clear_lane (lane : int) =
  draw_pixels (lane_cx lane) (default_vs.maxy / 2) lane_width
    default_vs.maxy Graphics.white

let dolphin_init s =
  Graphics.clear_graph ();
  g.game <- init_game ();
  s.animations <- [ { rock_static with cx = lane_cx 0; cy = 120 } ];
  draw_pixels_ll 0 0 5 120 Graphics.black;
  (* draw_pixels_ll 35 0 5 120 Graphics.black; *)
  (* draw_pixels_ll 75 0 5 120 Graphics.black; *)
  draw_pixels_ll 115 0 5 120 Graphics.black

let dolphin_exit s =
  (* REPLACE draw user score on screen for a while then return to home
     screen*)
  print_endline "Bye"

let dolphin_except s ex =
  match ex with
  | Dolphin.Gameover score ->
      gameover_screen 500 score "You're Trash"
        { gg_static with cx = vs.maxx / 2; cy = vs.maxy / 2 }
        s
      (* print_endline (string_of_int score) *)
  | _ -> raise ex

let dolphin_key s c =
  match c with
  | 'a' -> g.game <- process_left g.game
  | 's' -> g.game <- process_middle g.game
  | 'd' -> g.game <- process_right g.game
  | _ -> print_endline "Invalid Key_pressed"

let rec get_rocks_anims
    (rocks : (int * int) list)
    (lst_so_far : Animation.animation list) : Animation.animation list =
  match rocks with
  | [] -> lst_so_far
  | (lane, row) :: t ->
      get_rocks_anims t
        ({ rock_static with cx = lane_cx lane; cy = row * g.row_scale }
         :: lst_so_far)

let get_player_anims (lane : Dolphin.lane) : Animation.animation =
  let player_cx =
    match lane with
    | Left -> lane_cx 0
    | Middle -> lane_cx 1
    | Right -> lane_cx 2
  in
  { dolphin_static with cx = player_cx; cy = dolphin_cy }

let get_animations (game : Dolphin.gamestate) : Animation.animation list
    =
  let rock_anims = get_rocks_anims (get_rocks g.game) [] in
  let player_anims = get_player_anims (get_dolphin_lane g.game) in
  player_anims :: rock_anims

(** Debug function to view where the rocks are *)
let string_of_rocks (rocks : (int * int) list) : string =
  String.concat " "
    (List.map
       (fun (x, y) ->
         "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")")
       rocks)

let dolphin_step s =
  (* g.game |> get_rocks |> string_of_rocks |> print_endline; *)
  (* Add Rocks *)
  if s.tick mod g.rock_speed = 0 then g.game <- add_rock g.game;
  (* Step Game *)
  if s.tick mod g.speed = 0 then g.game <- next g.game;
  (* Update Animations *)
  if s.tick mod g.speed = 0 then s.animations <- get_animations g.game;
  if s.tick = 3999 then g.speed <- speed_up g.speed;
  (* print_endline (string_of_int g.speed); *)
  (* print_endline (string_of_int s.tick); *)
  s.tick <- (s.tick + 1) mod 4000

let dolphin_predraw s =
  clear_lane 0;
  clear_lane 1;
  clear_lane 2

let draw () =
  draw_loop vs dolphin_init dolphin_exit dolphin_key dolphin_except
    dolphin_step dolphin_predraw

(* For debugging. Uncomment the following line and run [make homemode] *)
(* let _ = draw () *)
