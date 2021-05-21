open Elementals
open Gui
open Animation

exception Gameover of int

type vs = Gui.viewstate

type game_var = { mutable game : Elementals.gamestate }

let g = { game = init_game () }

let elementals_exit s =
  (* REPLACE draw user score on screen for a while then return to home
     screen*)
  print_endline "Bye"

let elementals_except s ex =
  match ex with
  | Elementals.Gameover w_l ->
       if w_l then (print_endline "Congrats, you won!";) else (print_endline "Boo, you lost!";);
      s.animations <-
        {
          gg_static with
          cx = default_vs.maxx / 2;
          cy = default_vs.maxy / 2;
        }
        :: s.animations
  | _ -> raise ex

let elementals_key s c =
  match c with
  | 'a' -> g.game <- play_water g.game
  | 's' -> g.game <- play_fire g.game
  | 'd' -> g.game <- play_leaf g.game
  | _ -> print_endline "Invalid Key_pressed"
