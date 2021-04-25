open Animation
open Gui

(** Draw the tool bars, setup screen*)
let setup_toolbars s =
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 0 (s.scale * 120) (s.scale * 20);
  Graphics.fill_rect 0 (100 * s.scale) (s.scale * 120) (s.scale * 20);
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 (10 * s.scale) (120 * s.scale) (100 * s.scale);
  (* Top row *)
  Graphics.draw_image
    (Graphics.make_image eat_icon)
    (0 * s.scale) (90 * s.scale);
  Graphics.draw_image
    (Graphics.make_image sleep_icon)
    (45 * s.scale) (90 * s.scale);
  Graphics.draw_image
    (Graphics.make_image toilet_icon)
    (90 * s.scale) (90 * s.scale);
  (* Bottom row *)
  Graphics.draw_image
    (Graphics.make_image play_icon)
    (0 * s.scale) (10 * s.scale);
  Graphics.draw_image
    (Graphics.make_image shop_icon)
    (45 * s.scale) (10 * s.scale);
  Graphics.draw_image
    (Graphics.make_image inventory_icon)
    (90 * s.scale) (10 * s.scale)

(** Main init function for HomeMode*)
let init s =
  (* Window has width s.scale * maxx and height s.scale * s.maxy *)
  Graphics.open_graph
    (" "
    ^ string_of_int (s.scale * s.maxx)
    ^ "x"
    ^ string_of_int (s.scale * s.maxy));
  setup_toolbars s

(** Main exit function for HomeMode *)
let exit s =
  Graphics.close_graph ();
  print_endline "";
  print_endline
    "Thanks for playing! Your Tamagotchi will be waiting for your \
     return";
  print_endline ""

let except s ex = ()

let key s c =
  (* draw_pixel s.x s.y s.scale s.fc; *)
  (match c with
  | 'c' -> Graphics.clear_graph ()
  | 'x' -> raise End
  | _ -> ());
  print_endline (Char.escaped c)

(** [step] increments the tick in the state, used for conservatively
    animating *)
let step (state : viewstate) : unit =
  (* incr animations every 100 frames *)
  if state.tick mod 1000 = 0 then
    state.animations <- increment_anims state.animations;
  state.tick <- (state.tick + 1) mod 1000

let test_anims = [ test_anim; eat_anim ]

(* This will be the model data *)
let sample_state : viewstate =
  { default_vs with animations = test_anims }

let draw () = draw_loop sample_state init exit key except step

(* For debugging. Uncomment the following line and run [make homemode] *)
let _ = draw ()
