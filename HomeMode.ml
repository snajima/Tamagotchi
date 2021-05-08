open Animation
open Gui

type game_flags = {
  mutable dolphin : bool;
  mutable drum : bool;
  mutable elements : bool;
}

let my_game_flags = { dolphin = false; drum = false; elements = false }

let reset_game_flags () =
  my_game_flags.dolphin <- false;
  my_game_flags.drum <- false;
  my_game_flags.elements <- false

(** Draw the tool bars, setup screen*)
let setup_toolbars s =
  (* Draw top and bottom black bars *)
  draw_pixels_ll 0 0 120 10 Graphics.black;
  draw_pixels_ll 0 110 120 10 Graphics.black;
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
  Graphics.set_color s.bc;
  Graphics.fill_rect 0 0 (s.scale * s.maxx) (s.scale * s.maxy);
  setup_toolbars s

(** Main exit function for HomeMode *)
let exit s =
  Graphics.close_graph ();
  (* TODO: save the current state of Tamagotchi to json *)
  print_endline "";
  print_endline
    "Thanks for playing! Your Tamagotchi will be waiting for your \
     return";
  print_endline ""

(** Main exception function for HomeMode *)
let except s ex = ()

(** [key] processes the [c] character pressed and updates the state [s]
    accordingly *)
let key s c =
  (* draw_pixel s.x s.y s.scale s.fc; *)
  (match c with
  | '1' -> Dolphinview.draw ()
  | '2' -> Drumview.draw ()
  | '3' -> failwith "unimplemented"
  | 'x' -> raise End
  | _ -> ());
  print_endline (Char.escaped c)

(** [step] increments the tick in the state, used for conservatively
    animating *)
let step (state : viewstate) : unit =
  (* incr animations every 100 frames *)
  if state.tick mod 10 = 0 then
    state.animations <- increment_anims state.animations;
  state.tick <- (state.tick + 1) mod 10;
  Graphics.set_color state.bc;
  Graphics.fill_rect 0 0
    (state.scale * state.maxx)
    (state.scale * state.maxy);
  setup_toolbars state

let predraw (state : viewstate) : unit = ()

let test_anims = [ test_anim; eat_anim ]

(* This will be the model data *)
let sample_state : viewstate =
  { default_vs with animations = test_anims }

let draw () = draw_loop sample_state init exit key except step predraw

(* For debugging. Uncomment the following line and run [make homemode] *)
(* TODO: Load from json (if it exists), otherwise launch new Tamagotchi
   session *)
let _ = draw ()
