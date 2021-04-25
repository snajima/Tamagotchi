open Animation
open Gui

type viewstate = {
  mutable tick : int;
  (* mutable anim_frame : int; *)
  (* mutable anim_init_func : unit -> animation; *)
  mutable animations : animation list;
  maxx : int;
  maxy : int;
  mutable x : int;
  mutable y : int;
  scale : int;
  back_color : Graphics.color;
  fc : Graphics.color;
}

(** [step] increments the tick in the state, used for conservatively
    animating *)
let step (state : viewstate) : unit =
  state.tick <- (state.tick + 1) mod 100

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
let init s () =
  (* Window has width s.scale * maxx and height s.scale * s.maxy *)
  Graphics.open_graph
    (" "
    ^ string_of_int (s.scale * s.maxx)
    ^ "x"
    ^ string_of_int (s.scale * s.maxy));
  setup_toolbars s

(** Main exit function for HomeMode *)
let exit s () =
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
  | 'w' -> if s.y < s.maxy then s.y <- s.y + 1
  | 's' -> if s.y > 0 then s.y <- s.y - 1
  | 'a' -> if s.x > 0 then s.x <- s.x - 1
  | 'd' -> if s.x < s.maxx then s.x <- s.x + 1
  | 'c' -> Graphics.clear_graph ()
  | 'x' -> raise End
  | _ -> ());
  print_endline (Char.escaped c)

let skel (state : viewstate) f_init f_end f_key f_except =
  f_init ();
  try
    while true do
      try
        let s =
          Graphics.wait_next_event [ Graphics.Poll ]
          (* [ Graphics.Button_down; Graphics.Key_pressed ] *)
          (* and anim = state.anim_init_func ()  *)
        in
        step state;
        Graphics.set_color Graphics.white;
        process_anims state.animations;
        (* ------------------------------------------ *)
        (* Increment animation frame every 100 frames *)
        if state.tick mod 100 = 0 then
          state.animations <- increment_anims state.animations;
        (* ------------------------------------------ *)
        if s.Graphics.keypressed then f_key (Graphics.read_key ())
      with
      | End -> raise End
      | e -> f_except e
    done
  with End -> f_end ()

let test_anims = [ test_anim (); eat_anim () ]

(* This will be the model data *)
let sample_state : viewstate =
  {
    tick = 0;
    animations = [];
    maxx = 120;
    maxy = 120;
    x = 60;
    y = 60;
    scale = 4;
    back_color = Graphics.rgb 255 255 255;
    fc = Graphics.black;
  }

let draw () =
  skel sample_state (init sample_state) (exit sample_state) handle_char
    (except sample_state)

(* For debugging. Uncomment the following line and run [make homemode] *)
let _ = draw ()
