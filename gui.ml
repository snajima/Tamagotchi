open Animation

exception End

type viewstate = {
  mutable tick : int;
  mutable anim_frame : int;
  mutable anim_init_func : unit -> animation;
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

let next_line () =
  let x, y = Graphics.current_point () in
  if y > 12 then Graphics.moveto 0 (y - 12) else Graphics.moveto 0 y

let handle_char c =
  match c with
  | '&' -> raise End
  | '\n' -> next_line ()
  | '\r' -> next_line ()
  | _ -> Graphics.draw_char c

let skel (state : viewstate) f_init f_end f_key f_mouse f_except =
  f_init ();
  try
    while true do
      try
        let s = Graphics.wait_next_event [ Graphics.Poll ]
        (* [ Graphics.Button_down; Graphics.Key_pressed ] *)
        and anim = state.anim_init_func () in
        step state;
        Graphics.set_color Graphics.white;
        Graphics.draw_image
          (curr_frame anim state.anim_frame)
          (42 * state.scale) (35 * state.scale);
        (* ------------------------------------------ *)
        (* Increment animation frame every 100 frames *)
        if state.tick mod 100 = 0 then
          state.anim_frame <- (state.anim_frame + 1) mod anim.total;
        (* print_endline (string_of_int state.anim_frame); *)
        (* ------------------------------------------ *)
        if s.Graphics.keypressed then f_key (Graphics.read_key ())
          (* print_endline (Char.escaped (Graphics.read_key ())) *)
        else if s.Graphics.button then
          f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
      with
      | End -> raise End
      | e -> f_except e
    done
  with End -> f_end ()

(** Draw a single pixel *)
let draw_pixel x y s c =
  Graphics.set_color c;
  Graphics.fill_rect (s * x) (s * y) s s

(** Draw the tool bars, setup screen*)
let setup_toolbars s =
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 0 (s.scale * 120) (s.scale * 20);
  Graphics.fill_rect 0 (100 * s.scale) (s.scale * 120) (s.scale * 20);
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 (10 * s.scale) (120 * s.scale) (100 * s.scale);
  (* Top row *)
  Graphics.draw_image (eat_icon ()) (0 * s.scale) (90 * s.scale);
  Graphics.draw_image (sleep_icon ()) (45 * s.scale) (90 * s.scale);
  Graphics.draw_image (toilet_icon ()) (90 * s.scale) (90 * s.scale);
  (* Bottom row *)
  Graphics.draw_image (play_icon ()) (0 * s.scale) (10 * s.scale);
  Graphics.draw_image (shop_icon ()) (45 * s.scale) (10 * s.scale);
  Graphics.draw_image (inventory_icon ()) (90 * s.scale) (10 * s.scale)

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

let mouse s x y =
  print_endline (String.concat " " [ string_of_int x; string_of_int y ])

let except s ex = ()

let key s c =
  draw_pixel s.x s.y s.scale s.fc;
  (match c with
  | 'w' -> if s.y < s.maxy then s.y <- s.y + 1
  | 's' -> if s.y > 0 then s.y <- s.y - 1
  | 'a' -> if s.x > 0 then s.x <- s.x - 1
  | 'd' -> if s.x < s.maxx then s.x <- s.x + 1
  | 'e' -> s.anim_init_func <- eat_anim
  | 'c' -> Graphics.clear_graph ()
  | 'x' -> raise End
  | _ -> ());
  print_endline (Char.escaped c)

(* This will be the model data *)
let sample_state : viewstate =
  {
    tick = 0;
    anim_frame = 0;
    anim_init_func = eat_anim;
    maxx = 120;
    maxy = 120;
    x = 60;
    y = 60;
    scale = 4;
    back_color = Graphics.rgb 255 255 255;
    fc = Graphics.black;
  }

let draw () =
  skel sample_state (init sample_state) (exit sample_state)
    (key sample_state) (mouse sample_state) (except sample_state)
