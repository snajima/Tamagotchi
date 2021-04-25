open Graphics
open Animation

exception End

type viewstate = {
  mutable tick : int;
  mutable animations : animation list;
  maxx : int;
  maxy : int;
  scale : int;
  bc : Graphics.color;
  fc : Graphics.color;
}

let default_vs : viewstate =
  {
    tick = 0;
    animations = [];
    maxx = 120;
    maxy = 120;
    scale = 4;
    bc = Graphics.rgb 255 255 255;
    fc = Graphics.black;
  }

let scale = 4

(** Draw a single pixel *)
let draw_pixel x y s c =
  Graphics.set_color c;
  Graphics.fill_rect (s * x) (s * y) s s

(** [draw_img] converts [pixel_array] to an image and draws it with
    center at [x] and [y] in screen coordinates *)
let draw_img (x : int) (y : int) (p_array : pixel_array) : unit =
  let w = Array.length (Array.get p_array 0)
  and h = Array.length p_array in
  draw_image (p_array |> make_image)
    ((x * scale) - (w / 2))
    ((y * scale) - (h / 2))

(** [draw_img] converts [pixel_array] to an image and draws it with
    lower left corner at [x] and [y] in screen coordinates *)
let draw_img_ll (x : int) (y : int) (p_array : pixel_array) : unit =
  Graphics.set_color Graphics.white;
  Graphics.draw_image (p_array |> make_image) (x * scale) (y * scale)

(** [process_anims] process a list of animations and render the current
    frame on the graphics context *)
let rec process_anims (anims : animation list) : unit =
  match anims with
  | [] -> () (* Done: no more animations *)
  | anim :: t ->
      curr_frame anim |> draw_img anim.cx anim.cy;
      process_anims t

(** [increment_anims] returns a list of animations with all animations
    in [anims] incremented to the next frame *)
let rec increment_anims (anims : animation list) : animation list =
  List.map next_frame anims

(** [draw_loop] repeatedly updates the GUI. It first initiates the
    screen with [f_init] called on the sample function. It then calls
    [f_step] function to update the [state]. Then, it processes the user
    key input (if available) with the [f_key] function and updates
    [state] accordingly. Finally, it draws all the animations based on
    [state], and repeats the loop. This loop repeats indefinitely until
    the user issues a quit command. *)
let draw_loop (state : viewstate) f_init f_end f_key f_except f_step =
  f_init state;
  try
    while true do
      try
        let s = Graphics.wait_next_event [ Graphics.Poll ] in
        Graphics.set_color Graphics.white;
        f_step state;
        process_anims state.animations;
        if s.Graphics.keypressed then f_key state (Graphics.read_key ())
      with
      | End -> raise End
      | e -> f_except state e
    done
  with End -> f_end state
