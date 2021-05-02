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

(** [draw_pixels] draws a rectangle of pixels centered at [cx] and [cy]
    with width [sx] and height [sy] and color [c] *)
let draw_pixels cx cy sx sy c =
  Graphics.set_color c;
  Graphics.fill_rect
    ((cx - (sx / 2)) * default_vs.scale)
    ((cy - (sy / 2)) * default_vs.scale)
    (sx * default_vs.scale) (sy * default_vs.scale)

(** [draw_pixels] draws a rectangle of pixels with lower left corner at
    [x] and [y] with width [sx] and height [sy] and color [c] *)
let draw_pixels_ll x y sx sy c =
  Graphics.set_color c;
  Graphics.fill_rect (x * default_vs.scale) (y * default_vs.scale)
    (sx * default_vs.scale) (sy * default_vs.scale)

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

let draw_message
    (cx : int)
    (cy : int)
    (size : int)
    (c : color)
    (message : string) =
  (* Setup *)
  set_font
    ("-*-fixed-medium-r-semicondensed--" ^ string_of_int size
   ^ "-*-*-*-*-*-iso8859-1");
  set_color c;
  (* Pointer positioning*)
  moveto cx cy;
  let mx, my = text_size message in
  rmoveto (-mx / 2) (-my / 2);
  (* Draw *)
  draw_string message

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

(** [draw_loop] repeatedly updates the GUI.

    - It first initiates the screen with [f_init] called on the sample
      function.
    - It then calls [f_step] function to update the [vs].
    - Then, it calls the [f_predraw] function and updates [vs]
      accordingly.
    - Then, it processes the user key input (if available) with the
      [f_key] function and updates [vs] accordingly.
    - Finally, it draws all the stationary animations based on [vs], and
      repeats the loop.

    This loop repeats indefinitely until the user issues a quit command. *)
let draw_loop
    (vs : viewstate)
    f_init
    f_end
    f_key
    f_except
    f_step
    f_predraw =
  (try f_init vs with Graphic_failure msg -> print_endline msg);
  Graphics.auto_synchronize false;
  try
    while true do
      try
        let s = Graphics.wait_next_event [ Graphics.Poll ] in
        Graphics.set_color Graphics.white;
        f_step vs;
        f_predraw vs;
        process_anims vs.animations;
        Graphics.synchronize ();
        if s.Graphics.keypressed then f_key vs (Graphics.read_key ())
      with
      | End -> raise End
      | e -> f_except vs e
    done
  with
  | End -> f_end vs
  | Graphic_failure msg -> print_endline msg

let gameover_screen
    (length : int)
    (score : int)
    (message : string)
    (anim : animation)
    (vs : viewstate) =
  draw_loop vs
    (* Init *)
      (fun s ->
      s.tick <- 0;
      s.animations <- anim :: s.animations;
      let cx = vs.maxx * scale / 2
      and cy = vs.maxy * scale / 2
      and dy = vs.maxy * scale / 10
      and score_string = "Score: " ^ string_of_int score
      and quit_string = "Press 'S' to Quit" in
      (* Draw Score *)
      draw_message cx (cy - dy) 50 red score_string;
      (* Draw Custom Message *)
      draw_message cx (cy - (2 * dy)) 40 red message;
      (* Draw Quit message *)
      draw_message cx (cy - (3 * dy)) 30 green quit_string;
      set_color white)
    (fun s -> ())
    (fun s c ->
      match c with
      | 's' -> raise End
      | _ -> print_endline "Invalid Key_pressed")
    (fun s ex -> ())
    (fun s ->
      if s.tick = length then raise End else s.tick <- s.tick + 1)
    (fun s -> ());
  raise End
