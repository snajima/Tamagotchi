open Graphics
open Animation

type viewstate = {
  mutable tick : int;
  mutable animations : animation list;
  maxx : int;
  maxy : int;
  scale : int;
  bc : Graphics.color;
  fc : Graphics.color;
}

exception End

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

let draw_pixels cx cy sx sy c =
  Graphics.set_color c;
  Graphics.fill_rect
    ((cx - (sx / 2)) * default_vs.scale)
    ((cy - (sy / 2)) * default_vs.scale)
    (sx * default_vs.scale) (sy * default_vs.scale)

let draw_pixels_ll x y sx sy c =
  Graphics.set_color c;
  Graphics.fill_rect (x * default_vs.scale) (y * default_vs.scale)
    (sx * default_vs.scale) (sy * default_vs.scale)

let draw_img (x : int) (y : int) (p_array : pixel_array) : unit =
  let w = Array.length (Array.get p_array 0)
  and h = Array.length p_array in
  draw_image (p_array |> make_image)
    ((x * scale) - (w / 2))
    ((y * scale) - (h / 2))

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

let rec process_anims (anims : animation list) : unit =
  match anims with
  | [] -> () (* Done: no more animations *)
  | anim :: t ->
      curr_frame anim |> draw_img anim.cx anim.cy;
      process_anims t

let rec increment_anims (anims : animation list) : animation list =
  List.map next_frame anims

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
      draw_message cx (cy - dy) 50 black score_string;
      (* Draw Custom Message *)
      draw_message cx (cy - (2 * dy)) 40 black message;
      (* Draw Quit message *)
      draw_message cx (cy - (3 * dy)) 30 black quit_string;
      set_color white)
    (fun s -> ())
    (fun s c ->
      match c with
      | 's' ->
          clear_graph ();
          raise End
      | _ -> print_endline "Invalid Key_pressed")
    (fun s ex -> ())
    (fun s ->
      if s.tick = length then raise End else s.tick <- s.tick + 1)
    (fun s -> ());
  raise End

let gameover_screen_clear
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
      clear_graph ();
      draw_pixels_ll 0 0 5 120 Graphics.black;
      draw_pixels_ll 115 0 5 120 Graphics.black;
      (* Draw Score *)
      draw_message cx (cy - dy) 50 black score_string;
      (* Draw Custom Message *)
      draw_message cx (cy - (2 * dy)) 40 black message;
      (* Draw Quit message *)
      draw_message cx (cy - (3 * dy)) 30 black quit_string;
      set_color white)
    (fun s -> ())
    (fun s c ->
      match c with
      | 's' ->
          clear_graph ();
          raise End
      | _ -> print_endline "Invalid Key_pressed")
    (fun s ex -> ())
    (fun s ->
      if s.tick = length then raise End else s.tick <- s.tick + 1)
    (fun s -> ());
  raise End

let gameover_screen_no_score
    (length : int)
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
      and quit_string = "Press 'S' to Quit" in
      (* Draw Custom Message *)
      draw_message cx (cy - (3 * dy)) 40 black message;
      (* Draw Quit message *)
      draw_message cx (cy - (4 * dy)) 30 black quit_string;
      set_color white)
    (fun s -> ())
    (fun s c ->
      match c with
      | 's' ->
          clear_graph ();
          raise End
      | _ -> print_endline "Invalid Key_pressed")
    (fun s ex -> ())
    (fun s ->
      if s.tick = length then raise End else s.tick <- s.tick + 1)
    (fun s -> ());
  raise End
