open Graphics
open Animation

exception End

let scale = 4

let next_line () =
  let x, y = Graphics.current_point () in
  if y > 12 then Graphics.moveto 0 (y - 12) else Graphics.moveto 0 y

let handle_char c =
  match c with
  | '&' -> raise End
  | '\n' -> next_line ()
  | '\r' -> next_line ()
  | _ ->
      print_endline "hello";
      draw_char c

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
