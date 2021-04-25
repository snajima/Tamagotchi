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
  | _ -> draw_char c

(** Draw a single pixel *)
let draw_pixel x y s c =
  Graphics.set_color c;
  Graphics.fill_rect (s * x) (s * y) s s

(** [draw_img] draws [img] with center at [x] and [y] in screen
    coordinates *)
let draw_img img x y =
  let pixel_array = dump_image img in
  let w = Array.length (Array.get pixel_array 0)
  and h = Array.length pixel_array in
  draw_image img ((x * scale) - (w / 2)) ((y * scale) - (h / 2))

(** [draw_img] draws [img] with lower left corner at [x] and [y] in
    screen coordinates *)
let draw_img_ll img x y =
  Graphics.set_color Graphics.white;
  Graphics.draw_image img (x * scale) (y * scale)

(** [process_anims] process a list of animations and render the current
    frame on the graphics context *)
let process_anims (anims : animation list) : unit =
  match anims with
  | [] -> () (* Done: no more animations *)
  | anim :: t -> failwith ""

(** [increment_anims] returns a list of animations with all animations
    in [anims] incremented to the next frame *)
let rec increment_anims (anims : animation list) : animation list =
  List.map next_frame anims
