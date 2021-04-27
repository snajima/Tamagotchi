open Graphics

type pixel_array = int array array

type animation = {
  (* Drawing information *)
  frames : pixel_array list;
  current : int;
  total : int;
  (* Position information *)
  cx : int;
  cy : int;
}
(* ---------------------------------------------------------------- *)
(* -------------------- Animation Get/Setters --------------------- *)
(* ---------------------------------------------------------------- *)

(** [curr_frame] gets the current image in the animation frame *)
let curr_frame (anim : animation) : pixel_array =
  List.nth anim.frames anim.current

(** [next_frame] returns an animation that represents the next animation
    frame *)
let next_frame (anim : animation) : animation =
  { anim with current = (anim.current + 1) mod anim.total }

(* ---------------------------------------------------------------- *)
(* -------------------- Animation Constructors -------------------- *)
(* ---------------------------------------------------------------- *)

(** Returns an array with [item] repeated [num] times *)
let repeat (num : int) (item : 'a) = Array.make num item

(** Vertically scales the inputted image up by [num]*)
let vert_scale (num : int) (p_array : pixel_array) : pixel_array =
  Array.fold_left Array.append [||] (Array.map (repeat num) p_array)

(** Horizontally scales the inputted image up by [num]*)
let hor_scale (num : int) (p_array : pixel_array) : pixel_array =
  Array.map
    (fun row ->
      Array.fold_left Array.append [||] (Array.map (repeat num) row))
    p_array

(** Evenly scales the image up by [num]*)
let scale (num : int) (p_array : pixel_array) : pixel_array =
  p_array |> vert_scale num |> hor_scale num

let mirror (img : image) : image =
  let array_rev array =
    Array.to_list array |> List.rev |> Array.of_list
  in
  Array.map array_rev (dump_image img) |> make_image

let n = 0x000000
