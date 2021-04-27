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

let t = 0xFFFFFF

let r = Graphics.red

let neutral_f1 =
  scale 10
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; n; n; t; t; t; t; t; n; n; t; t; t |];
      [| t; t; n; n; n; n; t; t; t; n; n; n; n; t; t |];
      [| t; t; n; n; n; n; n; n; n; n; n; n; n; t; t |];
      [| t; t; n; n; t; t; t; t; t; t; t; n; n; t; t |];
      [| t; t; n; t; t; n; t; t; t; n; t; t; n; t; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; n; n; n; t; t; t; t; n; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; n; t; t; n; t; t; t; t; t; n; t; n; t; t |];
      [| t; t; n; n; t; t; t; t; t; t; n; n; n; t; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; n; t; n; n; n; t; n; t; t; t; t |];
      [| t; t; t; t; n; t; n; t; t; n; t; t; t; t; t |];
      [| t; t; t; t; t; n; t; t; t; t; t; t; t; t; t |];
    |]

let neutral_f2 =
  scale 10
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; n; n; t; t; t; t; t; n; n; t; t; t |];
      [| t; t; n; n; n; n; t; t; t; n; n; n; n; t; t |];
      [| t; t; n; n; n; n; n; n; n; n; n; n; n; t; t |];
      [| t; t; n; n; t; t; t; t; t; t; t; n; n; t; t |];
      [| t; t; n; t; t; n; t; t; t; n; t; t; n; t; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; n; n; n; t; t; t; t; n; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; t; n; t; t; n; t; t; t; t; t; n; t; n; t |];
      [| t; t; t; n; n; t; t; t; t; t; t; n; n; n; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; t; n; t; t |];
      [| t; t; t; t; t; n; t; n; n; n; t; n; t; t; t |];
      [| t; t; t; t; t; n; t; n; t; t; n; t; t; t; t |];
      [| t; t; t; t; t; t; n; t; t; t; t; t; t; t; t |];
    |]

let wide_f1 =
  scale 10
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; n; n; t; t; t; t; t; n; n; t; t; t |];
      [| t; t; n; n; n; n; t; t; t; n; n; n; n; t; t |];
      [| t; t; n; n; n; n; n; n; n; n; n; n; n; t; t |];
      [| t; t; n; n; t; t; t; t; t; t; t; n; n; t; t |];
      [| t; t; n; t; n; n; t; t; t; n; n; t; n; t; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; t; n; t; t; t; t; t; n; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; n; t; t; n; t; t; t; t; t; n; t; n; t; t |];
      [| t; t; n; n; t; t; t; t; t; t; n; n; n; t; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; n; t; n; n; n; t; n; t; t; t; t |];
      [| t; t; t; t; n; t; n; t; t; n; t; t; t; t; t |];
      [| t; t; t; t; t; n; t; t; t; t; t; t; t; t; t |];
    |]

let wide_f2 =
  scale 10
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; n; n; t; t; t; t; t; n; n; t; t; t |];
      [| t; t; n; n; n; n; t; t; t; n; n; n; n; t; t |];
      [| t; t; n; n; n; n; n; n; n; n; n; n; n; t; t |];
      [| t; t; n; n; t; t; t; t; t; t; t; n; n; t; t |];
      [| t; t; n; t; n; n; t; t; t; n; n; t; n; t; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; t; n; t; t; t; t; t; n; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; t; n; t; t; n; t; t; t; t; t; n; t; n; t |];
      [| t; t; t; n; n; t; t; t; t; t; t; n; n; n; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; t; n; t; t |];
      [| t; t; t; t; t; n; t; n; n; n; t; n; t; t; t |];
      [| t; t; t; t; t; n; t; n; t; t; n; t; t; t; t |];
      [| t; t; t; t; t; t; n; t; t; t; t; t; t; t; t |];
    |]

let idle =
  scale 10
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; n; n; t; t; t; t; t; n; n; t; t; t |];
      [| t; t; n; n; n; n; t; t; t; n; n; n; n; t; t |];
      [| t; t; n; n; n; n; n; n; n; n; n; n; n; t; t |];
      [| t; t; n; n; t; t; t; t; t; t; t; n; n; t; t |];
      [| t; t; n; t; n; n; t; t; t; n; n; t; n; t; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; t; n; t; t; t; t; t; n; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; n; t; n; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; n; t; n; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; t; n; t; t; t; n; t; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; n; t; n; n; n; t; n; t; t; t; t |];
      [| t; t; t; t; n; t; n; t; n; t; n; t; t; t; t |];
      [| t; t; t; t; t; n; t; t; t; n; t; t; t; t; t |];
    |]

let eat_f1 =
  scale 10
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; n; t; t; t; t |];
      [| t; t; t; n; n; n; t; t; t; n; n; n; t; t; t |];
      [| t; t; t; n; n; n; n; n; n; n; n; n; t; t; t |];
      [| t; t; t; n; t; t; t; t; t; t; n; n; t; t; t |];
      [| t; t; n; t; n; t; t; t; n; t; t; n; t; t; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; n; t; t; n; n; t; t; t; t; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; n; t; n; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; n; t; n; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; t; n; t; t; t; n; t; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; n; t; n; n; n; t; n; t; t; t; t |];
      [| t; t; t; t; n; t; n; t; n; t; n; t; t; t; t |];
      [| t; t; t; t; t; n; t; t; t; n; t; t; t; t; t |];
    |]

let eat_f2 =
  scale 10
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; n; n; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; n; n; n; n; t; t; t; t; t; t |];
      [| t; t; t; n; n; t; t; n; n; n; n; n; n; t; t |];
      [| t; t; n; t; t; t; t; t; t; n; n; n; n; n; t |];
      [| t; n; t; t; n; n; n; t; t; t; t; n; n; n; t |];
      [| t; n; t; t; n; n; n; t; t; t; n; t; n; t; t |];
      [| t; n; t; t; n; n; n; t; t; t; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; n; t; n; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; t; n; t; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; n; t; n; t; n; n; n; n; t; t; t; t |];
      [| t; t; t; n; t; n; t; n; t; t; t; t; t; t; t |];
      [| t; t; t; t; n; t; n; t; t; t; t; t; t; t; t |];
    |]

let eat_f3 =
  scale 10
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; n; n; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; n; n; n; n; t; t; t; t; t; t |];
      [| t; t; t; n; n; t; t; n; n; n; n; n; n; t; t |];
      [| t; t; n; t; t; t; t; t; t; n; n; n; n; n; t |];
      [| t; n; t; t; n; n; n; t; t; t; t; n; n; n; t |];
      [| t; n; t; t; n; n; n; t; t; t; n; t; n; t; t |];
      [| t; n; t; t; n; n; n; t; t; t; t; t; n; t; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| n; n; n; t; t; t; t; t; t; t; t; t; n; n; t |];
      [| n; t; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; n; t; t; t; t; t; t; t; t; n; n; t; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; n; n; t; n; n; t; n; n; t; t; t; t |];
      [| t; t; t; t; t; n; t; t; n; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
    |]

let typing =
  scale 10
    [|
      [| t; t; t; n; n; t; t; t; t; t; n; n; t; t; t |];
      [| t; t; n; n; n; n; t; t; t; n; n; n; n; t; t |];
      [| t; t; n; n; n; n; n; n; n; n; n; n; n; t; t |];
      [| t; t; n; n; t; t; t; t; t; t; t; n; n; t; t |];
      [| t; t; n; t; n; n; t; t; t; n; n; t; n; t; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; t; n; t; t; t; t; t; n; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; n; n; n; n; n; n; n; n; n; n; n; n; n; t |];
      [| n; t; t; n; t; t; t; t; t; t; t; n; t; t; n |];
      [| n; t; t; n; n; n; n; n; n; n; n; n; t; t; n |];
      [| n; t; n; n; t; n; t; n; t; n; t; n; n; t; n |];
      [| n; t; n; t; n; t; n; t; n; t; n; t; n; t; n |];
      [| n; t; t; n; n; n; n; n; n; n; n; n; t; t; n |];
      [| n; t; t; t; t; t; t; t; t; t; t; t; t; t; n |];
      [| t; n; n; n; n; n; n; n; n; n; n; n; n; n; t |];
    |]

let poop =
  scale 5
    [|
      [| t; t; t; n; t; t; t; n; t; t; t; t; t; t; t; t |];
      [| t; t; n; t; t; t; t; t; n; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; t; n; t; t; t; t; t; n; t; t |];
      [| t; t; n; n; t; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; n; t; t; t; n; t; t; t; t; n; n; t |];
      [| t; t; t; t; n; t; t; n; n; n; t; t; t; t; t; n |];
      [| t; t; t; n; t; t; n; t; t; t; n; n; t; t; t; n |];
      [| t; t; t; t; t; n; t; t; t; t; t; n; t; t; n; t |];
      [| t; t; t; t; n; n; n; n; n; n; n; n; n; t; t; t |];
      [| t; t; t; n; n; t; t; t; t; t; t; t; n; n; t; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; t; t; n; t; t |];
      [| t; t; t; n; n; n; n; n; t; t; t; n; n; n; t; t |];
      [| t; t; n; n; t; t; t; t; n; n; n; t; t; n; n; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; t; t; n |];
      [| t; n; n; t; t; t; t; t; t; t; t; t; t; t; t; n |];
      [| t; t; t; n; n; n; n; n; n; n; n; n; n; n; n; n |];
    |]

let poop_shovel =
  scale 5
    [|
      [| n; n; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| n; n; n; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; n; n; n; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; n; n; n; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; n; n; n; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; n; n; n; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; n; n; n; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; n; n; n; t; t; n; t; t; t; t |];
      [| t; t; t; t; t; t; t; n; n; n; n; t; n; t; t; t |];
      [| t; t; t; t; t; t; t; t; n; t; t; t; t; n; t; t |];
      [| t; t; t; t; t; t; t; t; n; t; t; t; t; t; n; t |];
      [| t; t; t; t; t; t; t; n; t; t; t; t; t; t; t; n |];
      [| t; t; t; t; t; t; t; t; n; t; t; t; t; t; t; n |];
      [| t; t; t; t; t; t; t; t; t; n; t; t; t; t; t; n |];
      [| t; t; t; t; t; t; t; t; t; t; n; t; t; t; n; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; n; n; n; t; t |];
    |]

let eat_icon =
  scale 5
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; n; t; n; t; n; t; t; t; t; t; n; t; t; t |];
      [| t; t; n; t; n; t; n; t; t; t; t; n; n; t; t; t |];
      [| t; t; n; t; n; t; n; t; t; t; t; n; n; t; t; t |];
      [| t; t; n; t; n; t; n; t; t; t; n; n; n; t; t; t |];
      [| t; t; n; n; n; n; n; t; t; t; n; n; n; t; t; t |];
      [| t; t; t; n; n; n; t; t; t; t; n; n; n; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; n; n; n; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
    |]

let sleep_icon =
  scale 5
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; n; n; n; n; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; n; t; t; t; t; t; t; t; t; n; t; n; t; t |];
      [| t; n; n; n; n; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; n; n; n; n; n; n; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; t; n; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; n; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; n; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; n; n; n; n; n; t |];
      [| t; t; t; n; n; n; n; n; n; t; t; t; t; n; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; n; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; n; t; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; n; n; n; n; n; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
    |]

let toilet_icon =
  scale 5
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; n; n; n; n; n; n; n; n; n; n; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; n; t; t; n; t |];
      [| t; t; t; n; t; t; n; n; n; n; t; t; n; t; n; t |];
      [| t; t; t; n; n; t; t; t; t; t; t; n; n; t; n; t |];
      [| t; t; t; n; t; n; n; n; n; n; n; t; n; t; n; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; t; n; t; n; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; t; n; t; n; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; t; n; t; n; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; t; n; t; n; t |];
      [| t; t; t; n; t; t; t; t; t; t; t; t; n; n; n; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; n; t; t; t; t |];
      [| t; t; t; t; t; n; n; n; n; n; n; t; t; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
    |]

let play_icon =
  scale 5
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; n; n; n; n; n; t; t; t; t; t; t; t |];
      [| t; t; t; n; t; t; t; t; t; n; t; t; n; n; n; t |];
      [| t; t; n; t; t; t; t; t; t; t; n; t; t; t; t; t |];
      [| t; n; t; n; t; t; t; t; t; n; t; n; t; n; n; t |];
      [| t; n; t; t; n; t; t; t; n; t; t; n; t; t; t; t |];
      [| t; n; t; t; n; t; t; t; n; t; t; n; t; n; n; t |];
      [| t; n; t; t; n; t; t; t; n; t; t; n; t; t; t; t |];
      [| t; n; t; n; t; t; t; t; t; n; t; n; t; t; t; t |];
      [| t; t; n; t; t; t; t; t; t; t; n; t; t; t; t; t |];
      [| t; t; t; n; t; t; t; t; t; n; t; t; t; t; t; t |];
      [| t; t; t; t; n; n; n; n; n; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
    |]

let shop_icon =
  scale 5
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; n; n; n; n; t; t; t; t; t; t |];
      [| t; t; t; t; t; n; t; t; t; t; n; t; t; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; n; t; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; n; t; t; t; t |];
      [| t; t; t; t; n; t; t; t; t; t; t; n; t; t; t; t |];
      [| t; n; n; n; n; n; n; n; n; n; n; n; n; n; n; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; t; t; t; t; t; t; t; t; t; t; t; t; n; t |];
      [| t; n; n; n; n; n; n; n; n; n; n; n; n; n; n; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
    |]

let inventory_icon =
  scale 5
    [|
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; n; n; n; t; t; n; n; n; t; t; t; t; t |];
      [| t; t; n; t; t; t; n; n; t; t; n; n; t; t; t; t |];
      [| t; t; t; n; n; n; n; n; n; n; t; t; n; t; t; t |];
      [| t; t; t; n; n; t; t; n; t; t; t; n; n; t; t; t |];
      [| t; t; n; t; t; n; n; t; n; t; n; t; n; t; t; t |];
      [| t; t; n; n; n; t; t; t; n; n; t; t; n; t; t; t |];
      [| t; t; n; t; n; n; n; n; t; n; t; n; n; t; t; t |];
      [| t; t; n; t; t; n; t; t; t; n; n; t; n; t; t; t |];
      [| t; t; n; n; t; n; t; t; n; n; t; t; n; t; t; t |];
      [| t; t; n; t; n; n; n; n; t; n; t; n; t; t; t; t |];
      [| t; t; n; t; t; n; t; t; t; n; n; t; t; t; t; t |];
      [| t; t; t; n; t; n; t; t; n; n; t; t; t; t; t; t |];
      [| t; t; t; t; n; n; n; n; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
      [| t; t; t; t; t; t; t; t; t; t; t; t; t; t; t; t |];
    |]

let black_sq = scale 5 [| [| n; n; n |]; [| n; n; n |]; [| n; n; n |] |]

let white_sq = scale 5 [| [| n; n; n |]; [| n; t; n |]; [| n; n; n |] |]

let black_triangle =
  scale 5
    [|
      [| t; t; n; t; t |]; [| t; n; n; n; t |]; [| t; n; n; n; t |];
      [| n; n; n; n; n |];
    |]

let gg =
  scale 5
    [|
      [| t; t; r; r; r; r; r; t; t; t; t; t; r; r; r; r; r; t; t; t |];
      [| t; r; r; r; t; t; t; t; t; t; t; r; r; r; t; t; t; t; t; t |];
      [| r; r; t; t; t; t; t; t; t; t; r; r; t; t; t; t; t; t; t; t |];
      [| r; t; t; t; t; t; t; t; t; t; r; t; t; t; t; t; t; t; t; t |];
      [| r; t; t; t; t; t; t; t; t; t; r; t; t; t; t; t; t; t; t; t |];
      [| r; t; t; r; r; r; r; t; t; t; r; t; t; r; r; r; r; t; t; t |];
      [| r; t; t; t; t; t; r; t; t; t; r; t; t; t; t; t; r; t; t; t |];
      [| t; r; t; t; t; t; r; t; t; t; t; r; t; t; t; t; r; t; t; t |];
      [| t; r; r; t; t; t; r; t; t; t; t; r; r; t; t; t; r; t; t; t |];
      [| t; t; r; r; r; r; r; t; t; t; t; t; r; r; r; r; r; t; t; t |];
    |]

(** Temporary animation frame for MS1 *)
let test_anim =
  {
    frames = [ neutral_f1; neutral_f2 ];
    total = 2;
    current = 0;
    cx = 30;
    cy = 60;
  }

let eat_anim =
  {
    frames = [ eat_f1; eat_f2; eat_f3 ];
    total = 3;
    current = 0;
    cx = 90;
    cy = 60;
  }

(* let don_anim =
  {
    frames = [ don_1; don_2 ];
    total = 2;
    current = 0;
    cx = 0;
    cy = 0;
  } *)

let don_anim = 
  {
    frames = [ black_sq ];
    total = 1;
    current = 0;
    cx = 0;
    cy = 0;
  }

let rock_static =
  {
    (* Temporary art for rocks *)
    frames = [ black_sq ];
    total = 1;
    current = 0;
    cx = 0;
    cy = 0;
  }
  
(* let ka_anim =
  {
    frames = [ ka_1; ka_2 ];
    total = 2;
    current = 0;
    cx = 0;
    cy = 0;
  } *)

let ka_anim =
  {
    frames = [ white_sq ];
    total = 1;
    current = 0;
    cx = 0;
    cy = 0;
  }

let dolphin_static =
  {
    (* Temporary art for dolphin *)
    frames = [ black_triangle ];
    total = 1;
    current = 0;
    cx = 0;
    cy = 0;
  }

let gg_static =
  {
    (* Temporary art for GG animation *)
    frames = [ gg ];
    total = 1;
    current = 0;
    cx = 0;
    cy = 0;
  }
