open Graphics

type animation = {
  frames : Graphics.image list;
  total : int;
}

(** Get current image in the animation frame *)
let curr_frame (anim : animation) (frame : int) : Graphics.image =
  List.nth anim.frames frame

(** Returns an array with [item] repeated [num] times *)
let repeat (num : int) (item : 'a) = Array.make num item

(** Vertically scales the inputted image up by [num]*)
let vert_augment (num : int) (img : image) : image =
  make_image
    (Array.fold_left Array.append [||]
       (Array.map (repeat num) (img |> dump_image)))

(** Horizontally scales the inputted image up by [num]*)
let hor_augment (num : int) (img : image) : image =
  make_image
    (Array.map
       (fun row ->
         Array.fold_left Array.append [||] (Array.map (repeat num) row))
       (img |> dump_image))

(** Evenly scales the image up by [num]*)
let scale_augment (num : int) (img : image) : image =
  img |> vert_augment num |> hor_augment num

let mirror (img : image) : image =
  let array_rev array =
    Array.to_list array |> List.rev |> Array.of_list
  in
  Array.map array_rev (dump_image img) |> make_image

let n = 0x000000

and t = 0xFFFFFF

let neutral_f1 () =
  scale_augment 10
    (make_image
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
       |])

let neutral_f2 () =
  scale_augment 10
    (make_image
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
       |])

let wide_f1 () =
  scale_augment 10
    (make_image
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
       |])

let wide_f2 () =
  scale_augment 10
    (make_image
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
       |])

let idle () =
  scale_augment 10
    (make_image
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
       |])

let eat_f1 () =
  scale_augment 10
    (make_image
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
       |])

let eat_f2 () =
  scale_augment 10
    (make_image
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
       |])

let eat_f3 () =
  scale_augment 10
    (make_image
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
       |])

let typing () =
  scale_augment 10
    (make_image
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
       |])

let poop () =
  scale_augment 5
    (make_image
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
       |])

let shovel () =
  scale_augment 5
    (make_image
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
       |])

let eat_icon () =
  scale_augment 5
    (make_image
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
       |])

let sleep_icon () =
  scale_augment 5
    (make_image
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
       |])

let toilet_icon () =
  scale_augment 5
    (make_image
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
       |])

let play_icon () =
  scale_augment 5
    (make_image
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
       |])

let shop_icon () =
  scale_augment 5
    (make_image
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
       |])

let inventory_icon () =
  scale_augment 5
    (make_image
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
       |])

(** Temporary animation frame for MS1 *)
let test_anim () =
  { frames = [ neutral_f1 (); neutral_f2 () ]; total = 2 }

let eat_anim () =
  { frames = [ eat_f1 (); eat_f2 (); eat_f3 () ]; total = 3 }
