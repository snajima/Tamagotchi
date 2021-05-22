open Graphics
open Yojson.Basic.Util

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

(** [pixel_array_from_bit_array bit_array] returns a [pixel_array] with
    all 1's replaced with n and 0's replaced with t *)
let pixel_array_from_bit_array (bit_array : int list list) : pixel_array
    =
  bit_array |> Array.of_list
  |> Array.map (fun lst ->
         lst |> Array.of_list
         |> Array.map (fun bin -> if bin = 1 then n else t))

(** [bit_array_from_json] extracts a bit_array that represents the
    frames of the animations from the json *)
let bit_array_from_json (animation_name : string) json : int list list =
  json |> member animation_name |> member "data" |> to_list
  |> List.map (fun data_row ->
         data_row |> to_list |> List.map (fun i -> to_int i))

(** [pixel_array_from_json] extracts the scaled pixel_array for the
    [animation_name] from the [json], using [bit_array_from_json] and
    [pixel_array_from_bit_array] as helper functions*)
let pixel_array_from_json (animation_name : string) json : pixel_array =
  let scale_num =
    json |> member animation_name |> member "scale" |> to_int
  in
  bit_array_from_json animation_name json
  |> pixel_array_from_bit_array |> scale scale_num

(* ---------------------------------------------------------------- *)
(* ---------------------- Home Screen Frames ---------------------- *)
(* ---------------------------------------------------------------- *)
let homescreen_anim_json = Yojson.Basic.from_file "./json/homemode.json"

let eat_icon = pixel_array_from_json "eat_icon" homescreen_anim_json

let sleep_icon = pixel_array_from_json "sleep_icon" homescreen_anim_json

let toilet_icon =
  pixel_array_from_json "toilet_icon" homescreen_anim_json

let play_icon = pixel_array_from_json "play_icon" homescreen_anim_json

let shop_icon = pixel_array_from_json "shop_icon" homescreen_anim_json

let inventory_icon =
  pixel_array_from_json "inventory_icon" homescreen_anim_json

let eat_icon_f1 =
  pixel_array_from_json "eat_icon_f1" homescreen_anim_json

let sleep_icon_f1 =
  pixel_array_from_json "sleep_icon_f1" homescreen_anim_json

let toilet_icon_f1 =
  pixel_array_from_json "toilet_icon_f1" homescreen_anim_json

let play_icon_f1 =
  pixel_array_from_json "play_icon_f1" homescreen_anim_json

let shop_icon_f1 =
  pixel_array_from_json "shop_icon_f1" homescreen_anim_json

let inventory_icon_f1 =
  pixel_array_from_json "inventory_icon_f1" homescreen_anim_json

let eat_icon_f2 =
  pixel_array_from_json "eat_icon_f2" homescreen_anim_json

let sleep_icon_f2 =
  pixel_array_from_json "sleep_icon_f2" homescreen_anim_json

let toilet_icon_f2 =
  pixel_array_from_json "toilet_icon_f2" homescreen_anim_json

let play_icon_f2 =
  pixel_array_from_json "play_icon_f2" homescreen_anim_json

let shop_icon_f2 =
  pixel_array_from_json "shop_icon_f2" homescreen_anim_json

let inventory_icon_f2 =
  pixel_array_from_json "inventory_icon_f2" homescreen_anim_json

let poop = pixel_array_from_json "poop" homescreen_anim_json

let poop_shovel =
  pixel_array_from_json "poop_shovel" homescreen_anim_json

let z_icon = pixel_array_from_json "z_icon" homescreen_anim_json

(* ---------------------------------------------------------------- *)
(* -------------------- Home Screen Animations -------------------- *)
(* ---------------------------------------------------------------- *)

let top_row_cy = 100

let bot_row_cy = 20

let eat_icon_static =
  {
    frames = [ eat_icon ];
    total = 1;
    current = 0;
    cx = 15;
    cy = top_row_cy;
  }

let sleep_icon_static =
  {
    frames = [ sleep_icon ];
    total = 1;
    current = 0;
    cx = 60;
    cy = top_row_cy;
  }

let toilet_icon_static =
  {
    frames = [ toilet_icon ];
    total = 1;
    current = 0;
    cx = 105;
    cy = top_row_cy;
  }

let play_icon_static =
  {
    frames = [ play_icon ];
    total = 1;
    current = 0;
    cx = 15;
    cy = bot_row_cy;
  }

let shop_icon_static =
  {
    frames = [ shop_icon ];
    total = 1;
    current = 0;
    cx = 60;
    cy = bot_row_cy;
  }

let inventory_icon_static =
  {
    frames = [ inventory_icon ];
    total = 1;
    current = 0;
    cx = 105;
    cy = bot_row_cy;
  }

let eat_icon_bobble =
  {
    eat_icon_static with
    frames = [ eat_icon_f1; eat_icon_f2 ];
    total = 2;
  }

let sleep_icon_bobble =
  {
    sleep_icon_static with
    frames = [ sleep_icon_f1; sleep_icon_f2 ];
    total = 2;
  }

let toilet_icon_bobble =
  {
    toilet_icon_static with
    frames = [ toilet_icon_f1; toilet_icon_f2 ];
    total = 2;
  }

let play_icon_bobble =
  {
    play_icon_static with
    frames = [ play_icon_f1; play_icon_f2 ];
    total = 2;
  }

let shop_icon_bobble =
  {
    shop_icon_static with
    frames = [ shop_icon_f1; shop_icon_f2 ];
    total = 2;
  }

let inventory_icon_bobble =
  {
    inventory_icon_static with
    frames = [ inventory_icon_f1; inventory_icon_f2 ];
    total = 2;
  }

let drum_json = Yojson.Basic.from_file "./json/drum.json"

let dolphin_json = Yojson.Basic.from_file "./json/dolphin.json"

let dolphin = pixel_array_from_json "dolphin" dolphin_json

let rock = pixel_array_from_json "rock" dolphin_json

let black_sq = pixel_array_from_json "black_icon" drum_json

let white_sq = pixel_array_from_json "white_icon" drum_json

let gg = pixel_array_from_json "gg" dolphin_json

(* let don_anim = { frames = [ don_1; don_2 ]; total = 2; current = 0;
   cx = 0; cy = 0; } *)
let tombstone = pixel_array_from_json "grave" homescreen_anim_json

let don_anim =
  { frames = [ black_sq ]; total = 1; current = 0; cx = 0; cy = 0 }

(* let ka_anim = { frames = [ ka_1; ka_2 ]; total = 2; current = 0; cx =
   0; cy = 0; } *)

let ka_anim =
  { frames = [ white_sq ]; total = 1; current = 0; cx = 0; cy = 0 }

let gg_static =
  {
    (* Temporary art for GG animation *)
    frames = [ gg ];
    total = 1;
    current = 0;
    cx = 0;
    cy = 0;
  }

let tam_death =
  {
    (* TODO: Stephen replease the poop with the gravestone plz *)
    frames = [ tombstone ];
    total = 1;
    current = 0;
    cx = 0;
    cy = 0;
  }

and t = 0xFFFFFF

let animation_json = Yojson.Basic.from_file "./json/animation.json"

let baby_animation_json =
  Yojson.Basic.from_file "./json/baby_animation.json"

let elder_animation_json =
  Yojson.Basic.from_file "./json/elder_animation.json"

let neutral_f1_elder =
  pixel_array_from_json "neutral_f1" baby_animation_json

let neutral_f2_elder =
  pixel_array_from_json "neutral_f2" baby_animation_json

let wide_f1_elder = pixel_array_from_json "wide_f1" elder_animation_json

let wide_f2_elder = pixel_array_from_json "wide_f2" elder_animation_json

let idle_elder = pixel_array_from_json "idle" elder_animation_json

let eat_f1_elder = pixel_array_from_json "eat_f1" elder_animation_json

let eat_f2_elder = pixel_array_from_json "eat_f2" elder_animation_json

let eat_f3_elder = pixel_array_from_json "eat_f3" elder_animation_json

let sleeping_elder =
  pixel_array_from_json "sleeping" elder_animation_json

let neutral_f1_baby =
  pixel_array_from_json "neutral_f1" baby_animation_json

let neutral_f2_baby =
  pixel_array_from_json "neutral_f2" baby_animation_json

let wide_f1_baby = pixel_array_from_json "wide_f1" baby_animation_json

let wide_f2_baby = pixel_array_from_json "wide_f2" baby_animation_json

let idle_baby = pixel_array_from_json "idle" baby_animation_json

let eat_f1_baby = pixel_array_from_json "eat_f1" baby_animation_json

let eat_f2_baby = pixel_array_from_json "eat_f2" baby_animation_json

let eat_f3_baby = pixel_array_from_json "eat_f3" baby_animation_json

let sleeping_baby = pixel_array_from_json "sleeping" baby_animation_json

let neutral_f1_adult = pixel_array_from_json "neutral_f1" animation_json

let neutral_f2_adult = pixel_array_from_json "neutral_f2" animation_json

let wide_f1_adult = pixel_array_from_json "wide_f1" animation_json

let wide_f2_adult = pixel_array_from_json "wide_f2" animation_json

let idle_adult = pixel_array_from_json "idle" animation_json

let eat_f1_adult = pixel_array_from_json "eat_f1" animation_json

let eat_f2_adult = pixel_array_from_json "eat_f2" animation_json

let eat_f3_adult = pixel_array_from_json "eat_f3" animation_json

let sleeping_adult = pixel_array_from_json "sleeping" animation_json

let elementals_json = Yojson.Basic.from_file "./json/elementals.json"

let fireball = pixel_array_from_json "fireball" elementals_json

let fireball_anim =
  { frames = [ fireball ]; total = 1; current = 0; cx = 0; cy = 0 }

let leaf = pixel_array_from_json "leaf" elementals_json

let leaf_anim =
  { frames = [ leaf ]; total = 1; current = 0; cx = 0; cy = 0 }

let water = pixel_array_from_json "water" elementals_json

let water_anim =
  { frames = [ water ]; total = 1; current = 0; cx = 0; cy = 0 }

let shoot = pixel_array_from_json "shoot" elementals_json

let shoot_anim =
  {
    frames = [ idle_adult; shoot ];
    total = 2;
    current = 0;
    cx = 0;
    cy = 0;
  }

let robot = pixel_array_from_json "robot" elementals_json

let robot_anim =
  { frames = [ robot ]; total = 1; current = 0; cx = 0; cy = 0 }

(** Temporary animation frame for MS1 *)
let eat_anim_adult =
  {
    frames = [ eat_f1_adult; eat_f2_adult; eat_f3_adult ];
    total = 3;
    current = 0;
    cx = 60;
    cy = 60;
  }

let eat_anim_baby =
  {
    frames = [ eat_f1_baby; eat_f2_baby; eat_f3_baby ];
    total = 3;
    current = 0;
    cx = 60;
    cy = 60;
  }

let eat_anim_elder =
  {
    frames = [ eat_f1_elder; eat_f2_elder; eat_f3_elder ];
    total = 3;
    current = 0;
    cx = 60;
    cy = 60;
  }

let sleep_anim_adult =
  {
    frames = [ sleeping_adult ];
    total = 1;
    current = 0;
    cx = 60;
    cy = 60;
  }

let sleep_anim_baby =
  {
    frames = [ sleeping_baby ];
    total = 1;
    current = 0;
    cx = 60;
    cy = 60;
  }

let sleep_anim_elder =
  {
    frames = [ sleeping_elder ];
    total = 1;
    current = 0;
    cx = 60;
    cy = 60;
  }

let clean_anim =
  {
    frames = [ poop_shovel; poop_shovel; poop_shovel ];
    total = 3;
    current = 0;
    cx = 60;
    cy = 60;
  }

let avatar_adult =
  {
    frames = [ neutral_f1_adult; neutral_f2_adult ];
    total = 2;
    current = 0;
    cx = 60;
    cy = 60;
  }

let avatar_baby =
  {
    frames = [ neutral_f1_baby; neutral_f2_baby ];
    total = 2;
    current = 0;
    cx = 60;
    cy = 60;
  }

let avatar_elder =
  {
    frames = [ neutral_f1_elder; neutral_f2_elder ];
    total = 2;
    current = 0;
    cx = 60;
    cy = 60;
  }

let rock_static =
  {
    (* Temporary art for rocks *)
    frames = [ rock ];
    total = 1;
    current = 0;
    cx = 0;
    cy = 0;
  }

let dolphin_static =
  {
    (* Temporary art for dolphin *)
    frames = [ dolphin ];
    total = 1;
    current = 0;
    cx = 0;
    cy = 0;
  }
