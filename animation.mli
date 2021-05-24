(** Provides abstract methods to load and render custom animations with
    variable refresh rates *)

(** The type [pixel_array] represents.. *)
type pixel_array = int array array

(** The type [animation] represents .. *)
type animation = {
  (* Drawing information *)
  frames : pixel_array list;
  current : int;
  total : int;
  (* Position information *)
  cx : int;
  cy : int;
}

(** [curr_frame anim] gets the current image (represented as a pixel
    array) in [anim] *)
val curr_frame : animation -> pixel_array

(** [next_frame anim] returns an animation that represents the next
    animation frame of [anim] *)
val next_frame : animation -> animation

(* Home Screen *)

val poop : pixel_array

val sun : pixel_array

val moon : pixel_array

val eat_icon_static : animation

val sleep_icon_static : animation

val toilet_icon_static : animation

val play_icon_static : animation

val shop_icon_static : animation

val inventory_icon_static : animation

val eat_icon_bobble : animation

val sleep_icon_bobble : animation

val toilet_icon_bobble : animation

val play_icon_bobble : animation

val shop_icon_bobble : animation

val inventory_icon_bobble : animation

(* Drum *)

val drum_anim : animation

val idle_drummer_anim : animation

val right_drum_anim : animation

val left_drum_anim : animation

val don_anim : animation

val ka_anim : animation

val gg_static : animation

(* Dolphin *)

val rock_static : animation

val dolphin_static : animation

(* Elementalist *)

val fireball_anim : animation

val leaf_anim : animation

val water_anim : animation

val shoot_anim : animation

val robot_anim : animation

val cloud_anim : animation

(* Other *)

val tam_death : animation

val eat_anim_baby : animation

val eat_anim_adult : animation

val eat_anim_elder : animation

val sleep_anim_baby : animation

val sleep_anim_adult : animation

val sleep_anim_elder : animation

val clean_anim_baby : animation

val clean_anim_adult : animation

val clean_anim_elder : animation

val avatar_baby : animation

val avatar_adult : animation

val avatar_elder : animation
