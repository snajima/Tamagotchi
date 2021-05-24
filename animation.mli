(** Provides abstract methods to load and render custom animations with
    variable refresh rates *)

(** The type [pixel_array] represents an black and white image (frame in
    an animation) by listing out the color of each of the pixel *)
type pixel_array = int array array

(** The type [animation] represents a filmstrip that stores the list of
    frames and a pointer to the current frame. It also stores the center
    of where to render the current animation *)
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

(** [poop] is a pixelated representation of a Tamagotchi's excretion in
    the form of a [pixel_array]. It is used to render the cleanliness of
    the Tamagotchi *)
val poop : pixel_array

(** [sun] is a pixelated representation of a Sun and is used to indicate
    the time of day (Morning) *)
val sun : pixel_array

(** [moon] is a pixelated representation of a Moon and is used to
    indicate the time of day (Night) *)
val moon : pixel_array

(** [eat_icon_static] is an [animation] that renders the eat button in
    the toolbar in the homescreen *)
val eat_icon_static : animation

(** [sleep_icon_static] is an [animation] that renders the sleep button
    in the toolbar in the homescreen *)
val sleep_icon_static : animation

(** [toilet_icon_static] is an [animation] that renders the clean button
    in the toolbar in the homescreen *)
val toilet_icon_static : animation

(** [dolphin_icon_static] is an [animation] that renders the dolphin button
    in the toolbar in the homescreen *)
val dolphin_icon_static : animation

(** [drum_icon_static] is an [animation] that renders the drum button
    in the toolbar in the homescreen *)
val drum_icon_static : animation

(** [elementals_icon_static] is an [animation] that renders the elementals 
(robot) button in the toolbar in the homescreen *)
val elementals_icon_static : animation

(** [eat_icon_bobble] is an [animation] that renders the eat button in
    the toolbar in the homescreen when the option is being hovered over *)
val eat_icon_bobble : animation

(** [sleep_icon_bobble] is an [animation] that renders the sleep button
    in the toolbar in the homescreen when the option is being hovered
    over *)
val sleep_icon_bobble : animation

(** [toilet_icon_bobble] is an [animation] that renders the clean button
    in the toolbar in the homescreen when the option is being hovered
    over *)
val toilet_icon_bobble : animation

(** [dolphin_icon_bobble] is an [animation] that renders the dolphin button
    in the toolbar in the homescreen when the option is being hovered
    over *)
val dolphin_icon_bobble : animation

(** [drum_icon_bobble] is an [animation] that renders the drum button
    in the toolbar in the homescreen when the option is being hovered
    over *)
val drum_icon_bobble : animation

(** [toilet_icon_bobble] is an [animation] that renders the elementals (robot)
    button in the toolbar in the homescreen when the option is being hovered
    over *)
val elementals_icon_bobble : animation

(** [drum_anim] is an [animation] that renders a Tamagotchi drumming
    used to indicate progress in the Drum game *)
val drum_anim : animation

(** [idle_drummer_anim] is an [animation] that renders a Tamagotchi with
    drumsticks in hand and is used to represent the state where the
    player has not pressed any key input in the Drum game *)
val idle_drummer_anim : animation

(** [right_drum_anim] is an [animation] that renders a Tamagotchi with
    the right drumstick stricking the drum *)
val right_drum_anim : animation

(** [right_drum_anim] is an [animation] that renders a Tamagotchi with
    the left drumstick stricking the drum *)
val left_drum_anim : animation

(** [idle_drummer_anim_baby] is an [animation] that renders a baby
    Tamagotchi with drumsticks in hand and is used to represent the
    state where the player has not pressed any key input in the Drum
    game *)
val idle_drummer_anim_baby : animation

(** [right_drum_anim_baby] is an [animation] that renders a baby
    Tamagotchi with the right drumstick stricking the drum *)
val right_drum_anim_baby : animation

(** [right_drum_anim_baby] is an [animation] that renders a baby
    Tamagotchi with the left drumstick stricking the drum *)
val left_drum_anim_baby : animation

(** [idle_drummer_anim_elder] is an [animation] that renders an elder
    Tamagotchi with drumsticks in hand and is used to represent the
    state where the player has not pressed any key input in the Drum
    game *)
val idle_drummer_anim_elder : animation

(** [right_drum_anim_elder] is an [animation] that renders an elder
    Tamagotchi with the right drumstick stricking the drum *)
val right_drum_anim_elder : animation

(** [right_drum_anim_elder] is an [animation] that renders an elder
    Tamagotchi with the left drumstick stricking the drum *)
val left_drum_anim_elder : animation

(** [don_anim] is an [animation] that renders a the Don drum beat in the
    Drum game *)
val don_anim : animation

(** [don_anim] is an [animation] that renders a the Ka drum beat in the
    Drum game *)
val ka_anim : animation

(** [gg_static] is an [animation] that renders custom gameover animation *)
val gg_static : animation

(** [rock_static] is an [animation] that renders the rock obstacles in
    the Dolphin game *)
val rock_static : animation

(** [dolphin_static] is an [animation] that renders the dolphin (the
    player) in the Dolphin game *)
val dolphin_static : animation

(** [fireball_anim] is an [animation] that renders the fire option in
    the Elementalist game *)
val fireball_anim : animation

(** [leaf_anim] is an [animation] that the renders the leaf option in
    the Elementalist game *)
val leaf_anim : animation

(** [water_anim] is an [animation] that the renders the water option in
    the Elementalist game *)
val water_anim : animation

(** [shoot_anim] is an [animation] that the renders the player's
    shooting motion in the Elementalist game *)
val shoot_anim : animation

(** [robot_anim] is an [animation] that the renders the enemy's shooting
    motion in the Elementalist game *)
val robot_anim : animation

(** [cloud_anim] is an [animation] that the renders the collision effect
    of the player and the enemy's element selection in the Elementalist
    game *)
val cloud_anim : animation

(** [tam_death] is an [animation] that the renders the Tamagotchi in the
    unfortunate event of passing away *)
val tam_death : animation

(** [eat_anim_baby] is an [animation] that the renders a baby Tamagotchi
    eating *)
val eat_anim_baby : animation

(** [eat_anim_adult] is an [animation] that the renders an adult
    Tamagotchi eating *)
val eat_anim_adult : animation

(** [eat_anim_elder] is an [animation] that the renders an elder
    Tamagotchi eating *)
val eat_anim_elder : animation

(** [sleep_anim_baby] is an [animation] that the renders a baby
    Tamagotchi sleeping *)
val sleep_anim_baby : animation

(** [sleep_anim_adult] is an [animation] that the renders an adult
    Tamagotchi sleeping *)
val sleep_anim_adult : animation

(** [sleep_anim_elder] is an [animation] that the renders an elder
    Tamagotchi sleeping *)
val sleep_anim_elder : animation

(** [clean_anim_baby] is an [animation] that the renders a baby
    Tamagotchi cleaning *)
val clean_anim_baby : animation

(** [clean_anim_adult] is an [animation] that the renders an adult
    Tamagotchi cleaning *)
val clean_anim_adult : animation

(** [clean_anim_elder] is an [animation] that the renders an elder
    Tamagotchi cleaning *)
val clean_anim_elder : animation

(** [avatar_baby] is an [animation] that the renders a baby Tamagotchi
    in the idle state *)
val avatar_baby : animation

(** [avatar_adult] is an [animation] that the renders an adult
    Tamagotchi in the idle state *)
val avatar_adult : animation

(** [avatar_elder] is an [animation] that the renders an elder
    Tamagotchi in the idle state *)
val avatar_elder : animation
