(** The type [viewstate] represents state of the visual elements in the
    GUI which stores:

    - tick : number of frames that passed
    - animations: a list of [animations] currently active
    - maxx : width of the screen in game coordinates
    - maxy : height of the screen in game coordinates
    - scale : conversion factor from game coordinates to pixels
    - bc : background color
    - fc : foreground color *)
type viewstate = {
  mutable tick : int;
  mutable animations : Animation.animation list;
  maxx : int;
  maxy : int;
  scale : int;
  bc : Graphics.color;
  fc : Graphics.color;
}

(** Raised when the user presses the exit command to exit the draw loop *)
exception End

(** [default_vs] stores the default initial values for [tick], [maxx],
    [maxy], [scale], [bc] and [fc] *)
val default_vs : viewstate

(** [draw_pixels] draws a rectangle of pixels centered at [cx] and [cy]
    with width [sx] and height [sy] and color [c] *)
val draw_pixels : int -> int -> int -> int -> Graphics.color -> unit

(** [draw_pixels] draws a rectangle of pixels with lower left corner at
    [x] and [y] with width [sx] and height [sy] and color [c] *)
val draw_pixels_ll : int -> int -> int -> int -> Graphics.color -> unit

(** [draw_img] converts [pixel_array] to an image and draws it with
    center at [x] and [y] in screen coordinates *)
val draw_img : int -> int -> Animation.pixel_array -> unit

(** [draw_img] converts [pixel_array] to an image and draws it with
    lower left corner at [x] and [y] in screen coordinates *)
val draw_img_ll : int -> int -> Animation.pixel_array -> unit

(** [draw_message] draws the string [message] with center at [cx] and
    [cy] with a scale of [size] and with a tint of [color] *)
val draw_message : int -> int -> int -> Graphics.color -> string -> unit

(** [process_anims] process a list of animations and render the current
    frame on the graphics context *)
val process_anims : Animation.animation list -> unit

(** [increment_anims] returns a list of animations with all animations
    in [anims] incremented to the next frame *)
val increment_anims :
  Animation.animation list -> Animation.animation list

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
val draw_loop :
  viewstate ->
  (viewstate -> unit) ->
  (viewstate -> unit) ->
  (viewstate -> char -> unit) ->
  (viewstate -> exn -> unit) ->
  (viewstate -> unit) ->
  (viewstate -> unit) ->
  unit

(** [gameover_screen] renders a custom gameover_screen over the
    currently active [viewstate] that persists for [length] frames. It
    renders [score] in the center and then writes a [message] underneath
    the score. Finally, it renders [animation] *)
val gameover_screen :
  int -> int -> string -> Animation.animation -> viewstate -> unit

val gameover_screen_no_score :
  int -> string -> Animation.animation -> viewstate -> unit
