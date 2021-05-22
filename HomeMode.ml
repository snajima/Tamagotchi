open Animation
open Gui
open State

let delta = 30

let default_anim_length = 300

let vs : viewstate = { default_vs with animations = [] }

type button =
  | Eat
  | Sleep
  | Toilet
  | Play
  | Shop
  | Inventory

type avatar_anim =
  | Eating
  | Sleeping
  | Cleaning
  | Idle

type homestate = {
  (* 0 = Eat, 1 = Sleep, 2 = Toilet, 3 = Play, 4 = Shop, 5 = Inventory *)
  mutable tam_state : tamagotchi;
  total_icons : int;
  mutable active_icon : int;
  mutable active_anim : avatar_anim;
  mutable anim_counter : int;
}

type game_flags = {
  mutable dolphin : bool;
  mutable drum : bool;
  mutable elements : bool;
}

let my_home =
  {
    tam_state = init_tam "./json/baby.json";
    total_icons = 6;
    active_icon = 0;
    anim_counter = 0;
    active_anim = Idle;
  }

let _ = ignore (State.set_cleanliness 10 my_home.tam_state)

(** [button_of_int] returns the corresponding button for the int *
    representation of [active_icon_num]

    Precondition: [active_icon_num] must be an integer between 0 and 5
    inclusive *)
let button_of_int (active_icon_num : int) =
  match active_icon_num with
  | 0 -> Eat
  | 1 -> Sleep
  | 2 -> Toilet
  | 3 -> Play
  | 4 -> Shop
  | 5 -> Inventory
  | _ -> failwith "Impossible: Precondition violation"

let activate_button (active_button : button) =
  match active_button with
  | Eat ->
      my_home.active_anim <- Eating;
      my_home.anim_counter <- default_anim_length
      (* TODO: Update tamagotchi state -- make less hungry*)
  | Sleep ->
      my_home.active_anim <- Sleeping;
      my_home.anim_counter <- default_anim_length
      (* TODO: Update tamagotchi state -- make less sleepy*)
  | Toilet ->
      my_home.active_anim <- Cleaning;
      my_home.anim_counter <- default_anim_length
      (* TODO: Update tamagotchi state -- make more clean*)
  | Play -> Dolphinview.draw ()
  | Shop -> Drumview.draw ()
  | Inventory -> Elementalsview.draw ()

let my_game_flags = { dolphin = false; drum = false; elements = false }

let reset_game_flags () =
  my_game_flags.dolphin <- false;
  my_game_flags.drum <- false;
  my_game_flags.elements <- false

let get_avatar_animations (hs : homestate) : Animation.animation =
  match hs.active_anim with
  | Eating -> eat_anim
  | Sleeping -> sleep_anim
  | Cleaning -> clean_anim
  | Idle -> avatar

let reset_avatar_animations (hs : homestate) =
  hs.active_anim <- Idle;
  hs.anim_counter <- 0

let get_toolbar_animations (hs : homestate) : Animation.animation list =
  match button_of_int hs.active_icon with
  | Eat ->
      [
        eat_icon_bobble; sleep_icon_static; toilet_icon_static;
        play_icon_static; shop_icon_static; inventory_icon_static;
      ]
  | Sleep ->
      [
        eat_icon_static; sleep_icon_bobble; toilet_icon_static;
        play_icon_static; shop_icon_static; inventory_icon_static;
      ]
  | Toilet ->
      [
        eat_icon_static; sleep_icon_static; toilet_icon_bobble;
        play_icon_static; shop_icon_static; inventory_icon_static;
      ]
  | Play ->
      [
        eat_icon_static; sleep_icon_static; toilet_icon_static;
        play_icon_bobble; shop_icon_static; inventory_icon_static;
      ]
  | Shop ->
      [
        eat_icon_static; sleep_icon_static; toilet_icon_static;
        play_icon_static; shop_icon_bobble; inventory_icon_static;
      ]
  | Inventory ->
      [
        eat_icon_static; sleep_icon_static; toilet_icon_static;
        play_icon_static; shop_icon_static; inventory_icon_bobble;
      ]

let get_status_animations (hs : homestate) : unit =
  let sleep = hs.tam_state |> get_sleep
  and cleanliness = hs.tam_state |> get_cleanliness
  and hunger = hs.tam_state |> get_hunger
  and age = hs.tam_state |> get_age in
  (* Status Name *)
  draw_message 50 (80 * 4) 25 Graphics.black "Sleep:";
  draw_message 50 (70 * 4) 25 Graphics.black "Clean:";
  draw_message 50 (60 * 4) 25 Graphics.black "Hunger:";
  draw_message 50 (50 * 4) 25 Graphics.black "Age:";
  (* Status Value *)
  draw_message 120 (80 * 4) 30 Graphics.black (string_of_int sleep);
  draw_message 120 (70 * 4) 30 Graphics.black
    (string_of_int cleanliness);
  draw_message 120 (60 * 4) 30 Graphics.black (string_of_int hunger);
  draw_message 120 (50 * 4) 30 Graphics.black (string_of_int age)

let get_animations (hs : homestate) : Animation.animation list =
  let tool_bar_anims = get_toolbar_animations hs
  and avatar_anim = get_avatar_animations hs in
  avatar_anim :: tool_bar_anims

(** Draw the tool bars, setup screen*)
let setup_toolbars s =
  (* Draw top and bottom black bars *)
  draw_pixels_ll 0 0 120 10 Graphics.black;
  draw_pixels_ll 0 110 120 10 Graphics.black;
  s.animations <- get_animations my_home

(** Main init function for HomeMode*)
let init s =
  (* Window has width s.scale * maxx and height s.scale * s.maxy *)
  Graphics.open_graph
    (" "
    ^ string_of_int (s.scale * s.maxx)
    ^ "x"
    ^ string_of_int (s.scale * s.maxy));
  Graphics.set_color s.bc;
  Graphics.fill_rect 0 0 (s.scale * s.maxx) (s.scale * s.maxy);
  setup_toolbars s

(** Main exit function for HomeMode *)
let exit s =
  Graphics.close_graph ();
  (* TODO: save the current state of Tamagotchi to json *)
  print_endline "";
  print_endline
    "Thanks for playing! Your Tamagotchi will be waiting for your \
     return";
  print_endline ""

(** Main exception function for HomeMode *)
let except s ex = ()

(** [key] processes the [c] character pressed and updates the state [s]
    accordingly *)
let key s c =
  (* draw_pixel s.x s.y s.scale s.fc; *)
  (match c with
  | '1' -> Dolphinview.draw ()
  | '2' -> Drumview.draw ()
  | '3' -> Elementalsview.draw ()
  | 'a' ->
      my_home.active_icon <-
        (my_home.active_icon - 1 + my_home.total_icons)
        mod my_home.total_icons
  | 'd' ->
      my_home.active_icon <-
        (my_home.active_icon + 1 + my_home.total_icons)
        mod my_home.total_icons;
      print_endline (string_of_int my_home.active_icon)
  | 's' -> my_home.active_icon |> button_of_int |> activate_button
  | 'x' -> raise End
  | _ -> ());
  s.animations <- get_animations my_home;
  print_endline (Char.escaped c)

let clear_center (state : viewstate) : unit =
  Graphics.set_color state.bc;
  Graphics.fill_rect 0 (10 * state.scale)
    (state.scale * state.maxx)
    (state.scale * (state.maxy - 20))

(** [step] increments the tick in the state, used for conservatively
    animating *)
let step (state : viewstate) : unit =
  (* Handle Avatar Animation (except idle) Transitions*)
  if my_home.active_anim != Idle then
    if my_home.anim_counter = 0 then (
      reset_avatar_animations my_home;
      state.animations <- get_animations my_home)
    else my_home.anim_counter <- my_home.anim_counter - 1;
  (* Update animations every [delta] frames *)
  if state.tick mod delta = 0 then
    state.animations <- increment_anims state.animations;
  if true then (
    try ignore (State.step my_home.tam_state)
    with Death ->
      Graphics.clear_graph ();
      Graphics.synchronize ();
      gameover_screen_no_score 500 "Oh no :("
        { tam_death with cx = vs.maxx / 2; cy = vs.maxy / 2 }
        vs;
      raise End);
  state.tick <- (state.tick + 1) mod delta

let predraw (state : viewstate) : unit =
  draw_pixels_ll 0 0 120 10 Graphics.black;
  draw_pixels_ll 0 110 120 10 Graphics.black;
  clear_center state;
  get_status_animations my_home

let draw () = draw_loop vs init exit key except step predraw

(* For debugging. Uncomment the following line and run [make homemode] *)
(* TODO: Load from json (if it exists), otherwise launch new Tamagotchi
   session *)
(* let _ = draw () *)
