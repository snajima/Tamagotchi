open OUnit2
open Graphics
open Homemode
open State

(* -------------------------------------------------------------------- *)
(* ------------------- Helper Functions for Testing ------------------- *)
(* -------------------------------------------------------------------- *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let sort_list_1 = List.sort compare lst1 in
  let sort_list_2 = List.sort compare lst2 in
  sort_list_1 = sort_list_2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let equal_sets_test init name a b =
  init ();
  name >:: fun ctxt ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) a b

(* -------------------------------------------------------------------- *)
(* -------------------------- State Testing --------------------------- *)
(* -------------------------------------------------------------------- *)

let sample_baby = "./json/baby.json"

let babe = from_json sample_baby

let sample_teen = "./json/teen.json"

let teen = from_json sample_teen

let sample_senior = "./json/senior.json"

let senior = from_json sample_senior

let equal_value_test init name a b =
  init ();
  name >:: fun ctxt -> assert_equal a b

let death_exc name fxn = name >:: fun ctxt -> assert_raises Death fxn

let negative_money_exc name fxn =
  name >:: fun ctxt -> assert_raises NegativeMoney fxn

let piano : item = { name = "piano"; cost = 10 }

let violin : item = { name = "violin"; cost = 15 }

let main_tests =
  [
    equal_value_test
      (fun () -> ())
      "breed of baby" "crazy" (get_breed babe);
    equal_value_test
      (fun () -> ())
      "breed of teen" "fluffy" (get_breed teen);
    equal_value_test
      (fun () -> ())
      "LifeStage of baby" "Baby" (get_lifeStage babe);
    equal_value_test
      (fun () -> ())
      "LifeStage of teen" "Teenager" (get_lifeStage teen);
    equal_value_test (fun () -> ()) "sleep of baby" 100 (get_sleep babe);
    (* equal_value_test (fun () -> ()) "sleep of teen" 45 (get_sleep
       teen); *)
    (* equal_value_test (fun () -> ()) "sleep of teen, incr 10" 55
       (get_sleep teen); *)
    (* Why are these commented out? *)
    (* equal_value_test (fun () -> set_sleep (-20) teen) "sleep of teen,
       incr -20" 35 (get_sleep teen); *)
    (* equal_value_test (fun () -> set_sleep 1000 teen) "sleep of teen,
       incr 1000" 100 (get_sleep teen); *)
    death_exc "sleep below 0 for teen" (fun () ->
        set_sleep (-1000) teen);
    equal_value_test
      (fun () -> ())
      "cleanliness of baby" 100 (get_cleanliness babe);
    (* equal_value_test (fun () -> ()) "cleanliness of teen" 87
       (get_cleanliness teen); *)
    (* equal_value_test (fun () -> set_cleanliness 10 teen) "cleanliness
       of teen, incr 10" 97 (get_cleanliness teen); *)
    (* equal_value_test (fun () -> set_cleanliness (-20) teen)
       "cleanliness of teen, incr -20" 77 (get_cleanliness teen); *)
    (* equal_value_test (fun () -> set_cleanliness 34 teen) "cleanliness
       of teen, incr 34" 100 (get_cleanliness teen); *)
    death_exc "cleanliness below 0 for teen" (fun () ->
        set_cleanliness (-298) teen);
    equal_value_test
      (fun () -> ())
      "hunger of baby" 100 (get_hunger babe);
    (* equal_value_test (fun () -> ()) "hunger of teen" 83 (get_hunger
       teen); *)
    (* equal_value_test (fun () -> set_hunger 17 teen) "hunger of teen,
       incr 17" 100 (get_hunger teen); *)
    (* equal_value_test (fun () -> set_hunger (-20) teen) "hunger of
       teen, incr -20" 80 (get_hunger teen); *)
    (* equal_value_test (fun () -> set_hunger 34 teen) "hunger of teen,
       incr 34" 100 (get_hunger teen); *)
    death_exc "hunger below 0 for teen" (fun () ->
        set_hunger (-298) teen);
    (* equal_value_test (fun () -> ()) "money of baby" 0 (get_money
       babe); *)
    equal_value_test (fun () -> ()) "money of teen" 10 (get_money teen);
    (* equal_value_test (fun () -> set_money 10 babe) "money of baby,
       incr 10" 10 (get_money babe); *)
    (* equal_value_test (fun () -> set_money 1 babe) "money of baby,
       incr 1" 11 (get_money babe); *)
    (* negative_money_exc "money below 0 for baby" (fun () -> set_money
       (-6) babe); *)
    (* equal_sets_test (fun () -> ()) "inven of teen" [ piano ]
       (get_inventory teen); *)
    (* equal_sets_test (fun () -> set_item violin teen) "inven of teen,
       add 1 violin" [ piano; violin ] (get_inventory teen); *)
    (* equal_sets_test (fun () -> set_item violin teen) "inven of teen,
       add 2 violin" [ piano; violin; violin ] (get_inventory teen); *)
    (* equal_value_test (fun () -> ()) "age of teen" 10 (get_age teen); *)
    (* equal_value_test (fun () -> ()) "age of baby" 0 (get_age babe); *)
    (* equal_value_test (fun () -> increment_age teen) "lifeStage of
       teen incr" "Adult" (get_lifeStage teen); *)
    (* equal_value_test (fun () -> ()) "age of teen incr" 11 (get_age
       teen); *)
    equal_value_test
      (fun () -> increment_age babe)
      "lifeStage of baby incr" "Baby" (get_lifeStage babe);
    (* equal_value_test (fun () -> ()) "age of baby incr" 1 (get_age
       babe); *)
    equal_value_test (fun () -> ()) "age of senior" 35 (get_age senior);
    death_exc "death of senior" (fun () -> increment_age senior);
  ]

(* -------------------------------------------------------------------- *)
(* -------------------------- Dolphin Testing ------------------------- *)
(* -------------------------------------------------------------------- *)

(** [lane_printer lane] returns a string representing [lane] *)
let lane_printer (lane : Dolphin.lane) : string =
  match lane with
  | Left -> "Left"
  | Middle -> "Middle"
  | Right -> "Right"

(** [rock_printer rocks] returns a string representing [rocks] *)
let rock_printer (rocks : (int * int) list) : string =
  let str_list =
    List.map
      (fun (lane, height) ->
        "(" ^ string_of_int lane ^ ", " ^ string_of_int height ^ ") ")
      rocks
  in
  List.fold_left ( ^ ) "( " str_list ^ ")"

(** [num_rocks_printer num_rocks] returns a string representing
    [num_rocks] *)
let num_rocks_printer num_rocks : string = string_of_int num_rocks

(** [dolphin_lane_test name actual_value expected_output] constructs an
    OUnit test named [name] that checks if [expected_output] is equal to
    [actual_value] and uses a custom [lane_printer] *)
let dolphin_lane_test (name : string) actual_value expected_out : test =
  name >:: fun _ ->
  assert_equal expected_out actual_value ~printer:lane_printer

(** [dolphin_rock_test_w_seed name gamestate_func expected_output]
    constructs an OUnit test named [name] that checks if
    [expected_output] is equal to [gamestate_func] applied on a freshly
    initialized Dolphin.gamestate and uses a custom [rock_printer]

    Note that gamestate_func allows for delayed application of the
    Dolphin.add_rock functions. The purpose of this is to allow the seed
    to be set before the add_rock methods (which involve randomness) to
    allow for testing*)
let dolphin_rock_test_w_seed
    ?(seed = 1)
    (name : string)
    gamestate_func
    expected_out : test =
  Random.init 1;
  name >:: fun _ ->
  assert_equal expected_out
    (Dolphin.init_game () |> gamestate_func)
    ~printer:rock_printer

(** [dolphin_num_rock_test name actual_value expected_output] constructs
    an OUnit test named [name] that checks if [expected_output] is equal
    to [actual_value] and uses a custom [num_rocks_printer] *)
let dolphin_num_rock_test (name : string) actual_value expected_out :
    test =
  name >:: fun _ ->
  assert_equal expected_out actual_value ~printer:num_rocks_printer

(** [repeated_next n gamestate] returns the result of applying the
    Dolphin.next function on [gamestate] [n] time *)
let rec repeated_next (n : int) (gamestate : Dolphin.gamestate) :
    Dolphin.gamestate =
  if n = 0 then gamestate
  else repeated_next (n - 1) (gamestate |> Dolphin.next)

let dolphin_test =
  let open Dolphin in
  [
    (* ----------------- Observer: get_dolphin_lane ------------------- *)
    (* -------------------------- One --------------------------- *)
    dolphin_lane_test "Middle to Right"
      (init_game () |> process_right |> get_dolphin_lane)
      Right;
    dolphin_lane_test "Middle to Left"
      (init_game () |> process_left |> get_dolphin_lane)
      Left;
    (* -------------------------- Two --------------------------- *)
    dolphin_lane_test "Middle |> Right |> Right"
      (init_game () |> process_right |> process_right
     |> get_dolphin_lane)
      Right;
    dolphin_lane_test "Middle |> Left |> Left"
      (init_game () |> process_left |> process_left |> get_dolphin_lane)
      Left;
    dolphin_lane_test "Middle |> Right |> Left"
      (init_game () |> process_right |> process_left |> get_dolphin_lane)
      Middle;
    dolphin_lane_test "Middle |> Left |> Right"
      (init_game () |> process_left |> process_right |> get_dolphin_lane)
      Middle;
    (* ------------------------- Three -------------------------- *)
    dolphin_lane_test "Middle |> Right |> Right |> Right"
      (init_game () |> process_right |> process_right |> process_right
     |> get_dolphin_lane)
      Right;
    dolphin_lane_test "Middle |> Right |> Right |> Left"
      (init_game () |> process_right |> process_right |> process_left
     |> get_dolphin_lane)
      Middle;
    dolphin_lane_test "Middle |> Right |> Left |> Right"
      (init_game () |> process_right |> process_left |> process_right
     |> get_dolphin_lane)
      Right;
    dolphin_lane_test "Middle |> Left |> Right |> Right"
      (init_game () |> process_left |> process_right |> process_right
     |> get_dolphin_lane)
      Right;
    dolphin_lane_test "Middle |> Left |> Left |> Right"
      (init_game () |> process_left |> process_left |> process_right
     |> get_dolphin_lane)
      Middle;
    dolphin_lane_test "Middle |> Left |> Right |> Left"
      (init_game () |> process_left |> process_right |> process_left
     |> get_dolphin_lane)
      Left;
    dolphin_lane_test "Middle |> Right |> Left |> Left"
      (init_game () |> process_right |> process_left |> process_left
     |> get_dolphin_lane)
      Left;
    dolphin_lane_test "Middle |> Left |> Left |> Left"
      (init_game () |> process_left |> process_left |> process_left
     |> get_dolphin_lane)
      Left;
    (* --------------------- Observer: get_rocks ---------------------- *)
    (* Seed default is set to 1 - values are: 1, 2, 0, 0, 2, 2, 2, 0, 0,
       0, 2 *)
    (* The application of the gamestate functions are delayed since the
       seed needs to be reset each time the [dolphin_rock_test_w_seed]
       function is called *)
    dolphin_rock_test_w_seed "Adding one rock - middle lane"
      (fun gs -> gs |> add_rock |> next |> get_rocks)
      [ (1, 60) ];
    dolphin_rock_test_w_seed "Adding two rock - left, right lanes"
      (fun gs -> gs |> add_rock |> add_rock |> next |> get_rocks)
      [ (0, 60); (2, 60) ];
    dolphin_rock_test_w_seed "Add rock |> next |> add 2 rocks"
      (fun gs ->
        gs |> add_rock |> next |> add_rock |> add_rock |> next
        |> get_rocks)
      [ (2, 60); (2, 60); (0, 59) ];
    dolphin_rock_test_w_seed
      "Repeat (Add rock |> next) three times then add one last rock"
      (fun gs ->
        gs |> add_rock |> next |> add_rock |> next |> add_rock |> next
        |> add_rock |> next |> get_rocks)
      [ (0, 60); (0, 59); (0, 58); (2, 57) ];
    dolphin_rock_test_w_seed
      "Repeat (Add rock |> next) three times then add one last rock"
      (fun gs ->
        gs |> add_rock |> next |> add_rock |> next |> add_rock |> next
        |> add_rock |> next |> get_rocks)
      [ (0, 60); (1, 59); (2, 58); (2, 57) ];
    dolphin_rock_test_w_seed "Add one rock and fall to bottom"
      (fun gs -> gs |> add_rock |> repeated_next 52 |> get_rocks)
      [ (1, 9) ];
    (* --------------------- Observer: num_rocks ---------------------- *)
    dolphin_num_rock_test "Adding one rock "
      (init_game () |> add_rock |> num_rocks)
      1;
    dolphin_num_rock_test "Adding two rock "
      (init_game () |> add_rock |> add_rock |> num_rocks)
      2;
    dolphin_num_rock_test "Adding three rocks "
      (init_game () |> add_rock |> add_rock |> add_rock |> num_rocks)
      3;
  ]

(* -------------------------------------------------------------------- *)
(* ------------------------- Homemode Testing ------------------------- *)
(* -------------------------------------------------------------------- *)

(* -------------------------------------------------------------------- *)
(* --------------------------- Drum Testing --------------------------- *)
(* -------------------------------------------------------------------- *)

(* -------------------------------------------------------------------- *)
(* ----------------------- Elementalist Testing ----------------------- *)
(* -------------------------------------------------------------------- *)

let suite =
  "test suite for Tamagotchi Final Project"
  >::: List.flatten [ main_tests; dolphin_test ]

let _ = run_test_tt_main suite
