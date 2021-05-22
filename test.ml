open OUnit2
open Homemode
open State

(* -------------------------------------------------------------------- *)
(* -------------------------- State Testing --------------------------- *)
(* -------------------------------------------------------------------- *)
(*Objects and inventory lists used for testing*)
let senior_teen_inventory = [ { name = "piano"; cost = 10 } ]

(*Printer functions*)
let num_printer num : string = string_of_int num

let string_printer str : string = str

(*Helper functions for testing*)
let str_feature_test name a b =
  name >:: fun ctxt -> assert_equal a b ~printer:string_printer

let num_feature_test name a b =
  name >:: fun ctxt -> assert_equal a b ~printer:num_printer

let equal_sets_test name a b = name >:: fun ctxt -> assert_equal a b

let death_exc name fxn = name >:: fun ctxt -> assert_raises Death fxn

let negative_money_exc name fxn =
  name >:: fun ctxt -> assert_raises NegativeMoney fxn

(* [repeated_step n tam] returns the result of applying the State.step
   function on [tam] [n] time *)
let rec repeated_step n tam =
  if n = 0 then tam else repeated_step (n - 1) (step tam)

let state_tests =
  [
    (* ----------------- Observer: get_breed ------------------- *)
    str_feature_test "breed of baby" "crazy"
      (init_tam "./json/baby.json" |> get_breed);
    str_feature_test "breed of teen" "fluffy"
      (init_tam "./json/teen.json" |> get_breed);
    str_feature_test "breed of senior" "bald"
      (init_tam "./json/senior.json" |> get_breed);
    (* ---------------- Observer: get_lifestage ------------------ *)
    (* ----------------------- No Change ------------------------- *)
    str_feature_test "Lifestage of baby" "Baby"
      (init_tam "./json/baby.json" |> get_lifestage);
    str_feature_test "Lifestage of teen" "Teenager"
      (init_tam "./json/teen.json" |> get_lifestage);
    str_feature_test "Lifestage of senior" "Senior"
      (init_tam "./json/senior.json" |> get_lifestage);
    (* --------------------- Increment Age ---------------------- *)
    str_feature_test "Lifestage of baby incr 1" "Baby"
      (init_tam "./json/baby.json" |> increment_age |> get_lifestage);
    str_feature_test "Lifestage of teen incr 1" "Adult"
      (init_tam "./json/teen.json" |> increment_age |> get_lifestage);
    str_feature_test "Lifestage of senior incr 1" "Senior"
      (init_tam "./json/senior.json" |> increment_age |> get_lifestage);
    (* --------------------------- Step ---------------------------- *)
    str_feature_test "Lifestage of baby incr 2" "Teenager"
      (init_tam "./json/baby.json" |> repeated_step 729 |> get_lifestage);
    str_feature_test "Lifestage of teen incr 2" "Adult"
      (init_tam "./json/teen.json" |> repeated_step 729 |> get_lifestage);
    death_exc "Lifestage of senior incr 2" (fun () ->
        init_tam "./json/senior.json" |> repeated_step 729);
    (* -------------------- Observer: get_sleep ---------------------- *)
    (* ------------------------ No Change ------------------------- *)
    num_feature_test "sleep of baby" 100
      (init_tam "./json/baby.json" |> get_sleep);
    num_feature_test "sleep of teen" 45
      (init_tam "./json/teen.json" |> get_sleep);
    num_feature_test "sleep of senior" 90
      (init_tam "./json/senior.json" |> get_sleep);
    (* ---------------------- Set Sleep ----------------------- *)
    num_feature_test "sleep of baby -20" 80
      (init_tam "./json/baby.json" |> set_sleep (-20) |> get_sleep);
    num_feature_test "sleep of teen -20" 25
      (init_tam "./json/teen.json" |> set_sleep (-20) |> get_sleep);
    num_feature_test "sleep of senior -20" 70
      (init_tam "./json/senior.json" |> set_sleep (-20) |> get_sleep);
    death_exc "death sleep of baby" (fun () ->
        init_tam "./json/baby.json" |> set_sleep (-100));
    num_feature_test "negative edge case sleep of teen" 1
      (init_tam "./json/teen.json" |> set_sleep (-44) |> get_sleep);
    num_feature_test "positive edge case sleep of senior" 100
      (init_tam "./json/senior.json" |> set_sleep 10 |> get_sleep);
    num_feature_test "over 100 sleep of senior" 100
      (init_tam "./json/senior.json" |> set_sleep 300 |> get_sleep);
    (* ----------------------- Increment Sleep ------------------------ *)
    num_feature_test "increment sleep of baby" 100
      (init_tam "./json/baby.json" |> increment_sleep |> get_sleep);
    num_feature_test "increment sleep of teen" 50
      (init_tam "./json/teen.json" |> increment_sleep |> get_sleep);
    num_feature_test "increment sleep of senior" 95
      (init_tam "./json/senior.json" |> increment_sleep |> get_sleep);
    (* ---------------------------- Step ----------------------------- *)
    num_feature_test "step sleep of baby" 90
      (init_tam "./json/baby.json" |> repeated_step 364 |> get_sleep);
    num_feature_test "step sleep of teen" 35
      (init_tam "./json/teen.json" |> repeated_step 364 |> get_sleep);
    num_feature_test "step sleep of senior" 80
      (init_tam "./json/senior.json" |> repeated_step 364 |> get_sleep);
    (* ------------------ Observer: get_cleanliness -------------------- *)
    (* ------------------------ No Change ------------------------- *)
    num_feature_test "clean of baby" 100
      (init_tam "./json/baby.json" |> get_cleanliness);
    num_feature_test "clean of teen" 87
      (init_tam "./json/teen.json" |> get_cleanliness);
    num_feature_test "clean of senior" 70
      (init_tam "./json/senior.json" |> get_cleanliness);
    (* ---------------------- Set Cleanliness ----------------------- *)
    num_feature_test "clean of baby -20" 80
      ( init_tam "./json/baby.json"
      |> set_cleanliness (-20) |> get_cleanliness );
    num_feature_test "clean of teen -20" 67
      ( init_tam "./json/teen.json"
      |> set_cleanliness (-20) |> get_cleanliness );
    num_feature_test "clean of senior -20" 50
      ( init_tam "./json/senior.json"
      |> set_cleanliness (-20) |> get_cleanliness );
    death_exc "death clean of teen" (fun () ->
        init_tam "./json/teen.json" |> set_cleanliness (-87));
    num_feature_test "negative edge case clean of baby" 1
      ( init_tam "./json/baby.json"
      |> set_cleanliness (-99) |> get_cleanliness );
    num_feature_test "positive edge case clean of teen" 100
      ( init_tam "./json/teen.json"
      |> set_cleanliness 13 |> get_cleanliness );
    num_feature_test "over 100 clean of senior" 100
      ( init_tam "./json/senior.json"
      |> set_cleanliness 300 |> get_cleanliness );
    (* -------------------- Increment Cleanliness --------------------- *)
    num_feature_test "increment clean of baby" 100
      ( init_tam "./json/baby.json"
      |> increment_cleanliness |> get_cleanliness );
    num_feature_test "increment clean of teen" 92
      ( init_tam "./json/teen.json"
      |> increment_cleanliness |> get_cleanliness );
    num_feature_test "increment clean of senior" 75
      ( init_tam "./json/senior.json"
      |> increment_cleanliness |> get_cleanliness );
    (* ---------------------------- Step ----------------------------- *)
    num_feature_test "step clean of baby" 90
      ( init_tam "./json/baby.json"
      |> repeated_step 460 |> get_cleanliness );
    num_feature_test "step clean of teen" 77
      ( init_tam "./json/teen.json"
      |> repeated_step 460 |> get_cleanliness );
    num_feature_test "step clean of senior" 60
      ( init_tam "./json/senior.json"
      |> repeated_step 460 |> get_cleanliness );
    (* ------------------ Observer: get_hunger -------------------- *)
    (* ------------------------ No Change ------------------------- *)
    num_feature_test "hunger of baby" 100
      (init_tam "./json/baby.json" |> get_hunger);
    num_feature_test "hunger of teen" 28
      (init_tam "./json/teen.json" |> get_hunger);
    num_feature_test "hunger of senior" 83
      (init_tam "./json/senior.json" |> get_hunger);
    (* ---------------------- Set Hunger ----------------------- *)
    num_feature_test "hunger of baby -20" 80
      (init_tam "./json/baby.json" |> set_hunger (-20) |> get_hunger);
    num_feature_test "hunger of teen -20" 8
      (init_tam "./json/teen.json" |> set_hunger (-20) |> get_hunger);
    num_feature_test "hunger of senior -20" 63
      (init_tam "./json/senior.json" |> set_hunger (-20) |> get_hunger);
    death_exc "death hunger of senior" (fun () ->
        init_tam "./json/senior.json" |> set_hunger (-550));
    num_feature_test "negative edge case hunger of teen" 1
      (init_tam "./json/teen.json" |> set_hunger (-27) |> get_hunger);
    num_feature_test "positive edge case hunger of baby" 100
      (init_tam "./json/baby.json" |> set_hunger 0 |> get_hunger);
    num_feature_test "over 100 hunger of senior" 100
      (init_tam "./json/senior.json" |> set_hunger 18 |> get_hunger);
    (* ---------------------- Increment Hunger ----------------------- *)
    num_feature_test "increment hunger of baby" 100
      (init_tam "./json/baby.json" |> increment_eat |> get_hunger);
    num_feature_test "increment hunger of teen" 33
      (init_tam "./json/teen.json" |> increment_eat |> get_hunger);
    num_feature_test "increment hunger of senior" 88
      (init_tam "./json/senior.json" |> increment_eat |> get_hunger);
    (* ---------------------------- Step ----------------------------- *)
    num_feature_test "step hunger of baby" 80
      (init_tam "./json/baby.json" |> repeated_step 740 |> get_hunger);
    num_feature_test "step hunger of teen" 8
      (init_tam "./json/teen.json" |> repeated_step 740 |> get_hunger);
    num_feature_test "step hunger of senior" 73
      (init_tam "./json/senior.json" |> repeated_step 365 |> get_hunger);
    (* ------------------ Observer: get_happy -------------------- *)
    (* ------------------------ No Change ------------------------- *)
    num_feature_test "happy of baby" 100
      (init_tam "./json/baby.json" |> get_happy);
    num_feature_test "happy of teen" 42
      (init_tam "./json/teen.json" |> get_happy);
    num_feature_test "happy of senior" 93
      (init_tam "./json/senior.json" |> get_happy);
    (* ---------------------- Set Happy ----------------------- *)
    num_feature_test "happy of baby -20" 80
      (init_tam "./json/baby.json" |> set_happy (-20) |> get_happy);
    num_feature_test "happy of teen -20" 22
      (init_tam "./json/teen.json" |> set_happy (-20) |> get_happy);
    num_feature_test "happy of senior -20" 73
      (init_tam "./json/senior.json" |> set_happy (-20) |> get_happy);
    death_exc "death happy of baby" (fun () ->
        init_tam "./json/baby.json" |> set_happy (-100));
    num_feature_test "negative edge case happy of senior" 1
      (init_tam "./json/senior.json" |> set_happy (-92) |> get_happy);
    num_feature_test "positive edge case happy of teen" 100
      (init_tam "./json/teen.json" |> set_happy 58 |> get_happy);
    num_feature_test "over 100 happy of senior" 100
      (init_tam "./json/senior.json" |> set_happy 100 |> get_happy);
    (* ---------------------- Increment Happy ----------------------- *)
    num_feature_test "increment happy of baby" 100
      (init_tam "./json/baby.json" |> increment_happy |> get_happy);
    num_feature_test "increment happy of teen" 47
      (init_tam "./json/teen.json" |> increment_happy |> get_happy);
    num_feature_test "increment happy of senior" 98
      (init_tam "./json/senior.json" |> increment_happy |> get_happy);
    (* ---------------------- Decrement Happy ----------------------- *)
    num_feature_test "increment happy of baby" 95
      (init_tam "./json/baby.json" |> decrement_happy |> get_happy);
    num_feature_test "increment happy of teen" 37
      (init_tam "./json/teen.json" |> decrement_happy |> get_happy);
    num_feature_test "increment happy of senior" 88
      (init_tam "./json/senior.json" |> decrement_happy |> get_happy);
    (* ---------------------------- Step ----------------------------- *)
    num_feature_test "step happy of baby" 90
      (init_tam "./json/baby.json" |> repeated_step 364 |> get_happy);
    num_feature_test "step happy of teen" 32
      (init_tam "./json/teen.json" |> repeated_step 364 |> get_happy);
    num_feature_test "step happy of senior" 83
      (init_tam "./json/senior.json" |> repeated_step 364 |> get_happy);
    (* ------------------ Observer: get_age -------------------- *)
    (* ------------------------ No Change ------------------------- *)
    num_feature_test "age of baby" 4
      (init_tam "./json/baby.json" |> get_age);
    num_feature_test "age of teen" 10
      (init_tam "./json/teen.json" |> get_age);
    num_feature_test "age of senior" 34
      (init_tam "./json/senior.json" |> get_age);
    (* ------------------------ Increment Age ------------------------- *)
    num_feature_test "increment age of baby 3" 7
      (init_tam "./json/baby.json"
      |> increment_age |> increment_age |> increment_age |> get_age);
    num_feature_test "increment age of teen 2" 12
      (init_tam "./json/teen.json"
      |> increment_age |> increment_age |> get_age);
    num_feature_test "increment age of senior 1" 35
      (init_tam "./json/senior.json" |> increment_age |> get_age);
    (* ---------------------------- Step ----------------------------- *)
    num_feature_test "step age of baby" 6
      (init_tam "./json/baby.json" |> repeated_step 740 |> get_age);
    num_feature_test "step age of teen" 11
      (init_tam "./json/teen.json" |> repeated_step 364 |> get_age);
    num_feature_test "step age of senior" 34
      (init_tam "./json/senior.json" |> repeated_step 363 |> get_age);
    (* ------------------ Observer: get_money -------------------- *)
    (* ------------------------ No Change ------------------------- *)
    num_feature_test "money of baby" 0
      (init_tam "./json/baby.json" |> get_money);
    num_feature_test "money of teen" 10
      (init_tam "./json/teen.json" |> get_money);
    num_feature_test "money of senior" 10
      (init_tam "./json/senior.json" |> get_money);
    (* ------------------------ Set Money ------------------------- *)
    num_feature_test "set money of baby 10" 10
      (init_tam "./json/baby.json" |> set_money 10 |> get_money);
    num_feature_test "set money of teen -10" 0
      (init_tam "./json/teen.json" |> set_money (-10) |> get_money);
    negative_money_exc "senior in debt" (fun () ->
        init_tam "./json/senior.json" |> set_money (-11));
    (* ------------------ Observer: get_inventory -------------------- *)
    (* ------------------------ No Change ------------------------- *)
    equal_sets_test "inventory of baby" []
      (init_tam "./json/baby.json" |> get_inventory);
    equal_sets_test "inventory of teen" senior_teen_inventory
      (init_tam "./json/teen.json" |> get_inventory);
    equal_sets_test "inventory of senior" senior_teen_inventory
      (init_tam "./json/senior.json" |> get_inventory);
    (* ------------------------ Set Item ------------------------- *)
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
      ( init_game () |> process_right |> process_right
      |> get_dolphin_lane )
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
      ( init_game () |> process_right |> process_right |> process_right
      |> get_dolphin_lane )
      Right;
    dolphin_lane_test "Middle |> Right |> Right |> Left"
      ( init_game () |> process_right |> process_right |> process_left
      |> get_dolphin_lane )
      Middle;
    dolphin_lane_test "Middle |> Right |> Left |> Right"
      ( init_game () |> process_right |> process_left |> process_right
      |> get_dolphin_lane )
      Right;
    dolphin_lane_test "Middle |> Left |> Right |> Right"
      ( init_game () |> process_left |> process_right |> process_right
      |> get_dolphin_lane )
      Right;
    dolphin_lane_test "Middle |> Left |> Left |> Right"
      ( init_game () |> process_left |> process_left |> process_right
      |> get_dolphin_lane )
      Middle;
    dolphin_lane_test "Middle |> Left |> Right |> Left"
      ( init_game () |> process_left |> process_right |> process_left
      |> get_dolphin_lane )
      Left;
    dolphin_lane_test "Middle |> Right |> Left |> Left"
      ( init_game () |> process_right |> process_left |> process_left
      |> get_dolphin_lane )
      Left;
    dolphin_lane_test "Middle |> Left |> Left |> Left"
      ( init_game () |> process_left |> process_left |> process_left
      |> get_dolphin_lane )
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
  >::: List.flatten [ state_tests; dolphin_test ]

let _ = run_test_tt_main suite
