open OUnit2
open Main

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

let equal_sets_test name a b =
  name >:: fun ctxt ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) a b

let sample_baby = Yojson.Basic.from_file "baby.json"

let babe = from_json sample_baby

let sample_teen = Yojson.Basic.from_file "teen.json"

let teen = from_json sample_teen

let sample_senior = Yojson.Basic.from_file "senior.json"

let senior = from_json sample_senior

let equal_value_test name a b = name >:: fun ctxt -> assert_equal a b

let death_exc name fxn = name >:: fun ctxt -> assert_raises Death fxn

let negative_money_exc name fxn =
  name >:: fun ctxt -> assert_raises NegativeMoney fxn

let piano = { name = "piano"; cost = 10 }

let violin = { name = "violin"; cost = 15 }

let main_tests =
  [
    equal_value_test "breed of baby" "crazy" (get_breed babe);
    equal_value_test "breed of teen" "fluffy" (get_breed teen);
    equal_value_test "LifeStage of baby" "Baby" (get_lifeStage babe);
    equal_value_test "LifeStage of teen" "Teenager" (get_lifeStage teen);
    equal_value_test "sleep of baby" 100 (get_sleep babe);
    equal_value_test "sleep of teen" 45 (get_sleep teen);
    set_sleep 10 teen;
    equal_value_test "sleep of teen, incr 10" 55 (get_sleep teen);
    set_sleep (-20) teen;
    equal_value_test "sleep of teen, incr -20" 35 (get_sleep teen);
    set_sleep 1000 teen;
    equal_value_test "sleep of teen, incr 1000" 100 (get_sleep teen);
    death_exc "sleep below 0 for teen" (fun () ->
        set_sleep (-1000) teen);
    equal_value_test "cleanliness of baby" 100 (get_cleanliness babe);
    equal_value_test "cleanliness of teen" 87 (get_cleanliness teen);
    set_cleanliness 10 teen;
    equal_value_test "cleanliness of teen, incr 10" 97
      (get_cleanliness teen); set_cleanliness (-20) teen;
    equal_value_test "cleanliness of teen, incr -20" 77
      (get_cleanliness teen); set_cleanliness 34 teen;
    equal_value_test "cleanliness of teen, incr 34" 100
      (get_cleanliness teen);
    death_exc "cleanliness below 0 for teen" (fun () ->
        set_cleanliness (-298) teen);
    equal_value_test "hunger of baby" 100 (get_hunger babe);
    equal_value_test "hunger of teen" 83 (get_hunger teen);
    set_hunger 17 teen;
    equal_value_test "hunger of teen, incr 17" 100 (get_hunger teen);
    set_hunger (-20) teen;
    equal_value_test "hunger of teen, incr -20" 80 (get_hunger teen);
    set_hunger 34 teen;
    equal_value_test "hunger of teen, incr 34" 100 (get_hunger teen);
    death_exc "hunger below 0 for teen" (fun () ->
        set_hunger (-298) teen);
    equal_value_test "money of baby" 0 (get_money babe);
    equal_value_test "money of teen" 10 (get_money teen);
    set_money 10 babe;
    equal_value_test "money of baby, incr 10" 10 (get_money babe);
    set_money (-5) babe;
    equal_value_test "money of baby, incr -5" 5 (get_money babe);
    negative_money_exc "money below 0 for baby" (fun () ->
        set_money (-6) babe);
    equal_sets_test "inven of baby" [] (get_inventory babe);
    equal_sets_test "inven of teen" [ piano ] (get_inventory teen);
    set_item violin teen;
    equal_sets_test "inven of teen, add 1 violin" [ (piano, violin) ]
      (get_inventory teen); set_item violin teen;
    equal_sets_test "inven of teen, add 2 violin"
      [ (piano, violin, violin) ]
      (get_inventory teen);
    equal_value_test "age of teen" 10 (get_age teen);
    equal_value_test "age of baby" 0 (get_age babe); increment_age teen;
    equal_value_test "lifeStage of teen incr" "Adult"
      (get_lifeStage teen);
    equal_value_test "age of teen incr" 11 (get_age teen);
    increment_age babe;
    equal_value_test "lifeStage of baby incr" "Baby"
      (get_lifeStage babe);
    equal_value_test "age of baby incr" 1 (get_age babe);
    equal_value_test "age of senior" 35 (get_age senior);
    death_exc "death of senior" (fun () -> increment_age senior);
  ]

let suite =
  "test suite for Tamagotchi Final Project"
  >::: List.flatten [ main_tests ]

let _ = run_test_tt_main suite
