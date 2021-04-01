open OUnit2
open Main

let sample_baby = Yojson.Basic.from_file "baby.json"

let babe = from_json sample_baby

let sample_teen = Yojson.Basic.from_file "teen.json"

let teen = from_json sample_teen

let equal_value_test name a b = name >:: fun ctxt -> assert_equal a b

let death_exc name fxn = name >:: fun ctxt -> assert_raises Death fxn

let negative_money_exc name fxn =
  name >:: fun ctxt -> assert_raises NegativeMoney fxn

let wrong_lifestage_exc name fxn =
  name >:: fun ctxt -> assert_raises WrongLifeStage fxn

let main_tests =
  [
    equal_value_test "breed of baby" "crazy" get_breed babe;
    equal_value_test "LifeStage of baby" "Baby" get_lifeStage babe;
    equal_value_test "LifeStage of teen" "Teenager" get_lifeStage teen;
    increment_age teen;
    equal_value_test "LifeStage of teen to adult" "Adult" get_lifeStage teen; 
    equal_value_test "Age of teen to adult" 11 get_age teen;
  ]

let suite =
  "test suite for Tamagotchi Final Project"
  >::: List.flatten [ main_tests ]

let _ = run_test_tt_main suite
