open Yojson.Basic.Util

type item = {
  name : string;
  cost : int;
}

type tamagotchi = {
  breed : string;
  mutable lifeStage : string;
  mutable sleep : int;
  mutable cleanliness : int;
  mutable hunger : int;
  mutable happy : int;
  mutable age : int;
  mutable money : int;
  mutable inventory : item list;
  mutable step : int;
}

exception Death

exception NegativeMoney

(*[increment] is the increment amount for [increment_eat],
  [increment_cleanliness], and [increment_sleep] is 5*)
let increment = 5

(*[item_of_json] is a helper function for [init_tam] that converts an
  item in the json file to a type item*)
let item_of_json json =
  {
    name = json |> member "name" |> to_string;
    cost = json |> member "cost" |> to_int;
  }

let init_tam file_name =
  let json = Yojson.Basic.from_file file_name in
  {
    breed = json |> member "breed" |> to_string;
    lifeStage = json |> member "lifeStage" |> to_string;
    sleep = json |> member "sleep" |> to_int;
    cleanliness = json |> member "cleanliness" |> to_int;
    hunger = json |> member "hunger" |> to_int;
    happy = json |> member "happy" |> to_int;
    age = json |> member "age" |> to_int;
    money = json |> member "money" |> to_int;
    inventory =
      json |> member "inventory" |> to_list |> List.map item_of_json;
    step = json |> member "step" |> to_int;
  }

let get_breed tam = tam.breed

let get_lifeStage tam = tam.lifeStage

let get_sleep tam = tam.sleep

let set_sleep amount tam =
  let new_amount = amount + tam.sleep in
  if new_amount <= 0 then raise Death
  else if new_amount <= 100 then tam.sleep <- new_amount
  else tam.sleep <- 100;
  tam

let get_cleanliness tam = tam.cleanliness

let set_cleanliness amount tam =
  let new_amount = amount + tam.cleanliness in
  if new_amount <= 0 then raise Death
  else if new_amount <= 100 then tam.cleanliness <- new_amount
  else tam.cleanliness <- 100;
  tam

let get_hunger tam = tam.hunger

let set_hunger amount tam =
  let new_amount = amount + tam.hunger in
  if new_amount <= 0 then raise Death
  else if new_amount <= 100 then tam.hunger <- new_amount
  else tam.hunger <- 100;
  tam

let get_happy tam = tam.happy

let set_happy amount tam =
  let new_amount = amount + tam.happy in
  if new_amount <= 0 then raise Death
  else if new_amount <= 100 then tam.happy <- new_amount
  else tam.happy <- 100;
  tam

let get_age tam = tam.age

let increment_age tam =
  let new_age = tam.age + 1 in
  if new_age = 6 then tam.lifeStage <- "Teenager"
  else if new_age = 11 then tam.lifeStage <- "Adult"
  else if new_age = 26 then tam.lifeStage <- "Senior"
  else if new_age > 35 then raise Death;
  tam.age <- new_age;
  tam

let get_money tam = tam.money

let set_money amount tam =
  let new_amount = tam.money + amount in
  if new_amount < 0 then raise NegativeMoney
  else tam.money <- new_amount;
  tam

let get_inventory tam = tam.inventory

let set_item item tam =
  tam.inventory <- item :: tam.inventory;
  tam

let increment_eat tam = set_hunger increment tam

let increment_sleep tam = set_sleep increment tam

let increment_cleanliness tam = set_cleanliness increment tam

let increment_happy tam = set_happy increment tam

let decrement_happy tam = set_happy (-increment) tam

let step tam =
  tam.step <- tam.step + 1;
  if tam.step mod 365 = 0 then (
    ignore (increment_age tam);
    ignore (set_hunger (-10) tam);
    ignore (set_cleanliness (-10) tam);
    ignore (set_sleep (-10) tam);
    ignore (set_happy (-10) tam) );
  tam

(**Saving a game idk i might work on this later*)

(* let save tam = Yojson.to_file "hi" *)
