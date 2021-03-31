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
  mutable age : int;
  mutable money : int;
  mutable inventory : item list;
}

exception WrongLifeStage

let item_of_json json =
  {
    name = json |> member "name" |> to_string;
    cost = json |> member "cost" |> to_int;
  }

let from_json json =
  {
    breed = json |> member "breed" |> to_string;
    lifeStage = json |> member "lifeStage" |> to_string;
    sleep = json |> member "sleep" |> to_int;
    cleanliness = json |> member "cleanliness" |> to_int;
    hunger = json |> member "hunger" |> to_int;
    age = json |> member "age" |> to_int;
    money = json |> member "money" |> to_int;
    inventory =
      json |> member "inventory" |> to_list |> List.map item_of_json;
  }

let get_breed tam = tam.breed

let get_lifeStage tam = tam.lifeStage

(** This function only increments the lifecycle.*)
let increment_lifeStage tam =
  let current_stage = tam.lifeStage in
  match current_stage with
  | "Baby" -> tam.lifeStage <- "Teenager"
  | "Teenager" -> tam.lifeStage <- "Adult"
  | "Adult" -> tam.lifeStage <- "Senior"
  | "Senior" -> tam.lifeStage <- "Baby"
  | _ -> raise WrongLifeStage

let get_sleep tam = tam.sleep

(** Amount can be positive or negative*)
let set_sleep amount tam =
  let old_amount = tam.sleep in
  tam.sleep <- amount + old_amount

let get_cleanliness tam = tam.cleanliness

(** Amount can be positive or negative*)
let set_cleanliness amount tam =
  let old_amount = tam.cleanliness in
  tam.cleanliness <- amount + old_amount

let get_hunger tam = tam.hunger

(** Amount can be positive or negative*)
let set_hunger amount tam =
  let old_amount = tam.hunger in
  tam.hunger <- amount + old_amount

let get_age tam = tam.age

let increment_age tam =
  let old_age = tam.age in
  tam.age <- 1 + old_age

let get_money tam = tam.money

(** Amount can be positive or negative*)
let set_money amount tam =
  let old_amount = tam.money in
  tam.money <- amount + old_amount

let get_inventory tam = tam.inventory

(**Adding item to the list*)
let set_item item tam =
  let old_inven = tam.inventory in
  tam.inventory <- item :: old_inven
