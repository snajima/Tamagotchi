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

exception Death

exception NegativeMoney

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

let get_sleep tam = tam.sleep

(** Amount can be positive or negative*)
let set_sleep amount tam =
  let new_amount = amount + tam.sleep in
  if new_amount < 0 then raise Death
  else if new_amount <= 100 then tam.sleep <- new_amount
  else tam.sleep <- 100

let get_cleanliness tam = tam.cleanliness

(** Amount can be positive or negative*)
let set_cleanliness amount tam =
  let new_amount = amount + tam.cleanliness in
  if new_amount < 0 then raise Death
  else if new_amount <= 100 then tam.cleanliness <- new_amount
  else tam.cleanliness <- 100

let get_hunger tam = tam.hunger

(** Amount can be positive or negative*)
let set_hunger amount tam =
  let new_amount = amount + tam.hunger in
  if new_amount < 0 then raise Death
  else if new_amount <= 100 then tam.hunger <- new_amount
  else tam.hunger <- 100

let get_age tam = tam.age

(**The age ranges for lifestage is as follows: 0-5 for Baby, 6-10 for
   Teenager, 11-25 for Adult, 26-35 for Senior. If incrementing the age
   pushes the Tamagotchi to a new lifestage, then it automatically
   changes its lifestage too *)
let increment_age tam =
  let new_age = tam.age + 1 in
  if new_age = 6 then tam.lifeStage <- "Teenager"
  else if new_age = 11 then tam.lifeStage <- "Adult"
  else if new_age = 26 then tam.lifeStage <- "Senior"
  else if new_age > 35 then raise Death;
  tam.age <- new_age

let get_money tam = tam.money

(** Amount can be positive or negative. Raise Exception NegativeMoney if
    the new amount of money becomes negative.*)
let set_money amount tam =
  let new_amount = tam.money + amount in
  if new_amount < 0 then raise NegativeMoney
  else tam.money <- new_amount

let get_inventory tam = tam.inventory

(**Adding item to the list. The list is a set that can contain
   duplicates*)
let set_item item tam = tam.inventory <- item :: tam.inventory
