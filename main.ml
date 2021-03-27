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

let add_currency amount tam =
  let old_amount = tam.money in
  tam.money <- amount + old_amount

let add_item item tam =
  let old_inven = tam.inventory in
  tam.inventory <- item :: old_inven
