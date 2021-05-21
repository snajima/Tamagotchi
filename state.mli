type item

type tamagotchi

val from_json : string -> tamagotchi

val get_breed : tamagotchi -> string

val get_lifeStage : tamagotchi -> string

val get_sleep : tamagotchi -> int

val set_sleep : int -> tamagotchi -> unit

val get_cleanliness : tamagotchi -> int

val set_cleanliness : int -> tamagotchi -> unit

val get_hunger : tamagotchi -> int

val set_hunger : int -> tamagotchi -> unit

val get_age : tamagotchi -> int

val increment_age : tamagotchi -> unit

val get_money : tamagotchi -> int

val set_money : int -> tamagotchi -> unit

val get_inventory : tamagotchi -> item list

val set_item : item -> tamagotchi -> unit
