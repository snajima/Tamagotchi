(** Type [item] represents an item that is possible for the Tamagotchi
    to have. Each item has the following:

    - name: a string representing the item's name
    - cost: an integer representing the item's cost*)
type item = {
  name : string;
  cost : int;
}

(** Type [tamagotchi] represents the Tamagotchi's current state which
    includes:

    - breed: a string representing what type of breed it is
    - lifestage: a string representing the lifestage of it (there are
      four lifestages: Baby, Teenager, Adult, and Senior)
    - sleep: an integer between 1 and 100 representing the amount of
      sleep of the Tamagotchi (1 being sleep deprived, 100 being well
      rested)
    - cleanliness: an integer between 1 and 100 representing the
      cleanliness of the Tamagotchi (1 being super dirty, 100 being
      super clean)
    - hunger: an integer between 1 and 100 representing the hunger of
      the Tamagotchi (1 being starving, 100 being full)
    - happiness: an integer between 1 and 100 representing the happiness
      of the Tamagotchi (1 being depressed and 100 representing estatic)
    - age: an integer representing the Tamagotchi's age
    - money: an integer representing the amount of money the Tamagotchi
      has
    - inventory: an item list representing the different items that the
      Tamagotchi has in its inventory, the list can contain duplicates
    - step: an integer keeping track, internally, of how much "time" has
      passed in the game *)
type tamagotchi

(** Raised when the Tamagotchi dies*)
exception Death

(** Raised when the Tamagotchi reaches a negative amount of money*)
exception NegativeMoney

(** [set_sleep num tam] takes in an integer [num] (can be positive or
    negative) and changes the Tamagotchi's [tam] sleep by that amount.
    If [set_sleep] causes [tam.sleep] to go above 100, then [tam.sleep]
    is just set to 100. The function returns the mutated Tamgotchi type.

    Raises: [Death] if [set_sleep] causes [tam.sleep] to go less than or
    equal to 0.*)
val set_sleep : int -> tamagotchi -> tamagotchi

(** [set_hunger num tam] takes in an integer [num] (can be positive or
    negative) and changes the Tamagotchi's [tam] hunger by that amount.
    If [set_hunger] causes [tam.hunger] to go above 100, then
    [tam.hunger] is just set to 100. The function returns the mutated
    Tamgotchi type.

    Raises: [Death] if [set_hunger] causes [tam.hunger] to go less than
    or equal to 0.*)
val set_hunger : int -> tamagotchi -> tamagotchi

(** [set_happy num tam] takes in an integer [num] (can be positive or
    negative) and changes the Tamagotchi's [tam] happiness by that
    amount. If [set_happy] causes [tam.happy] to go above 100, then
    [tam.happy] is just set to 100. The function returns the mutated
    Tamgotchi type.

    Raises: [Death] if [set_happy] causes [tam.happy] to go less than or
    equal to 0.*)
val set_happy : int -> tamagotchi -> tamagotchi

(** [set_cleanliness num tam] takes in an integer [num] (can be positive
    or negative) and changes the Tamagotchi's [tam] cleanliness by that
    amount. If [set_cleanliness] causes [tam.cleanliness] to go above
    100, then [tam.cleanliness] is just set to 100. The function returns
    the mutated Tamgotchi type.

    Raises: [Death] if [set_cleanliness] causes [tam.cleanliness] to go
    less than or equal to 0.*)
val set_cleanliness : int -> tamagotchi -> tamagotchi

(** [set_money num tam] takes in an integer [num] (can be positive or
    negative) and changes the Tamagotchi's [tam] money by that amount.
    The function returns the mutated Tamgotchi type.

    Raises: [NegativeMoney] if [set_money] causes [tam.money] to go
    negative.*)
val set_money : int -> tamagotchi -> tamagotchi

(** [set_item item tam] takes in a type item [item] and adds that to the
    Tamagotchi's [tam] inventory [tam.inventory]. The function returns
    the mutated Tamgotchi type. *)
val set_item : item -> tamagotchi -> tamagotchi

(** [init_tam file] returns a type tamagotchi that represents that
    tamagotchi stored in the file [file].*)
val init_tam : string -> tamagotchi

(** [get_breed tam] returns a string representing the Tamagotchi's [tam]
    breed.*)
val get_breed : tamagotchi -> string

(** [get_lifestage tam] returns a string representing the Tamagotchi's
    [tam] current life stage.*)
val get_lifestage : tamagotchi -> string

(** [get_sleep tam] returns an integer representing the Tamagotchi's
    [tam] current state of sleep.*)
val get_sleep : tamagotchi -> int

(** [get_cleanliness tam] returns an integer representing the
    Tamagotchi's [tam] current state of cleanliness.*)
val get_cleanliness : tamagotchi -> int

(** [get_hunger tam] returns an integer representing the Tamagotchi's
    [tam] current state of hunger.*)
val get_hunger : tamagotchi -> int

(** [get_happy tam] returns an integer representing the Tamagotchi's
    [tam] current state of happiness.*)
val get_happy : tamagotchi -> int

(** [get_age tam] returns an integer representing the Tamagotchi's [tam]
    current age.*)
val get_age : tamagotchi -> int

(** [get_money tam] returns an integer representing the Tamagotchi's
    [tam] current money amount.*)
val get_money : tamagotchi -> int

(** [get_inventory tam] returns an item list representing the
    Tamagotchi's [tam] current inventory.*)
val get_inventory : tamagotchi -> item list

(** [increment_age tam] increases the Tamagotchi's [tam] by 1. The age
    ranges for life stage is as follows: 0-5 for Baby, 6-10 for
    Teenager, 11-25 for Adult, 26-35 for Senior. If [increment_age tam]
    makes the Tamagotchi [tam] reach a new life stage, then this
    function automatically updates their lifestage [tam.lifestage] as
    well. The function returns the mutated Tamgotchi type.

    Raises [Death] if [increment_age tam] is called on a Tamagotchi age
    35*)
val increment_age : tamagotchi -> tamagotchi

(** [increment_eat tam] increases the Tamagotchi's hunger state
    [tam.hunger], using [set_hunger num tam], by a set increment defined
    internally in the module.*)
val increment_eat : tamagotchi -> tamagotchi

(** [increment_sleep tam] increases the Tamagotchi's sleep state
    [tam.sleep], using [set_sleep num tam], by a set increment defined
    internally in the module.*)
val increment_sleep : tamagotchi -> tamagotchi

(** [increment_cleanliness tam] increases the Tamagotchi's cleanliness
    state [tam.cleanliness], using [set_cleanliness num tam], by a set
    increment defined internally in the module.*)
val increment_cleanliness : tamagotchi -> tamagotchi

(** [increment_happy tam] increases the Tamagotchi's happiness state
    [tam.happy], using [set_happy num tam], by a set increment defined
    internally in the module.*)
val increment_happy : tamagotchi -> tamagotchi

(** [decrement_happy tam] decreases the Tamagotchi's happiness state
    [tam.happy], using [set_happy num tam], by a set increment defined
    internally in the module.*)
val decrement_happy : tamagotchi -> tamagotchi

(** [step tam] increases the Tamagotchi's [tam] step state [tam.step] by
    1 and is called constantly by a time frame dictated in the homemode
    module. Each step is equivilent to one day in real life, so
    everytime [tam.step] is a multiple of 365, the Tamogatchi will
    celebrate its birthday. For each birthday, [tam.age] will increase
    by 1, [tam.cleanliness] will decrease by 10, [tam.hunger] will
    decrease by 10, [tam.happy] will decrease by 10, and [tam.sleep]
    will decrease by 10.*)
val step : tamagotchi -> tamagotchi
