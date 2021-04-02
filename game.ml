type move = 
  | Left
  | Right

type location = {
  (** there are three lanes in dolphin game, starting at index 0 for 1st
  lane *)
  mutable lane : int;
}

exception InvalidMove

exception Lose

let get_lane tam = tam.lane

(** For changing location from key input, doesn't work yet *)
let change_location tam = 
  let key_input = 'a' in
  let new_lane = match key_input with 
  | 'a' -> tam.lane - 1
  | 'd' -> tam.lane + 1
  | _ -> -1
in
  if (new_lane < 0 || new_lane > 2) then raise InvalidMove
  else tam.lane <- new_lane