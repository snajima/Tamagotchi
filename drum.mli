(** Functional game engine that simulates the execution of the Drum
    game, inspired by the Japanese game "Taiko no Tatsujin" *)

(** The type [color] represents the two possible colors of the beats in
    the game. *)
type color =
  | Don
  | Ka

(** The type [gamestate] represents state of the Dolphin game simulation
    which stores:

    - The lane the player is currently in
    - The number of steps that have occured in the game simulation
    - The positions of all the rocks in the current game simulation

    Note that since adding rocks involves randomness, calling [next] on
    two identical gamestates may not result in the same result *)
type beat =
  | Right of int
  | Idle
  | Left of int

(** The type [gamestate] represents state of the Drum game simulation
    which stores:

    - The score of the player
    - The combo of the player
    - The color and location of beats
    - The inputted button for animating purposes
    - Good multipler (i.e. how many more points you get when you hit a
      good note vs an "ok" one)
    - A combo multipler (i.e. how much your store exponentially
      increases when you have a high combo)
    - The range from the drum in which a "good" hit is detected
    - The range from the drum in which a "ok" hit is detected
    - The range from the drum in which a "bad" hit is detected
    - The number of beats until game over

    Note that since adding rocks involves randomness, calling [next] on
    two identical gamestates may not result in the same result *)
type gamestate

(** Raised when a certain number of beats have been encountered *)
exception Gameover of int

(** [max_height] is an integer indicating the maximum height of a beat
    in the game. It takes [max_height] number of calls to the [next]
    function in order for a beat to fall from the right to the left and
    then subsequently be removed *)
val max_height : int

(** The type [hit] represents state of the type of the hit per beat

    - Note that the OutOfRange is to not punish players for hitting a
      button when no beats are nearby, intentionally or not *)
type hit =
  | Good
  | Ok
  | Bad
  | OutOfRange

(** [beats] is a list of beats and their locations. They are hard-coded
    beats that are randomly chosen from when calling [add_beat]. The
    reason for being hard-coded is to ensure some kind of pattern in the
    beats to closer simulate the actual game (rather than randomly
    spawning beats with no pattern). *)
val beats : (int * color) list list

(** [init_game ()] is the initial state of the game when playing the
    Drum game. In this state, there are no beats and the drummer is in
    an idle position. *)
val init_game : unit -> gamestate

(** [get_beats gs] returns a list of coordinates that indicate the
    location of the beats in [gs] following the format (color, height).
    Color is of type [color], and indicates if the beat is a "Don" or
    "Ka". Height is a integer between 1 - [max_height] *)
val get_beats : gamestate -> (int * color) list

(** [get_num_beats gs] returns the number of beats left until the game
    is over. *)
val get_num_beats : gamestate -> int

(** [get_combo gs] returns the combo of the player. *)
val get_combo : gamestate -> int

(** [get_score gs] returns the score of the player. *)
val get_score : gamestate -> int

(** [get_beat_type gs] returns the the of beat played by the player. *)
val get_beat_type : gamestate -> beat

(** [process_left gs] returns a new gamestate after the player attempts
    to play a "Ka" beat, meaning that the closest "Ka" beat within
    hitting range gets removed. Additionally, updates score and combo
    accordingly depending on how well-timed the input was. Also animates
    the drummer. *)
val process_left : gamestate -> gamestate

(** [process_middle gs] returns [gs] *)
val process_middle : gamestate -> gamestate

(** [process_right gs] returns a new gamestate after the player attempts
    to play a "Don" beat, meaning that the closest "Don" beat within
    hitting range gets removed. Additionally, updates score and combo
    accordingly depending on how well-timed the input was. Also animates
    the drummer. *)
val process_right : gamestate -> gamestate

(** [next gs] returns a new gamestate where the height of all the beats
    in [gs] is decremented by one.

    Raises: [Gameover] if the number of beats left is 0. *)
val next : gamestate -> gamestate

(** [add_rock gs] randomly selects a beat pattern in [beats] and adds it
    to the game with [max_height + 1]. Note that the beats only enters
    the game simulation when next is called to have consistent behaviour
    \-- all beats fall for the same number of turns *)
val add_beat : gamestate -> gamestate
