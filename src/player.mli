type player =
  | White
  | Black

(* The type representing a player of the game*)

val switch_player : player -> player

(*[switch_player p] switches the turn and returns the new player. If p is White,
  the new player is Black, and vice versa*)

val string_of_player : player -> string

(*[string_of_player p] returns the string representation of player p. *)
