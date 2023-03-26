type player =
  | White
  | Black

let switch_player p =
  match p with
  | Black -> White
  | White -> Black

let string_of_player p =
  match p with
  | White -> "White"
  | Black -> "Black"
