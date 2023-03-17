type player =
  | White
  | Black

let switch_player p =
  match p with
  | Black -> White
  | White -> Black
