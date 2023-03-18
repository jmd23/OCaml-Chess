open Player

type piece =
  | Pawn of player
  | Bishop of player
  | Knight of player
  | Rook of player
  | Queen of player
  | King of player

type square =
  | Piece of piece
  | Empty

type board = square list list

let rank_1 =
  [
    Piece (Rook White);
    Piece (Knight White);
    Piece (Bishop White);
    Piece (Queen White);
    Piece (King White);
    Piece (Bishop White);
    Piece (Knight White);
    Piece (Rook White);
  ]

let rank_2 =
  [
    Piece (Pawn White);
    Piece (Pawn White);
    Piece (Pawn White);
    Piece (Pawn White);
    Piece (Pawn White);
    Piece (Pawn White);
    Piece (Pawn White);
    Piece (Pawn White);
  ]

let rank_7 =
  [
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
  ]

let rank_8 =
  [
    Piece (Rook Black);
    Piece (Knight Black);
    Piece (Bishop Black);
    Piece (Queen Black);
    Piece (King Black);
    Piece (Bishop Black);
    Piece (Knight Black);
    Piece (Rook Black);
  ]

let empty_rank = [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ]

let piece_to_string = function
  | Pawn p -> if p = White then "♙" else "♟"
  | Bishop b -> if b = White then "♗" else "♝"
  | Knight n -> if n = White then "♘" else "♞"
  | Rook r -> if r = White then "♖" else "♜"
  | Queen q -> if q = White then "♕" else "♛"
  | King k -> if k = White then "♔" else "♚"

let print_square = function
  | Empty -> " "
  | Piece p -> piece_to_string p

let row_to_string i row =
  "   _________________________\n"
  ^ List.fold_left
      (fun x y -> x ^ " |" ^ y)
      (string_of_int (8 - i) ^ " ")
      (List.map (fun p -> print_square p) row)
  ^ " |\n"

let starting_board =
  [
    rank_8;
    rank_7;
    empty_rank;
    empty_rank;
    empty_rank;
    empty_rank;
    rank_2;
    rank_1;
  ]

let board_to_string (b : board) =
  List.fold_left
    (fun x y -> x ^ y)
    "\n   A B C D E F G H\n"
    (List.mapi (fun i x -> row_to_string i x) b)

(*let full_board = board_to_string starting_board |> print_endline*)

let set_row j row value = List.mapi (fun a x -> if a = j then value else x) row

let set_square i j board value =
  List.mapi (fun a x -> if a = i then set_row j x value else x) board

let get_square board i j = List.nth (List.nth board i) j

(*let test = board_to_string new_board |> print_endline*)
