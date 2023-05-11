(* open Player *)

exception Invalid_move
exception Invalid_piece

type piece =
  | Pawn of Player.player
  | Bishop of Player.player
  | Knight of Player.player
  | Rook of Player.player
  | Queen of Player.player
  | King of Player.player

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
  | Pawn p -> "♙"
  | Bishop b -> "♗"
  | Knight n -> "♘"
  | Rook r -> "♖"
  | Queen q -> "♕"
  | King k -> "♔"

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

let color (player : Player.player) (pc : piece) =
  match player with
  | White ->
      ANSITerminal.print_string [ ANSITerminal.white ] (piece_to_string pc)
  | Black ->
      ANSITerminal.print_string [ ANSITerminal.black ] (piece_to_string pc)

let print_color_piece (p : piece) =
  match p with
  | Pawn u -> color u (Pawn u)
  | Knight u -> color u (Knight u)
  | King u -> color u (King u)
  | Queen u -> color u (Queen u)
  | Bishop u -> color u (Bishop u)
  | Rook u -> color u (Rook u)

let print_color_square = function
  | Empty -> print_string " "
  | Piece p -> print_color_piece p

let color_row i (row : square list) =
  let inner x =
    print_color_square x;
    print_string "  | "
  in

  print_endline "  _________________________________________";
  print_string (string_of_int (8 - i) ^ " ");
  print_string "| ";
  List.iter inner row;
  print_endline ""

let print_board (bd : board) =
  print_endline "\n    A    B    C    D    E    F    G    H";
  List.iteri color_row bd

let validate_owner (mover : Player.player) (p : piece) =
  match p with
  | Pawn plr -> mover = plr
  | Knight plr -> mover = plr
  | King plr -> mover = plr
  | Queen plr -> mover = plr
  | Rook plr -> mover = plr
  | Bishop plr -> mover = plr

let square_equal (a : square) (b : square) =
  match (a, b) with
  | Empty, Empty -> true
  | Empty, Piece p -> false
  | Piece p, Empty -> false
  | Piece p1, Piece p2 -> p1 = p2

let row_equal (first : square list) (second : square list) =
  List.equal square_equal first second

let board_equal (board1 : board) (board2 : board) =
  List.equal row_equal board1 board2
