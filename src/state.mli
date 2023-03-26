type state
(** The abstract type of values representing the game state.. *)

(** The type representing the result of an attempted move. *)
type move_result =
  | Legal of state
  | Illegal_Piece
  | Illegal_Move

(** The type representing the result of an attempted undo. *)
type undo_result =
  | Undone of state
  | Undo_Fail

(** The type representing the result of an attempted redo. *)
type redo_result =
  | Redone of state
  | Redo_Fail

val init_state : unit -> state
(** [init_state ()] is the initial state of the game. In that state, all pieces
    are in their starting positions and the player with the starting turn being
    [Player.White]. There are also no previous boards. *)

val get_current_board : state -> Board.board
(** [get_current_board st] is the current board of a given state. *)

val get_current_player : state -> Player.player
(** [get_current_player st] returns the player whose turn it is to move. *)

val get_all_captured : state -> Board.piece list
(** [get_all_captured st] returns all the pieces that have been captured so far
    in the order in which they were captured.*)

val get_white_captured : state -> Board.piece list
(** [get_white_captured st] returns all the pieces captured by [Player.White] so
    far in the order in which they were captured.*)

val get_black_captured : state -> Board.piece list
(** [get_black_captured st] returns all the pieces captured by [Player.Black] so
    far in the order in which they were captured.*)

val make_move : state -> int list -> move_result
(** [make_move st lst] is the result of attempting to move a piece on the sqaure
    specified by lst[1],lst[0] to a new position specified by lst[3], lst[2].

    If a move is successful, a [Legal st'] is returned where st' is the new
    state after the move. The current_player is then switched.

    If a move is not successful because of an invalid move, a [Illegal_Move] is
    returned.

    If a move is not successful because of an invalid player piece, a
    [Illegal_Piece] is returned. *)

val undo : state -> undo_result
(** [undo state] is the result of attempting to undo a previous move.

    if successful, a [Undone st'] is returned where st' is the state before the
    last move was made.

    If no previous state existed before the current state, a [Undo_Fail] is
    returned. *)

val redo : state -> redo_result
(** [redo state] is the result of attempting to redo a previously undone move.

    if successful, a [Redone st'] is returned where st' is the state before the
    last undo was made.

    If no previously undone state existed before the current state, a
    [Redo_Fail] is returned. *)
