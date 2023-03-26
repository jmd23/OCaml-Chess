(* After each successful move, either a piece is captured or not. If nothing is
   captured, [None] is appended to state.captured. If a piece is captured, then
   [Some (captured_piece)] is appended to state.captured *)
type state = {
  past_boards : Board.board list;
  current_board : Board.board;
  current_player : Player.player;
  future_boards : Board.board list;
  captured : Board.piece option list;
  future_captured : Board.piece option list;
}

type move_result =
  | Legal of state
  | Illegal_Piece
  | Illegal_Move

type undo_result =
  | Undone of state
  | Undo_Fail

type redo_result =
  | Redone of state
  | Redo_Fail

(* =========================== Helper Functions ============================ *)

(** Returns a new state. *)
let advance_state (st : state) (brd : Board.board)
    (captured : Board.piece option) =
  {
    past_boards = st.past_boards @ [ st.current_board ];
    future_boards = [];
    current_player = Player.switch_player st.current_player;
    current_board = brd;
    captured = st.captured @ [ captured ];
    future_captured = [];
  }

(** remove the last element of a non-empty list and return a pair consisting of
    the remainder of the list and the removed element*)
let pop (lst : 'a list) =
  let rec last_element lst =
    match lst with
    | [] -> failwith "Cannot pop from empty list"
    | [ a ] -> a
    | a :: b -> last_element b
  in
  let remove_last lst =
    match lst with
    | [] -> failwith "Cannot pop from empty list"
    | _ -> lst |> List.rev |> List.tl |> List.rev
  in

  let le = last_element lst in
  let rm = remove_last lst in
  (rm, le)

(** Get the first element in a pair. [get_first (a , b)] Returns a*)
let get_first pair =
  match pair with
  | a, _ -> a

(** Get the second element in a pair. [get_second (a , b)] Returns b*)
let get_second pair =
  match pair with
  | _, b -> b

(** Take a state, switch the players, put current_board into future_boards, pop
    the last previous board and assign it to current_board, and update the
    past_boards with the remainder.*)
let undo_state (st : state) =
  let popped_board = pop st.past_boards in
  let previous_board = get_second popped_board in
  let remaining_boards = get_first popped_board in
  let popped_captured = pop st.captured in
  let previous_captured = get_second popped_captured in
  let remaining_captureds = get_first popped_captured in

  {
    future_boards = st.future_boards @ [ st.current_board ];
    current_player = Player.switch_player st.current_player;
    past_boards = remaining_boards;
    current_board = previous_board;
    captured = remaining_captureds;
    future_captured = st.future_captured @ [ previous_captured ];
  }

(** Take a state, switch the players, put current_board into past_boards, pop
    the last future board and assign it to current board, and update the future
    boards with the remainder. *)
let redo_state (st : state) =
  let popped_board = pop st.future_boards in
  let next_board = get_second popped_board in
  let remaining_boards = get_first popped_board in

  let popped_captured = pop st.future_captured in
  let next_captured = get_second popped_captured in
  let remaining_captureds = get_first popped_captured in

  {
    current_player = Player.switch_player st.current_player;
    past_boards = st.past_boards @ [ st.current_board ];
    current_board = next_board;
    future_boards = remaining_boards;
    captured = st.captured @ [ next_captured ];
    future_captured = remaining_captureds;
  }

(** Return true if a piece belongs to [Player.White]*)
let is_white_piece (p : Board.piece) =
  match p with
  | Board.Pawn plr -> plr = Player.White
  | Board.King plr -> plr = Player.White
  | Board.Bishop plr -> plr = Player.White
  | Board.Knight plr -> plr = Player.White
  | Board.Rook plr -> plr = Player.White
  | Board.Queen plr -> plr = Player.White

(* ================================================================================ *)
let init_state () =
  {
    past_boards = [];
    current_board = Board.starting_board;
    current_player = Player.White;
    future_boards = [];
    captured = [];
    future_captured = [];
  }

let get_current_board (st : state) = st.current_board
let get_current_player (st : state) = st.current_player

let make_move (st : state) (lst : int list) =
  try
    let move = Movement.move st.current_board st.current_player lst in
    match move with
    | Movement.Normal brd -> Legal (advance_state st brd None)
    | Movement.Captured pair ->
        Legal (advance_state st (get_first pair) (Some (get_second pair)))
  with
  | Movement.Invalid_move -> Illegal_Move
  | Movement.Invalid_piece -> Illegal_Piece

let undo (st : state) =
  if st.past_boards = [] then Undo_Fail else Undone (undo_state st)

let redo (st : state) =
  if st.future_boards = [] then Redo_Fail else Redone (redo_state st)

let get_all_captured (st : state) =
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | a :: b ->
        if Option.is_some a then inner b (acc @ [ Option.get a ])
        else inner b acc
  in
  inner st.captured []

let get_black_captured (st : state) =
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | a :: b ->
        if Option.is_some a && is_white_piece (Option.get a) then
          inner b (acc @ [ Option.get a ])
        else inner b acc
  in
  inner st.captured []

let get_white_captured (st : state) =
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | a :: b ->
        if Option.is_some a && not (is_white_piece (Option.get a)) then
          inner b (acc @ [ Option.get a ])
        else inner b acc
  in
  inner st.captured []
