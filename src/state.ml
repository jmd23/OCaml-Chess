type state = {
  past_boards : Board.board list;
  current_board : Board.board;
  current_player : Player.player;
  future_boards : Board.board list;
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
let advance_state (st : state) (brd : Board.board) =
  {
    past_boards = st.past_boards @ [ st.current_board ];
    future_boards = [];
    current_player = Player.switch_player st.current_player;
    current_board = brd;
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

let get_list (pair : 'a list * 'a) =
  match pair with
  | a, _ -> a

let get_popped (pair : 'a list * 'a) =
  match pair with
  | _, b -> b

(** Take a state, switch the players, put current_board into future_board, pop
    the last previous board and assign it to current_board, and update the
    past_boards with the remainder.*)
let undo_state (st : state) =
  let popped = pop st.past_boards in
  let previous_board = get_popped popped in
  let remaining_boards = get_list popped in

  {
    future_boards = st.future_boards @ [ st.current_board ];
    current_player = Player.switch_player st.current_player;
    past_boards = remaining_boards;
    current_board = previous_board;
  }

let redo_state (st:state) =
  let popped = pop st.future_boards in
  let next_board = get_popped popped in
  let remaining_boards = get_list popped in
  {
    current_player = Player.switch_player st.current_player;
    past_boards = st.past_boards @ [st.current_board];
    current_board = next_board;
    future_boards = remaining_boards
  }

(* ================================================================================ *)
let init_state () =
  {
    past_boards = [];
    current_board = Board.starting_board;
    current_player = Player.White;
    future_boards = [];
  }

let get_current_board (st : state) = st.current_board
let get_current_player (st : state) = st.current_player

let make_move (st : state) (lst : int list) =
  try
    let new_board = Board.move_piece st.current_board st.current_player lst in
    let new_state = advance_state st new_board in
    Legal new_state
  with
  | Board.Invalid_move -> Illegal_Move
  | Board.Invalid_piece -> Illegal_Piece

let undo (st : state) =
  if st.past_boards = [] then Undo_Fail else Undone (undo_state st)

let redo (st:state) =
  if st.future_boards = [] then Redo_Fail
  else Redone (redo_state st)