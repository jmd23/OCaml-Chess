
(** [convert_to_index mv] converts mv to an index based on the 2D chess board.*)
val convert_to_index : char -> int


(** [parse_input str] parses a user's input [str] into a list of indices that
    correspond to the squares on the board.*)
val parse_input : string -> int list
val handle_response : 'a -> unit
val validate_response : string -> bool
val prompt_user : unit -> unit
