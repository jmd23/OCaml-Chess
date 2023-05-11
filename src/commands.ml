open Str (* Needed for regex in validate_str*)

exception Empty
exception Malformed

type command_phrase = int list

type command =
  | Move of command_phrase
  | Redo
  | Undo
  | Quit

(** [convert_to_index acc s] converts the string [s] to a valid index coordinate
    on the board and appends it to [acc].

    For example [convert_to_index \[\] "a6"] returns [\[0; 2\]].

    Reasoning: "a" is the first column on a chess board and using 0-index based
    counting, "6" is the second row from the bottom up. *)
let convert_helper acc str =
  let convert_to_index (mv : char) =
    let code = int_of_char mv in
    if code > 56 then code - 97 else abs (code - 48 - 8)
  in
  List.of_seq (String.to_seq str)
  |> List.filter (fun x -> x <> ' ')
  |> List.map convert_to_index |> List.append acc

(** [convert_to_indices lst] converts each of strings in [lst] to an index on
    the board.

    For example, [convert_to_index \["a6"; "a4"\]] returns [\[0; 2; 0; 4\]]*)
let convert_to_indices (lst : string list) : int list =
  let rec inner acc lst =
    match lst with
    | [] -> acc
    | a :: b -> inner (convert_helper acc a) b
  in
  inner [] lst

(** [split_input s] returns list of all non-empty substrings in [s] separated by
    a whitespace*)
let split_input (str : string) =
  let split = String.split_on_char ' ' str in
  let rec filter (acc : string list) (strlst : string list) =
    match strlst with
    | [] -> acc
    | a :: b -> if a = "" then filter acc b else filter (acc @ [ a ]) b
  in
  filter [] split

(** [validate_str s] checks whether the string [s] is of the form [a-h][1-8]*)
let validate_str s =
  let rexp = Str.regexp "^[a-hA-H][1-8]$" in
  Str.string_match rexp s 0

(** [make_move lst] makes a [Move] type. Each string in lst is tested to match
    the regex "^[a-hA-H][1-8]$".

    Raises a [Malformed] if [lst] is empty or an element in [lst] fails match *)
let make_move (strlst : string list) =
  if List.length strlst <> 2 then raise Malformed
  else if strlst = [] then raise Malformed
  else if List.fold_left (fun acc s -> acc && validate_str s) true strlst then
    Move (convert_to_indices strlst)
  else raise Malformed

(** [make_quit lst] makes a [Quit] type.

    Raises a [Malformed] if [lst] is not empty*)
let make_quit (strlst : string list) =
  if strlst = [] then Quit else raise Malformed

(** [make_undo lst] makes an [Undo] type.

    Raises a [Malformed] if [lst] is not empty*)
let make_undo (strlst : string list) =
  if strlst = [] then Undo else raise Malformed

(** [make_redo lst] makes an [Redo] type.

    Raises a [Malformed] if [lst] is not empty*)
let make_redo (strlst : string list) =
  if strlst = [] then Redo else raise Malformed

(** [get_command lst] parses the commands in [lst] to their appropriate types

    For example: [get_command \["move"; "a2"; "a4"\]] returns Move
    [command_phrase].

    Raises [Empty] if [lst] is empty

    Raises [Malformed] if the arguments for a command are incorrect. For
    example: [get_command \["quit"; "a2"; "a4"\]] raises [Malformed]*)
let get_command (strlst : string list) =
  match strlst with
  | [] -> raise Empty
  | a :: b ->
      if a = "move" then make_move b
      else if a = "quit" then make_quit b
      else if a = "undo" then make_undo b
      else if a = "redo" then make_redo b
      else raise Malformed

let parse (str : string) = str |> split_input |> get_command

(* let debug (cmd:command) = match cmd with | Move c -> String.concat ","
   (List.map string_of_int c) | _ -> raise Malformed *)
