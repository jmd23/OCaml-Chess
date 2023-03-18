exception Empty
exception Malformed

type command_phrase = int list

type command =
  | Move of command_phrase
  | Quit

let convert_helper acc str =
  let convert_to_index (mv : char) =
    let code = int_of_char mv in
    if code > 56 then code - 97 else abs (code - 48 - 8)
  in
  List.of_seq (String.to_seq str)
  |> List.filter (fun x -> x <> ' ')
  |> List.map convert_to_index |> List.append acc

let convert_to_indices (lst : string list) : int list =
  let rec inner acc lst =
    match lst with
    | [] -> acc
    | a :: b -> inner (convert_helper acc a) b
  in
  inner [] lst

let split_input (str : string) =
  let split = String.split_on_char ' ' str in
  let rec filter (acc : string list) (strlst : string list) =
    match strlst with
    | [] -> acc
    | a :: b -> if a = "" then filter acc b else filter (acc @ [ a ]) b
  in
  filter [] split

let make_move (strlst : string list) =
  if strlst = [] then raise Malformed else Move (convert_to_indices strlst)

let make_quit (strlst : string list) =
  if strlst = [] then Quit else raise Malformed

let get_command (strlst : string list) =
  match strlst with
  | [] -> raise Empty
  | a :: b ->
      if a = "move" then make_move b
      else if a = "quit" then make_quit b
      else raise Malformed

let parse (str : string) = str |> split_input |> get_command
