(**)

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

type command_phrase = int list
(** The type [command_phrase] represents the parts of a player's command. A
   command_phrase has only 4 elements. Each element is an index on the board
   where a piece can be or is placed. The first and third elements are converted
   from the letter label of a column and the second and fourth elements are
   converted from the number label of a row. For example:

    - if the player command is ["a2 a7"], then the command_phrase is [\[0;6;0;1\]]
        
    - if the player command is ["h7 e7"], then the command_phrase is [\[7;1;4;1]]    
        *)

(** The type [command] represents a player command that is decomposed into a
    verb and possibly, a command_phrase. Invariant: the [command_phrase] carried
    by [Move] must not be empty*)
type command =
  | Move of command_phrase
  | Redo
  | Undo
  | Quit

val parse : string -> command
(** [parse str] parses a player's input into a [command] as follows. The first
    consecutive sequence of non-space characters of [str] becomes the verb.
    The rest, if any become the command_phrase. Examples:
      
      -  [parse "move a2 a7"] is [Move \[0;6;0;1]]
      -  [parse "quit"] is [Quit].
      -  [parse "redo"] is [Redo]
      -  [parse "undo"] is [Undo]

      Requires: [str] contains only alphanumeric (a-h, 1-8) and space characters
       (only ASCII character code 32; not tabs or newlines, etc).

      Raises: [Empty] if [str] is the empty string or contains only spaces.
      
      Raises: [Malformed] if the command is malformed. A command is malformed if 
        the verb is not "quit", "move","redo" or "undo", or if the verb is "quit","redo" or "undo" and there is a
        non-empty object phrase, or if the verb is "move" and there is an empty object
        phrase. 
    *)
