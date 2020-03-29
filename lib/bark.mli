(* Parsers *)

type ('context, 'problem, 'value) t

type ('context, 'problem) dead_end

val run : ('c, 'x, 'a) t -> string -> ('a, ('c, 'x) dead_end list) result

val in_context : 'context -> ('context, 'x, 'a) t -> ('context, 'x, 'a) t

type 'x token =
  | Token of string * 'x

(* Building blocks *)

val is_alpha : char -> bool
val is_num : char -> bool

val int : 'x -> ('c, 'x, int) t
val float : 'x -> 'x -> ('c, 'x, float) t

val symbol : 'x token -> ('c, 'x, unit) t
val keyword : 'x token -> ('c, 'x, unit) t

module String_set : sig
  include Set.S with type elt = string
end

val variable :
  start:(char -> bool) ->
  inner:(char -> bool) ->
  reserved:String_set.t ->
  expecting:'x ->
    ('c, 'x, string) t

val endd : 'x -> ('c, 'x, unit) t

(* Pipelines *)

val succeed : 'a -> ('c, 'x, 'a) t

val (|=) : ('c, 'x, 'a -> 'b) t -> ('c, 'x, 'a) t -> ('c, 'x, 'b) t
val (|.) : ('c, 'x, 'keep) t -> ('c, 'x, 'ignore) t -> ('c, 'x, 'keep) t

val lazily : (unit -> ('c, 'x, 'a) t) -> ('c, 'x, 'a) t

val and_then : ('a -> ('c, 'x, 'b) t) -> ('c, 'x, 'a) t -> ('c, 'x, 'b) t

val problem : 'x -> ('c, 'x, 'a) t

(* Branches *)

val one_of : ('c, 'x, 'a) t list -> ('c, 'x, 'a) t

val map : ('a -> 'b) -> ('c, 'x, 'a) t -> ('c, 'x, 'b) t

val backtrackable : ('c, 'x, 'a) t -> ('c, 'x, 'a) t
val commit : 'a -> ('c, 'x, 'a) t

val token : 'x token -> ('c, 'x, unit) t

(* Loops *)

type trailing =
  | Forbidden
  | Optional
  | Mandatory

val sequence :
  start:('x token) ->
  separator:('x token) ->
  endd:('x token) ->
  spaces:(('c, 'x, unit) t) ->
  item:(('c, 'x, 'a) t) ->
  trailing:trailing ->
    ('c, 'x, 'a list) t

type ('state, 'a) step =
  | Loop of 'state
  | Done of 'a

val loop : 'state -> ('state -> ('c, 'x, ('state, 'a) step) t) -> ('c, 'x, 'a) t

(* Whitespace *)

val spaces : ('c, 'x, unit) t

val line_comment : 'x token -> ('c, 'x, unit) t

type nestable =
  | NotNestable
  | Nestable

val multi_comment : 'x token -> 'x token -> nestable -> ('c, 'x, unit) t

(* Chompers *)

val get_chomped_string : ('c, 'x, 'a) t -> ('c, 'x, string) t

val chomp_if : (char -> bool) -> 'x -> ('c, 'x, unit) t
val chomp_while : (char -> bool) -> ('c, 'x, unit) t
val chomp_until : 'x token -> ('c, 'x, unit) t
val chomp_until_end_or : string -> ('c, 'x, unit) t

val map_chomped_string :
  (string -> 'a -> 'b) -> ('c, 'x, 'a) t -> ('c, 'x, 'b) t

val with_indent : int -> ('c, 'x, 'a) t -> ('c, 'x, 'a) t

(* Indentation *)

val get_indent : ('c, 'x, int) t
val get_position : ('c, 'x, int * int) t

(* Positions *)

val get_row : ('c, 'x, int) t
val get_col : ('c, 'x, int) t
val get_offset : ('c, 'x, int) t
val get_source : ('c, 'x, string) t

(* Syntax *)

module Syntax : sig
  val ( let+ ) : ('c, 'x, 'a) t -> ('a -> 'b) -> ('c, 'x, 'b) t
  val ( let* ) : ('c, 'x, 'a) t -> ('a -> ('c, 'x, 'b) t) -> ('c, 'x, 'b) t
end
