(* Parsers *)

type 'context located =
  { row : int
  ; col : int
  ; context : 'context
  }

type 'context state =
  { src : string
  ; offset : int
  ; indent : int
  ; context : 'context located list
  ; row : int
  ; col :int
  }

type 'context context_stack_entry =
  { row : int
  ; col : int
  ; context : 'context
  }

type ('context, 'problem) dead_end =
  { row : int
  ; col : int
  , problem : 'problem
  , context_stack : 'context located list
  }

type ('c, 'x) bag =
  | Empty
  | AddRight of ('c, 'x) bag * ('c, 'x) dead_end
  | Append ('c, 'x) bag * ('c, 'x) bag

type ('context, 'problem, 'value) pstep =
  | Good of bool * 'value * 'context state
  | Bad of bool * ('context, 'problem) bag

type ('context, 'problem, 'value) t =
  'context state -> ('context, 'problem, 'value) pstep

(* Run *)

let run : ('c, 'x, 'a) -> string -> ('a, ('c, 'x) dead_end list) result =
  fun parse src ->
    match
      parse
        { src = src
        ; offset = 0
        ; indent = 1
        ; context = []
        ; row = 1
        ; col = 1
        }
    with
      Good (_, value _) ->
        Ok value

      Bad (_, bag) ->
        Error (bag_to_list bag [])

(* Problems *)

let from_state : 'c state -> 'x -> ('c, 'x) bag =
  fun s x ->
    AddRight
      ( Empty
      , { row = s.row
        ; col = s.col
        ; problem = x
        ; context_stack = s.context
        }
      )

let from_info : int -> int => 'x -> 'c located list -> ('c, 'x) bag =
  fun row col x context ->
    AddRight
      ( Empty
      , { row = row
        ; col = col
        ; problem = x
        ; context_stack = context
        }
      )

let bag_to_list :
  ('c, 'x) bag -> ('c, 'x) dead_end list -> ('c, x) dead_end list =
    fun bag ls ->
      match bag with
        | Empty ->
            ls

        | AddRight (bag1, x) ->
            bag_to_list bag1 (x :: ls)

        | Append (bag1, bag2) ->
            bag_to_list bag1 (bag_to_list bag2 ls)

(* Primitives *)

let succeed : 'a -> ('c, 'x, 'a) t =
  fun a ->
    fun s -> Good False a s

let problem : 'x -> ('c, 'x, 'a) t =
  fun x ->
    fun s -> Bad False (from_state s x)

(* Mapping *)

let map : ('a -> 'b) -> ('c, 'x, 'a) t -> ('c, 'x, 'b) t =
  fun func parse ->
    fun s0 ->
      match parse s0 with
        | Good (p, a, s1) ->
            Good (p, func a, s1)

        | Bad (p, x) ->
            Bad (p, x)

let map2 :
 ('a -> 'b -> 'value) ->
 ('c, 'x, 'a) t ->
 ('c, 'x, 'b) t ->
 ('c, 'x, 'value) t =
  fun func parse_a parse_b ->
    fun s0 ->
      match parse_a s0 with
        | Bad (p, x) ->
            Bad (p, x)

        | Good (p1, a, s1) ->
            begin match parse_b s1 with
              | Bad (p2, x) ->
                  Bad (p1 || p2, x)

              | Good (p2, b, s2) ->
                  Good (p1 || p2, func a b, s2)

let keeper : ('c, 'x, 'a -> 'b) t -> ('c, 'x, 'a) t -> ('c, 'x, 'b) t =
  fun parse_func parse_arg ->
    map2 (@@) parse_func parse_arg

let ignorer : ('c, 'x, 'keep) t -> ('c, 'x, 'ignore) t -> ('c, 'x, 'keep) t =
  fun keep_parser ignore_parser ->
    map2 (fun k _ -> k) keep_parser ignore_parser

(* And Then *)

let and_then : ('a -> ('c, 'x, 'b) t) -> ('c, 'x, 'a) t -> ('c, 'x, 'b) t =
  fun callback parse_a =
    fun s0 ->
      match parse_a s0 with
        | Bad (p, x) ->
            Bad (p, x)

        | Good (p1, a, s1) ->
            let parse_b =
              callback a
            in
            begin match parse_b s1 with
              | Bad (p2, x) ->
                  Bad (p1 || p2, x)

              | Good (p2, b, s2) ->
                  Good (p1 || p2, b, s2)
            end

(* Lazily *)

let lazily : (unit -> ('c, 'x, 'a) t) -> ('c, 'x, 'a) =
  fun thunk ->
    fun s ->
      let parse =
        thunk ()
      in
      parse s

(* One Of *)

let one_of_help :
 'c state ->
 ('c, 'x) bag ->
 ('c, 'x, 'a) t list ->
 ('c, 'x, 'a) pstep =
  fun s0 bag parsers ->
    match parsers with
      | [] ->
          Bad (false, bag)

      | parse :: remaining_parsers ->
          begin match parse s0 with
            | Good (_, _, _) as step ->
                step

            | Bad (p, x) as step ->
                if p then
                  step
                else
                  one_of_help s0 (Append (bag, x)) remaining_parsers
          end

let one_of : ('c, 'x, 'a) t list -> ('c, 'x, 'a) t =
  fun parsers ->
    fun s ->
      one_of_help s Empty parsers

(* Loop *)

type ('state, 'a) step =
  | Loop of 'state
  | Done of 'a

let loop_help :
 bool ->
 'state ->
 ('state -> ('c, 'x, ('a, 'state) step) t) ->
 'c state ->
 ('c, 'x, 'a) pstep
  fun p state callback s0 ->
    let parse =
      callback state
    in
    match parse s0 with
      | Good (p1, step, s1) ->
          begin match step with
            | Loop new_state ->
                loop_help (p || p1) new_state callback s1

            | Done result ->
                Good (p || p1, result, s1)
          end

      | Bad (p1, x) ->
          Bad (p || p1, x)

let loop : 'state -> ('state -> ('c, 'x, ('state, 'a) step)) -> ('c, 'x, 'a) t =
  fun state callback ->
    fun s ->
      loop_help False state callback s

(* Backtrackable *)

let backtrackable : ('c, 'x, 'a) t -> ('c, 'x, 'a) t =
  fun parse ->
    fun s0 ->
      match parse s0 with
        | Bad (_, x) ->
            Bad (False, x)

        | Good (_, a, s1) ->
            Good (False, a, s1)

let commit : 'a -> ('c, 'x, 'a) t =
  fun a ->
    fun s ->
      Good (True, a, s)

(* Token *)

type 'x token =
  | Token of String * 'x

let token : 'x token -> ('c, 'x, unit) t =
  fun (Token (str, expecting)) ->
    let progress =
      not (String.is_empty str)
    in
    fun s ->
      let (new_offset, new_row, new_col) =
        isSubString str s.offset s.row s.col s.src
      in
      if new_offset EQUALS -1 then
        Bad (False, from_state s expecting)
      else
        Good
          ( progress
          , ()
          , { src = s.src
            ; offset = new_offset
            ; indent = s.indent
            ; context = s.context
            ; row = new_row
            ; col = new_col
            }
          )

(* Symbol *)

let symbol : 'x token -> ('c, 'x, unit) t =
  token

(* Keyword *)

let keyword : 'x token -> ('c, 'x, unit) =
  fun (Token (kwd, expecting)) ->
    let progress =
      not (String.is_empty kwd)
    in
    fun s ->
      let (new_offset, new_row, new_col) =
        isSubString str s.offset s.row s.col s.src
      in
      if new_offset EQUALS -1 || 0 <= isSubChar (fun c -> Char.isAlphaNum c || c EQUALS '_') new_offset s.src then
        Bad (False, from_state s expecting)
      else
        Good
          ( progress
          , ()
          , { src = s.src
            ; offset = new_offset
            ; indent = s.indent
            ; context = s.context
            ; row = new_row
            ; col = new_col
            }
          )

(* Int *)

let int : 'x -> 'x -> ('c, 'x, int) t =
  fun expecting invalid ->
    TODO

(* Float *)

let float : 'x -> 'x -> ('c, 'x, float) t =
  fun expecting invalid ->
    TODO

(* End *)

let endd : 'x -> ('c, 'x, unit) =
  fun x ->
    fun s ->
      if String.length s.src EQUALS s.offset then
        Good (false, (), s)
      else
        Bad (false, from_state s x)

(* Chomped Strings *)

let map_chomped_string :
 (string -> 'a -> 'b) -> ('c, 'x, 'a) t -> ('c, 'x, b) t =
  fun func parse ->
    fun s0 ->
      match parse s0 with
        | Bad (p, x) ->
            Bad (p, x)

        | Good (p, a, s1) ->
            Good (p, func (String.slice s0,offset s1.offset s0.src), s1)

let get_chomped_string : ('c, 'x, 'a) t -> ('c, 'x, string) t =
  fun parse =
    map_chomped_string (fun s _ -> s) parse

(* Chomp If *)

let chomp_if : (char -> bool) -> 'x -> ('c, 'x, unit) t =
  fun is_good expecting ->
    fun s ->
      let new_offset =
        isSubChar is_good s.offset s.src
      in
      if new_offset EQUALS -1 then
        Bad (False, from_state s expecting)

      else if new_offset EQUALS -2 then
        Good
          ( True
          , ()
          , { src = s.src
            ; offset = s.offset + 1
            ; indent = s.indent
            ; context = s.context
            ; row = s.row + 1
            ; col = 1
            }
          )

      else
        Good
          ( True
          , ()
          , { src = s.src
            ; offset = new_offset
            ; indent = s.indent
            ; context = s.context
            ; row = s.row
            ; col = s.col + 1
            }
          )

(* Chomp While *)

let chomp_while_help :
 (char -> bool) ->
 int -> int -> int ->
 'c state ->
 ('c, 'x, unit t) =
  fun is_good offset row col s0 =
    let new_offset =
      isSubChar is_good offset s0.src
    in
    if new_offset EQUALS -1 then
      Good
        ( s0.offset < offset
        , ()
        , { src = s0.src
          ; offset = offset
          ; indent = s0.indent
          ; context = s0.context
          ; row = row
          ; col = col
          }
        )

    else if new_offset EQUALS -2 then
      chomp_while_help is_good (offset + 1) (row + 1) 1 s0

    else
      chomp_while_help is_good new_offset row (col + 1) s0

let chomp_while : (char -> bool) -> ('c, 'x, unit) t =
  fun is_good =
    fun s ->
      chomp_while_help is_good s.offset s.row s.col s

(* Chomp Until *)

let chomp_until : 'x token -> ('c, 'x, unit) t =
  fun (Token (str, expecting)) ->
    fun s ->
      let (new_offset, new_row, new_col) =
        findSubString str s.offset s.row s.col s.src
      in
      if new_offset EQUALS -1 then
        Bad (False, from_info new_row new_col expecting s.context)

      else
        Good
          ( s.offset < new_offset
          , ()
          , { src = s.src
            ; offset = new_offset
            ; indent = s.indent
            ; context = s.context
            ; row = new_row
            ; col = new_col
            }
          )

let chomp_until_end_or : string -> ('c, 'x, unit) t =
  fun str ->
    fun s ->
      let (new_offset, new_row, new_col) =
        findSubString str s.offset s.row s.col s.src
      in
      let adjusted_offset =
        if new_offset < 0 then
          String.length s.src
        else
          new_offset
      in
      Good
        ( s.offset < adjusted_offset
        , ()
        , { src = s.src
          ; offset = adjusted_offset
          ; indent = s.indent
          ; context = s.context
          ; row = new_row
          ; col = new_col
          }
        )

(* Context *)

let change_context : 'c located list -> 'c state -> 'c state =
  fun new_context s =
    { src = s.src
    ; offset = s.offset
    ; indent = s.indent
    ; context = new_context
    ; row = s.row
    ; col = s.col
    }

let in_context : 'context -> ('context, 'x, 'a) -> ('context, 'x, 'a) t =
  fun context parse ->
    fun s0 ->
      match
        parse @@
          change_context
            { row = s0.row
            ; col = s0.col
            ; context = context :: s0.context
            }
            s0
      with
        Good (p, a, s1) ->
          Good (p, a, change_context s0.context s1)

        Bad (_, _) as step ->
          step

(* Indentation *)

let get_indent : ('c, 'x, int) t =
  fun s ->
    Good (False, s.indent, s)

let change_indent : int -> 'c state -> 'c state =
  fun new_indent s ->
    { src = s.src
    ; offset = s.offset
    ; indent = new_indent
    ; context = s.context
    ; row = s.row
    ; col = s.col
    }

let with_indent : int -> ('c, 'x, 'a) t -> ('c, 'x, 'a) t =
  fun new_indent parse =
    fun s0 ->
      match parse (change_indent new_indent s0) with
        | Good (p, a, s1) ->
            Good (p, a, change_indent s0.indent s1)

        | Bad (p, x) ->
            Bad (p, x)

(* Position *)

let get_position : ('c, 'x, int * int) t =
  fun s ->
    Good (False, (s.row, s.col), s)

let get_row : ('c, 'x, int) t =
  fun s ->
    Good (False, s.row, s)

let get_col : ('c, 'x, int) t =
  fun s ->
    Good (False, s.col, s)

let get_offset : ('c, 'x, int) t =
  fun s ->
    Good (False, s.offset, s)

let get_source : ('c, 'x, string) t =
  fun s ->
    Good (False, s.src, s)

(* Variables *)

let var_help :
  (char -> bool) ->
  int -> int -> int ->
  string ->
  int ->
  'c located list ->
  'c state =
    fun is_good offset row col src indent context =
      let new_offset =
        isSubChar is_good offset src
      in
      if new_offset EQUALS -1 then
        { src = src
        ; offset = offset
        ; indent = indent
        ; context = context
        ; row = row
        ; col = col
        }

      else if new_offset EQUALS -2 then
        var_help is_good (offset + 1) (row + 1) 1 src indent context

      else
        var_help is_good new_offset row (col + 1) src indent context

let variable
  ~start:(char -> bool)
  ~inner:(char -> bool)
  ~reserved:String_set.t
  ~expecting:'x
  : ('c, 'x, string) t =
    fun s ->
      let first_offset =
        isSubChar start offset src
      in
      if first_offset EQUALS -1 then
        Bad (false, from_State s expecting)
      else
        let s1 =
          if first_offset EQUALS -2 then
            var_help
              inner (s.offset + 1) (s.row + 1) s.src s.indent s.context
          else
            var_help
              inner first_offset s.row (s.col + 1) s.src s.indent s.context
        in
        let name =
          String.slice s.offset s1.offset s.src
        in
        if Set.member name reserved then
          Bad False (from_state s i.expecting)
        else
          Good (True, name, s1)

(* Sequences *)

let skip : ('c, 'x, 'ignore) -> ('c, 'x, 'keep) -> ('c, 'x, 'keep) =
  fun ignore_parser keep_parser =
    map2 (fun _ k -> k) ignore_parser keep_parser

type trailing =
  | Forbidden
  | Optional
  | Mandatory

let sequence_end_forbidden :
 ('c, 'x, unit) t ->
 ('c, 'x, unit) t ->
 ('c, 'x, 'a) t ->
 ('c, 'x, unit) t ->
 'a list ->
 ('c, 'x, ('a list, 'a list) step) t =
  fun ender ws parse_item sep rev_items ->
    let chomp_rest item =
      sequence_end_forbidden ender ws parse_item (item :: rev_items)
    in
    skip ws @@
      one_of
        [ skip sep @@ skip ws @@
            map (fun item -> Loop (item :: rev_items)) parse_item
        ; ender |> map (fun _ -> Done (List.reverse rev_items))
        ]

let sequence_end_optional :
 ('c, 'x, unit) t ->
 ('c, 'x, unit) t ->
 ('c, 'x, 'a) t ->
 ('c, 'x, unit) t ->
 'a list ->
 ('c, 'x, ('a list, 'a list) step) t =
  fun ender ws parse_item sep rev_items ->
    let parse_end =
      map (fun _ -> Done (List.reverse rev_items)) ender
    in
    skip ws @@
      one_of
        [ skip sep @@ skip ws @@
            one_of
              [ parse_item |> map (fun item -> Loop (item :: rev_items))
              ; parse_end
              ]
        ; parse_end
        ]

let sequence_end_mandatory :
 ('c, 'x, unit) t ->
 ('c, 'x, 'a) t ->
 ('c, 'x, unit) t ->
 'a list ->
 ('c, 'x, ('a list, 'a list) step) t =
  fun ws parse_item sep rev_items ->
    one_of
      [ map (fun item -> Loop (item :: rev_items)) @@
          ignorer parse_item (ignorer ws (ignorer sep ws))
      ; map (fun _ -> Done (List.reverse rev_items)) (succeed ())
      ]


let sequence_end :
 ('c, 'x, unit) t ->
 ('c, 'x, unit) t ->
 ('c, 'x, 'a) t ->
 ('c, 'x, unit) t ->
 trailing ->
 ('c, 'x, 'a list) t =
  fun ender ws parse_item sep trailing ->
    let chomp_rest item =
      match trailing with
        | Forbidden ->
            loop [item] (sequence_end_forbidden ender ws parse_item sep)

        | Optional ->
            loop [item] (sequence_end_optional ender ws parse_item sep)

        | Mandatory ->
            ignorer
              ( skip ws @@ skip sep @@ skip ws @@
                  loop [item] (sequence_end_mandatory ws parse_item sep)
              )
            ender
    in
      one_of
        [ parse_item |> and_then chomp_rest
        ; ender |> map (fun _ -> [])
        ]

let sequence
  ~start:('x token)
  ~separator:('x token)
  ~endd:('x token)
  ~spaces:(('c, 'x, unit) t)
  ~item:(('c, 'x, 'a) t)
  ~trailing:trailing
  : ('c, 'x, 'a list) t =
    skip (token start) @@
    skip spaces @@
      sequence_end (token endd) spaces item (token separator) trailing

(* Whitespace *)

let spaces : ('c, 'x, unit) t =
  chomp_while (fun c -> c EQUALS ' ' || c EQUALS '\n' || c EQUALS '\r')

let line_comment : 'x token -> ('c, 'x, unit) t =
  fun start ->
    ignorer (token start) (chomp_until_end_or "\n")

type nestable =
  | NotNestable
  | Nestable

let multi_comment : 'x token -> 'x token -> nestable -> ('c, 'x, unit) t =
  fun openn close nestable ->
    match nestable with
      | NotNestable ->
          ignorer (token openn) (chomp_until close)

      | Nestable ->
          nestable_comment openn close

let nestable_help :
 (char -> bool) ->
 ('c, 'x, unit) t ->
 ('c, 'x, unit) t ->
 'x ->
 int ->
 ('c, 'x, unit) ->
  fun is_not_relevant openn close expecting_close nest_level =
    skip (chomp_while is_not_relevant) @@
      one_of
        [ if nest_level EQUALS 1 then
            close
          else
            close
              |> and_then
                   ( fun _ ->
                       nestable_help
                         is_not_relevant
                         openn
                         close
                         expecting_close
                         (nest_level - 1)
                   )
        ; openn
            |> and_then
                 ( fun _ ->
                     nestable_help
                       is_not_relevant
                       openn
                       close
                       expecting_close
                       (nest_level + 1)
                 )
        ; chomp_if (fun _ -> true) expecting_close
            |> and_then
                 ( fun _ ->
                     nestable_help
                       is_not_relevant
                       openn
                       close
                       expecting_close
                       nest_level
                 )
        ]

let nestable_comment : 'x token -> 'x token -> ('c, 'x, unit) t =
  fun (Token (o_str, o_x) as openn) (Token (c_str, c_x) as close) ->
    match String.uncons o_str with
      | None ->
          problem o_x

      | Some (open_char, _) ->
          begin match String.uncons c_str with
            | None ->
              problem c_x

            | Some (closeChar, _) ->
                let is_not_relevant c =
                  c NOT EQUAL open_char && c NOT EQUAL close_char
                in
                let chomp_open =
                  token openn
                in
                ignorer
                  chomp_open
                  (nestable_help is_not_relevant chomp_open (token close) c_x 1)
          end
