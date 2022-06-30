(** Compilation state, a simple state monad that simplifying code *)

type ('a, 'b) with_list = 'a * 'b list
type 'a with_cmds = ('a, string Gil_syntax.Cmd.t) with_list
type 'a with_body = ('a, Body_item.t) with_list

let bind ((x, l) : ('a, 'c) with_list) (f : 'a -> ('b, 'c) with_list) :
    ('b, 'c) with_list =
  let x', l' = f x in
  (x', l @ l')

let map (f : 'a -> 'b) ((x, l) : ('a, 'c) with_list) : ('b, 'c) with_list =
  let x' = f x in
  (x', l)

let many (f : 'a -> ('b, 'c) with_list) (x : 'a list) : ('b list, 'c) with_list
    =
  let x', l' = List.map f x |> List.split in
  (x', List.concat l')

let return ?(app = []) (x : 'a) : ('a, 'b) with_list = (x, app)
let unit app = ((), app)
let map_l f (x, l) = (x, List.map f l)

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) x f = map f x
end
