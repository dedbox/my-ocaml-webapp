open Core_kernel
open Async_kernel
open Incr_dom
open Incr.Let_syntax
module Js = Js_of_ocaml.Js

module Model = struct
  type t = Editor.t

  let cutoff = Editor.equal
end

module State = struct
  type t = unit
end

module Action = struct
  type t = Insert of string | Backspace | Newline | Delete | InvalidKey
  [@@deriving sexp]
end

open Action

let on_startup ~schedule_action:_ _model = Deferred.unit

let create model ~old_model:_ ~inject =
  let%map ed = model in
  let apply_action action _state ~schedule_action:_ =
    match action with
    | Insert str -> Editor.insert str ed
    | Backspace -> Editor.backspace ed
    | Newline -> Editor.newline ed
    | Delete -> Editor.delete ed
    | InvalidKey -> ed
  and view =
    let open Vdom.Node in
    let open Vdom.Attr in
    let make_action event =
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event event with
      | Enter -> Newline
      | Delete -> Delete
      | Backspace -> Backspace
      | _ -> (
          match Js.Optdef.to_option event##.key with
          | Some jstr ->
              let str = Js.to_string jstr in
              if String.length str = 1 then Insert str else InvalidKey
          | None -> InvalidKey )
    in
    div
      [ on_keydown (fun event -> inject @@ make_action event) ]
      (Editor.to_nodes ed)
  in
  Component.create ~apply_action ed view
