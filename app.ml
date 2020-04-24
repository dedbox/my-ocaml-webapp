open Core_kernel
open Async_kernel
open Incr_dom
open Incr.Let_syntax
module Js = Js_of_ocaml.Js

module Model = struct
  type t = Textbuf.t

  let cutoff = Textbuf.equal
end

module State = struct
  type t = unit
end

module Action = struct
  type t = Append of string | Backspace | Newline | InvalidKey
  [@@deriving sexp]
end

open Action

let on_startup ~schedule_action:_ _model = Deferred.unit

let create model ~old_model:_ ~inject =
  let%map buf = model in
  let apply_action action _state ~schedule_action:_ =
    match action with
    | Append str -> Textbuf.append str buf
    | Backspace -> Textbuf.backspace buf
    | Newline -> Textbuf.newline buf
    | InvalidKey -> buf
  and view =
    let open Vdom.Node in
    let open Vdom.Attr in
    let make_action event =
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event event with
      | Enter -> Newline
      | Backspace -> Backspace
      | _ -> (
          match Js.Optdef.to_option event##.key with
          | Some jstr ->
              let str = Js.to_string jstr in
              if String.length str = 1 then Append str else InvalidKey
          | None -> InvalidKey )
    in
    div
      [ on_keydown (fun event -> inject @@ make_action event) ]
      (Textbuf.nodes buf)
  in
  Component.create ~apply_action buf view
