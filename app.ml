open Core_kernel
open Async_kernel
open Incr_dom
open Incr.Let_syntax
module Js = Js_of_ocaml.Js

module Model = struct
  type t = int

  let cutoff = ( = )
end

module State = struct
  type t = bool ref
end

module Action = struct
  type t = Increment | Key of string [@@deriving sexp]
end

open Action

let on_startup ~schedule_action _model =
  every (Time_ns.Span.of_sec 1.) (fun () -> schedule_action Increment);
  Deferred.return @@ ref true

let create model ~old_model:_ ~inject =
  let%map counter = model in
  let apply_action action is_active ~schedule_action:_ =
    match action with
    | Increment -> if !is_active then counter + 1 else counter
    | Key " " ->
        is_active := not !is_active;
        counter
    | Key _ -> counter
  and view =
    let open Vdom.Node in
    let open Vdom.Attr in
    let make_key event =
      Key
        ( match Js.Optdef.to_option event##.key with
        | Some jstr -> Js.to_string jstr
        | None -> "" )
    in
    div
      [ on_keypress (fun event -> inject (make_key event)) ]
      [ text (Int.to_string counter) ]
  in
  Component.create ~apply_action counter view
