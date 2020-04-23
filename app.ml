open Core_kernel
open Async_kernel
open Incr_dom
open Incr.Let_syntax

module Model = struct
  type t = int

  let cutoff = ( = )
end

module State = struct
  type t = bool ref
end

module Action = struct
  type t = Increment | Toggle [@@deriving sexp]
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
    | Toggle ->
        is_active := not !is_active;
        counter
  and view =
    let open Vdom.Node in
    let open Vdom.Attr in
    div [ on_click (fun _ -> inject Toggle) ] [ text (Int.to_string counter) ]
  in
  Component.create ~apply_action counter view
