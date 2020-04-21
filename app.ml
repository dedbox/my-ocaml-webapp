open Core_kernel
open Async_kernel
open Incr_dom
open Incr.Let_syntax

module Model = struct
  type t = int

  let cutoff = ( = )
end

module State = struct
  type t = unit
end

module Action = struct
  type t = Increment [@@deriving sexp]
end

let on_startup ~schedule_action _model =
  every (Time_ns.Span.of_sec 1.) (fun () -> schedule_action Action.Increment);
  Deferred.unit

let create model ~old_model:_ ~inject:_ =
  let%map counter = model in
  let apply_action action _ ~schedule_action:_ =
    match action with Action.Increment -> counter + 1
  in
  Component.create ~apply_action counter
    (Vdom.Node.text @@ Int.to_string counter)
