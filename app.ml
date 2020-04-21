open Core_kernel
open Async_kernel
open Incr_dom
open Incr.Let_syntax

module Model = struct
  type t = string

  let cutoff = phys_equal
end

module State = struct
  type t = unit
end

module Action = struct
  type t = Nothing.t [@@deriving sexp]
end

let on_startup ~schedule_action:_ _model = Deferred.unit

let create model ~old_model:_ ~inject:_ =
  let%map message = model in
  let apply_action action _ ~schedule_action:_ =
    Nothing.unreachable_code action
  in
  Component.create ~apply_action message @@ Vdom.Node.text message
