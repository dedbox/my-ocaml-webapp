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

let on_startup ~schedule_action _model =
  every (Time_ns.Span.of_sec 1.) (fun () -> schedule_action Action.Increment);
  Deferred.return @@ ref true

let create model ~old_model:_ ~inject =
  let%map counter = model in
  let apply_action action is_active ~schedule_action:_ =
    match action with
    | Action.Increment -> if !is_active then counter + 1 else counter
    | Action.Toggle ->
        is_active := not !is_active;
        counter
  and view =
    let open Vdom in
    Node.div []
      [
        Node.text (Int.to_string counter);
        Node.button
          [ Attr.on_click (fun _ -> inject Action.Toggle) ]
          [ Node.text "toggle" ];
      ]
  in
  Component.create ~apply_action counter view
