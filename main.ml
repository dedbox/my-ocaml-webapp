open Incr_dom

let () =
  Start_app.start
    (module App)
    ~bind_to_element_with_id:"app" ~initial_model:"Hello, world!"
