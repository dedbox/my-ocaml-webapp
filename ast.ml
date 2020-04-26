type t = t' list

and t' = Text of string | Newline

let equal a b =
  match (a, b) with
  | Newline, Newline -> true
  | Text s1, Text s2 -> String.equal s1 s2
  | _ -> false
