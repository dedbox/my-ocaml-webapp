open Ast
open Core_kernel
open Incr_dom

type t = Ast.t * Ast.t

let equal (before1, after1) (before2, after2) =
  List.equal Ast.equal before1 before2 && List.equal Ast.equal after1 after2

let empty = ([], [])

let insert str (before, after) =
  match before with
  | [] -> ([ Text str ], after)
  | Newline :: _ -> (Text str :: before, after)
  | Text str0 :: before' -> (Text (str0 ^ str) :: before', after)

let newline (before, after) = (Newline :: before, after)

let nibble = function
  | [] -> []
  | Newline :: xs | Text "" :: xs -> xs
  | Text str :: xs ->
      Text (String.sub str ~pos:0 ~len:(String.length str - 1)) :: xs

let backspace (before, after) = (nibble before, after)

let delete (before, after) = (before, nibble after)

let to_nodes (before, after) =
  let open Vdom in
  let to_node = function
    | Text s -> Node.span [ Attr.class_ "text" ] [ Node.text s ]
    | Newline -> Node.br []
  in
  let nodes_before = List.rev_map before ~f:to_node in
  let cursor_node = Node.span [ Attr.id "cursor" ] [] in
  let nodes_after = List.map after ~f:to_node in
  nodes_before @ (cursor_node :: nodes_after)
