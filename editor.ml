open Ast
open Core_kernel
open Incr_dom

type t = Ast.t list

let empty = [ Cursor ]

let rec equal buf1 buf2 =
  match (buf1, buf2) with
  | Text s1 :: xs1, Text s2 :: xs2 -> String.equal s1 s2 && equal xs1 xs2
  | Cursor :: xs1, Cursor :: xs2 -> equal xs1 xs2
  | Newline :: xs1, Newline :: xs2 -> equal xs1 xs2
  | [], [] -> true
  | _, _ -> false

let rec at_cursor f head buf =
  match buf with
  | Cursor :: tail -> f head tail
  | x :: xs -> at_cursor f (x :: head) xs
  | [] -> []

let append str buf =
  at_cursor
    (fun head tail ->
      match head with
      | Text s :: xs -> List.rev (Text (s ^ str) :: xs) @ (Cursor :: tail)
      | _ -> List.rev head @ (Text str :: Cursor :: tail))
    [] buf

let backspace buf =
  at_cursor
    (fun head tail ->
      match head with
      | Text s :: xs when String.length s <= 1 -> List.rev xs @ (Cursor :: tail)
      | Text s :: xs ->
          List.rev (Text (String.sub s ~pos:0 ~len:(String.length s - 1)) :: xs)
          @ (Cursor :: tail)
      | Newline :: xs | Cursor :: xs -> List.rev xs @ (Cursor :: tail)
      | [] -> Cursor :: tail)
    [] buf

let newline buf =
  at_cursor
    (fun head tail -> List.rev head @ (Newline :: Cursor :: tail))
    [] buf

let nodes buf =
  let open Vdom in
  List.map buf ~f:(function
    | Text s -> Node.span [ Attr.class_ "text" ] [ Node.text s ]
    | Cursor -> Node.span [ Attr.id "cursor" ] []
    | Newline -> Node.br [])
