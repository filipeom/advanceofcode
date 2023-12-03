let get_digit = function
  | '0' .. '9' as c -> Some (int_of_string @@ Printf.sprintf "%c" c)
  | _ -> None

let ( >>= ) = Option.bind

let process line =
  String.fold_left
    (fun acc c ->
      match (acc, get_digit c) with
      | None, Some d -> Some (d, d)
      | Some (fst, _), Some d -> Some (fst, d)
      | _ -> acc)
    None line
  >>= (fun (fst, snd) -> Some ((fst * 10) + snd))
  |> Option.get

let () =
  let lines = In_channel.input_lines stdin in
  print_int @@ List.fold_left ( + ) 0 (List.map process lines)
