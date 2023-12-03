let safe_read_int () =
  try
    Some (read_int ())
  with _ ->
    None

let rec get_elf_calories acc =
  match safe_read_int () with
  | None -> if acc = 0 then None else Some acc
  | Some i -> get_elf_calories (acc + i)

let () =
  let rec loop ans =
    match get_elf_calories 0 with
    | None -> ans
    | Some c -> loop (max ans c)
  in
  print_int @@ loop 0
