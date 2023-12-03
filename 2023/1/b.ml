module SMap = Map.Make (String)

let map =
  SMap.of_list
    [
      ("0", 0);
      ("1", 1);
      ("2", 2);
      ("3", 3);
      ("4", 4);
      ("5", 5);
      ("6", 6);
      ("7", 7);
      ("8", 8);
      ("9", 9);
      ("one", 1);
      ("two", 2);
      ("three", 3);
      ("four", 4);
      ("five", 5);
      ("six", 6);
      ("seven", 7);
      ("eight", 8);
      ("nine", 9);
    ]

let get_digit s =
  let exception Found of int in
  try
    SMap.iter
      (fun k v ->
        if String.starts_with ~prefix:k s then raise (Found v) else ())
      map;
    None
  with Found d -> Some d

let ( >>= ) = Option.bind

let process line =
  let rec loop s len acc =
    if len = 0 then acc
    else
      match (acc, get_digit s) with
      | None, Some d -> loop (String.sub s 1 (len - 1)) (len - 1) (Some (d, d))
      | Some (fst, _), Some d ->
          loop (String.sub s 1 (len - 1)) (len - 1) (Some (fst, d))
      | _ -> loop (String.sub s 1 (len - 1)) (len - 1) acc
  in
  loop line (String.length line) None
  >>= (fun (fst, snd) ->
        Format.printf "%s %d %d@\n" line fst snd;
        Some ((fst * 10) + snd))
  |> Option.get

let () =
  let lines = In_channel.input_lines stdin in
  print_int @@ List.fold_left ( + ) 0 (List.map process lines)
