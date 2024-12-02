let input =
  let lines = In_channel.input_lines stdin in
  let lines = Array.of_list lines in
  Array.map (fun line -> Scanf.sscanf line "%d %d " (fun a b -> (a, b))) lines

let ans_a =
  let a, b = Array.split input in
  let () = Array.fast_sort compare a in
  let () = Array.fast_sort compare b in
  let dist = ref 0 in
  for i = 0 to Array.length a - 1 do
    dist := !dist + Int.abs (a.(i) - b.(i))
  done;
  !dist

let ans_b =
  let a, b = Array.split input in
  let hist = Hashtbl.create 256 in
  Array.iter
    (fun i ->
      let count =
        match Hashtbl.find hist i with exception Not_found -> 0 | i -> i
      in
      Hashtbl.replace hist i (count + 1))
    b;
  Array.fold_left
    (fun acc v ->
      let count =
        match Hashtbl.find hist v with exception Not_found -> 0 | i -> i
      in
      acc + (v * count))
    0 a

let () =
  Format.printf "Answer A: %d@." ans_a;
  Format.printf "Answer B: %d@." ans_b
