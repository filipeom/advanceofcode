type level = Init | Increasing | Decresing

type safety = Safe | Unsafe

let pp_safety fmt = function
  | Safe -> Format.pp_print_string fmt "Safe"
  | Unsafe -> Format.pp_print_string fmt "Unsafe"

let _ = pp_safety

type state = {
  mutable level : level;
  mutable prev_level : int option;
  mutable safety : safety;
  mutable bad_levels : int;
}

let reports =
  let lines = In_channel.input_lines stdin in
  List.map
    (fun line ->
      let levels = String.split_on_char ' ' line in
      List.map int_of_string levels)
    lines

let check_report_a levels =
  let result =
    List.fold_left
      (fun st lvl ->
        match st.safety with
        | Unsafe -> st
        | Safe -> (
            match (st.level, st.prev_level) with
            | Init, None ->
                st.prev_level <- Some lvl;
                st
            | _, Some prev_level
              when Int.abs (lvl - prev_level) < 1
                   || Int.abs (lvl - prev_level) > 3 ->
                st.safety <- Unsafe;
                st
            | Init, Some prev_level ->
                let d = lvl - prev_level in
                st.prev_level <- Some lvl;
                st.level <- (if d > 0 then Increasing else Decresing);
                st
            | Increasing, Some prev_level when lvl - prev_level <= 0 ->
                st.safety <- Unsafe;
                st
            | Decresing, Some prev_level when lvl - prev_level >= 0 ->
                st.safety <- Unsafe;
                st.prev_level <- Some lvl;
                st
            | (Increasing | Decresing), _ ->
                st.prev_level <- Some lvl;
                st))
      { level = Init; prev_level = None; safety = Safe; bad_levels = 0 }
      levels
  in
  (* Format.eprintf "%a@." pp_safety result.safety; *)
  match result.safety with
  | Safe -> true
  | Unsafe -> false

let ans_a = List.filter check_report_a reports |> List.length

let ans_b = 0

let () =
  Format.printf "Answer A: %d@." ans_a;
  Format.printf "Answer B: %d@." ans_b
