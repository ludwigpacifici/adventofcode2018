let part1 ~deltas = List.fold_left ( + ) 0 deltas

let%test _ = part1 ~deltas:[] = 0

let%test _ = part1 ~deltas:[42] = 42

let%test _ = part1 ~deltas:[-42] = -42

let%test _ = part1 ~deltas:[1; -2; 3; 1] = 3

let%test _ = part1 ~deltas:[1; 1; 1] = 3

let%test _ = part1 ~deltas:[1; 1; -2] = 0

let%test _ = part1 ~deltas:[-1; -2; -3] = -6

let part2 ~deltas ~repeat_threshold =
  if deltas = [] || repeat_threshold <= 0
  then None
  else
    let h = Hashtbl.create 0 in
    let f acc delta =
      match
        Hashtbl.find_opt h acc
        |> Core.Option.value ~default:0
        |> fun i -> i + 1
      with
      | c when c = repeat_threshold ->
          Core.Container.Continue_or_stop.Stop acc
      | c ->
          let () = Hashtbl.replace h acc c in
          Core.Container.Continue_or_stop.Continue (acc + delta)
    in
    Some
      ( deltas
      |> Core.Sequence.cycle_list_exn
      |> Core.Sequence.fold_until ~init:0 ~f ~finish:(fun x -> x) )


let%test _ = part2 ~deltas:[] ~repeat_threshold:0 = None

let%test _ = part2 ~deltas:[] ~repeat_threshold:2 = None

let%test _ = part2 ~deltas:[1; -1] ~repeat_threshold:2 = Some 0

let%test _ = part2 ~deltas:[3; 3; 4; -2; -4] ~repeat_threshold:2 = Some 10

let%test _ = part2 ~deltas:[-6; 3; 8; 5; -6] ~repeat_threshold:2 = Some 5

let%test _ = part2 ~deltas:[7; 7; -2; -7; -4] ~repeat_threshold:2 = Some 14

let%test _ = part2 ~deltas:[1; -2; 3; 1] ~repeat_threshold:2 = Some 2
