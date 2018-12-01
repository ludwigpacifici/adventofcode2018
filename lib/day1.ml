let part1 fs = List.fold_left ( + ) 0 fs

let%test _ = part1 [] = 0

let%test _ = part1 [42] = 42

let%test _ = part1 [-42] = -42

let%test _ = part1 [1; -2; 3; 1] = 3

let%test _ = part1 [1; 1; 1] = 3

let%test _ = part1 [1; 1; -2] = 0

let%test _ = part1 [-1; -2; -3] = -6

let part2 lines repeat_threshold =
  if lines = [] || repeat_threshold <= 0
  then None
  else
    let frequencies = Array.of_list lines in
    let frequency_count = Array.length frequencies in
    let frequencies_counts = Hashtbl.create frequency_count in
    let rec aux ~fs ~i ~acc ~h =
      match
        Hashtbl.find_opt h acc |> Common.unwrap_or ~default:0 |> fun i -> i + 1
      with
      | c when c = repeat_threshold ->
          acc
      | c ->
          let () = Hashtbl.replace h acc c in
          let acc = acc + fs.(i) in
          let i = (i + 1) mod frequency_count in
          aux ~fs ~i ~acc ~h
    in
    Some (aux ~fs:frequencies ~i:0 ~acc:0 ~h:frequencies_counts)


let%test _ = part2 [] 0 = None

let%test _ = part2 [] 2 = None

let%test _ = part2 [1; -1] 2 = Some 0

let%test _ = part2 [3; 3; 4; -2; -4] 2 = Some 10

let%test _ = part2 [-6; 3; 8; 5; -6] 2 = Some 5

let%test _ = part2 [7; 7; -2; -7; -4] 2 = Some 14

let%test _ = part2 [1; -2; 3; 1] 2 = Some 2
