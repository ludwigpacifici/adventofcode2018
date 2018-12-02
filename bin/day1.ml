let () =
  let deltas =
    Adventofcode.Common.read_line ~filename:"input/day1.txt"
    |> List.map Core.Int.of_string
  in
  Adventofcode.Day1.part1 ~deltas |> Printf.printf "part 1: %i\n" ;
  Adventofcode.Day1.part2 ~deltas ~repeat_threshold:2
  |> Core.Option.value ~default:0
  |> Printf.printf "part 2: %i\n"
