let () =
  let frequencies =
    Adventofcode.Common.read_line ~filename:"input/day1.txt"
    |> List.map Core.Int.of_string
  in
  Adventofcode.Day1.part1 frequencies |> Printf.printf "part 1: %i\n" ;
  Adventofcode.Day1.part2 frequencies 2
  |> Adventofcode.Common.unwrap_or ~default:0
  |> Printf.printf "part 2: %i\n"
