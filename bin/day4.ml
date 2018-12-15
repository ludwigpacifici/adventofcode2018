let () =
  let lines = Adventofcode.Common.read_line ~filename:"input/day4.txt" in
  Adventofcode.Day4.part1 lines |> Printf.printf "part1: %d\n" ;
  Adventofcode.Day4.part2 lines |> Printf.printf "part2: %d\n"
