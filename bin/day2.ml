let () =
  let lines = Adventofcode.Common.read_line ~filename:"input/day2.txt" in
  Adventofcode.Day2.part1 lines |> Printf.printf "part 1: %i\n" ;
  Adventofcode.Day2.part2 lines
  |> List.iter (fun w -> Printf.printf "part 2: %s\n" w)
