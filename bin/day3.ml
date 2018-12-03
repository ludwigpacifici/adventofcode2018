let () =
  let lines = Adventofcode.Common.read_line ~filename:"input/day3.txt" in
  Adventofcode.Day3.part1 lines |> Printf.printf "part 1: %i\n" ;
  Adventofcode.Day3.part2 lines
  |> List.iter (fun i -> Printf.printf "part 2: %i\n" i)
