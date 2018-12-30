let () =
  let lines = Adventofcode.Common.read_line ~filename:"input/day6.txt" in
  Adventofcode.Day6.part1 lines |> Printf.printf "part1: %d\n" ;
  Adventofcode.Day6.part2 ~total_distance_threshold:10000 lines
  |> Printf.printf "part2: %d\n"
