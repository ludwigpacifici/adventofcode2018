let () =
  let lines = Adventofcode.Common.read_line ~filename:"input/day5.txt" in
  Adventofcode.Day5.part1 lines |> List.iter (Printf.printf "part1: %d\n") ;
  Adventofcode.Day5.part2 lines
  |> fun o ->
  match o with
  | None ->
      Printf.printf "part2: None\n"
  | Some i ->
      Printf.printf "part2: %d\n" i
