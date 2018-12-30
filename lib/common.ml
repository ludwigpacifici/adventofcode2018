let read_line ~filename =
  let file = Core.In_channel.create filename in
  Core.In_channel.input_lines file


let ( ||> ) (x, y) f = f x y

let%test _ = (1, 2) ||> ( + ) = 3

let ignore _ = ()
