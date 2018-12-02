let read_line ~filename =
  let file = Core.In_channel.create filename in
  Core.In_channel.input_lines file
