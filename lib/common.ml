let read_line ~filename =
  let file = Core.In_channel.create filename in
  Core.In_channel.input_lines file


let unwrap_or ~default = function None -> default | Some v -> v

let%test _ = unwrap_or ~default:0 None = 0

let%test _ = unwrap_or ~default:0 (Some 42) = 42
