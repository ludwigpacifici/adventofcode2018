module CharIntMap = Map.Make (Char)

let repitition_count ~n ~word =
  let char_count word =
    let f acc c =
      CharIntMap.update
        c
        (Core.Option.value_map ~default:(Some 1) ~f:(fun x -> Some (x + 1)))
        acc
    in
    Core.String.fold word ~init:CharIntMap.empty ~f
  in
  let counts = char_count word in
  let folder _ count acc = if count = n then acc + 1 else acc in
  CharIntMap.fold folder counts 0


let%test _ =
  repitition_count ~n:2 ~word:"" = 0 && repitition_count ~n:3 ~word:"" = 0

let%test _ =
  repitition_count ~n:2 ~word:"abcdef" = 0
  && repitition_count ~n:3 ~word:"abcdef" = 0

let%test _ =
  repitition_count ~n:2 ~word:"bababc" = 1
  && repitition_count ~n:3 ~word:"bababc" = 1

let%test _ =
  repitition_count ~n:2 ~word:"abbcde" = 1
  && repitition_count ~n:3 ~word:"abbcde" = 0

let%test _ =
  repitition_count ~n:2 ~word:"abcccd" = 0
  && repitition_count ~n:3 ~word:"abcccd" = 1

let%test _ =
  repitition_count ~n:2 ~word:"aabcdd" = 2
  && repitition_count ~n:3 ~word:"aabcdd" = 0

let%test _ =
  repitition_count ~n:2 ~word:"abcdee" = 1
  && repitition_count ~n:3 ~word:"abcdee" = 0

let%test _ =
  repitition_count ~n:2 ~word:"ababab" = 0
  && repitition_count ~n:3 ~word:"ababab" = 2

let part1 lines =
  let checksum = ( * ) in
  let f (twos, threes) word =
    let consume n = if repitition_count ~n ~word > 0 then 1 else 0 in
    (twos + consume 2, threes + consume 3)
  in
  let open Common in
  List.fold_left f (0, 0) lines ||> checksum


let%test _ = part1 [] = 0

let%test _ =
  part1 ["abcdef"; "bababc"; "abbcde"; "abcccd"; "aabcdd"; "abcdee"; "ababab"]
  = 12

let diff w1 w2 =
  let f acc c1 c2 = if c1 <> c2 then (c1, c2) :: acc else acc in
  Core.List.fold2_exn w1 w2 ~init:[] ~f


let%test _ =
  diff ['a'; 'b'; 'c'; 'd'; 'e'] ['a'; 'x'; 'c'; 'y'; 'e']
  = [('d', 'y'); ('b', 'x')]

let%test _ =
  diff ['f'; 'g'; 'h'; 'i'; 'j'] ['f'; 'g'; 'u'; 'i'; 'j'] = [('h', 'u')]

let part2 lines =
  let rebuild_string_without_diff (w1, w2) =
    Core.List.fold2_exn
      w1
      w2
      ~f:(fun acc c1 c2 -> if c1 = c2 then c1 :: acc else acc)
      ~init:[]
    |> Core.String.of_char_list
    |> Core.String.rev
  in
  let lines = lines |> Core.List.map ~f:Core.String.to_list in
  Core.List.cartesian_product lines lines
  |> List.filter (fun (w1, w2) ->
         if diff w1 w2 |> List.length = 1 then true else false )
  |> List.map rebuild_string_without_diff


let%test _ = part2 [] = []

let%test _ = part2 ["a"] = []

let%test _ = part2 ["abcde"; "axcye"; "fghij"; "fguij"] = ["fgij"; "fgij"]
