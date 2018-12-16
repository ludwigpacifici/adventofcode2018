let is_reducable x y = Core.Char.lowercase x = Core.Char.lowercase y && x <> y

let%test _ = is_reducable 'a' 'A' = true

let%test _ = is_reducable 'a' 'a' = false

let collapse str =
  let f stack c =
    match stack with
    | [] ->
        [c]
    | previous :: tl ->
        if is_reducable c previous then tl else c :: stack
  in
  Core.String.fold str ~init:[] ~f |> Core.List.rev


let%test _ = collapse "" = []

let%test _ = collapse "aA" = []

let%test _ = collapse "abBA" = []

let%test _ = collapse "abAB" = ['a'; 'b'; 'A'; 'B']

let%test _ = collapse "aabAAB" = ['a'; 'a'; 'b'; 'A'; 'A'; 'B']

let%test _ =
  collapse "dabAcCaCBAcCcaDA"
  = ['d'; 'a'; 'b'; 'C'; 'B'; 'A'; 'c'; 'a'; 'D'; 'A']

let part1 lines =
  lines |> Core.List.map ~f:collapse |> Core.List.map ~f:Core.List.length


let%test _ = part1 [] = []

let%test _ = part1 ["aA"] = [0]

let%test _ = part1 ["abBA"] = [0]

let%test _ = part1 ["abAB"] = [4]

let%test _ = part1 ["aabAAB"] = [6]

let%test _ = part1 ["dabAcCaCBAcCcaDA"] = [10]

let filter c str = Core.String.filter str ~f:(( <> ) c)

let%test _ = filter 'a' "" = ""

let%test _ = filter 'a' "abaca" = "bc"

let filter_az str =
  (* Is there a way to use range, i.e. something like 'a'..'z'? *)
  let a_to_z =
    Core.List.range (int_of_char 'a') (int_of_char '{')
    |> Core.List.map ~f:char_of_int
  in
  Core.List.fold
    ~init:[]
    ~f:(fun acc c -> (str |> filter c |> filter (Core.Char.uppercase c)) :: acc)
    a_to_z


let%test _ =
  filter_az "abcdefghiklmnopqrstvxyz"
  = [ "abcdefghiklmnopqrstvxy"
    ; "abcdefghiklmnopqrstvxz"
    ; "abcdefghiklmnopqrstvyz"
    ; "abcdefghiklmnopqrstvxyz"
    ; "abcdefghiklmnopqrstxyz"
    ; "abcdefghiklmnopqrstvxyz"
    ; "abcdefghiklmnopqrsvxyz"
    ; "abcdefghiklmnopqrtvxyz"
    ; "abcdefghiklmnopqstvxyz"
    ; "abcdefghiklmnoprstvxyz"
    ; "abcdefghiklmnoqrstvxyz"
    ; "abcdefghiklmnpqrstvxyz"
    ; "abcdefghiklmopqrstvxyz"
    ; "abcdefghiklnopqrstvxyz"
    ; "abcdefghikmnopqrstvxyz"
    ; "abcdefghilmnopqrstvxyz"
    ; "abcdefghiklmnopqrstvxyz"
    ; "abcdefghklmnopqrstvxyz"
    ; "abcdefgiklmnopqrstvxyz"
    ; "abcdefhiklmnopqrstvxyz"
    ; "abcdeghiklmnopqrstvxyz"
    ; "abcdfghiklmnopqrstvxyz"
    ; "abcefghiklmnopqrstvxyz"
    ; "abdefghiklmnopqrstvxyz"
    ; "acdefghiklmnopqrstvxyz"
    ; "bcdefghiklmnopqrstvxyz" ]

let part2 lines =
  lines
  |> Core.List.fold ~init:[] ~f:(fun acc str -> filter_az str @ acc)
  |> Core.List.map ~f:collapse
  |> Core.List.map ~f:Core.List.length
  |> Core.List.reduce ~f:min


let%test _ = part2 [] = None

let%test _ = part2 [""] = Some 0

let%test _ = part2 ["dabAcCaCBAcCcaDA"] = Some 4
