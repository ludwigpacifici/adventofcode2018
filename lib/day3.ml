let parse_claim line =
  Scanf.sscanf line "#%i @ %i,%i: %ix%i" (fun id x y wide tall ->
      (id, x, y, wide, tall) )


let%test _ = parse_claim "#1 @ 1,3: 4x4" = (1, 1, 3, 4, 4)

let%test _ = parse_claim "#2 @ 3,1: 4x4" = (2, 3, 1, 4, 4)

let%test _ = parse_claim "#3 @ 5,5: 2x2" = (3, 5, 5, 2, 2)

let expand_claim x y wide tall =
  let expand n shift = List.init n (fun i -> i + shift) in
  let xs = expand wide x in
  let ys = expand tall y in
  Core.List.cartesian_product xs ys


let%test _ = expand_claim 0 0 0 0 = []

let%test _ = expand_claim 0 0 1 1 = [(0, 0)]

let%test _ =
  expand_claim 1 3 4 4
  = [ (1, 3)
    ; (1, 4)
    ; (1, 5)
    ; (1, 6)
    ; (2, 3)
    ; (2, 4)
    ; (2, 5)
    ; (2, 6)
    ; (3, 3)
    ; (3, 4)
    ; (3, 5)
    ; (3, 6)
    ; (4, 3)
    ; (4, 4)
    ; (4, 5)
    ; (4, 6) ]

let%test _ =
  expand_claim 3 1 4 4
  = [ (3, 1)
    ; (3, 2)
    ; (3, 3)
    ; (3, 4)
    ; (4, 1)
    ; (4, 2)
    ; (4, 3)
    ; (4, 4)
    ; (5, 1)
    ; (5, 2)
    ; (5, 3)
    ; (5, 4)
    ; (6, 1)
    ; (6, 2)
    ; (6, 3)
    ; (6, 4) ]

let%test _ = expand_claim 5 5 2 2 = [(5, 5); (5, 6); (6, 5); (6, 6)]

module Coordinate = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Pervasives.compare x0 x1 with
    | 0 ->
        Pervasives.compare y0 y1
    | c ->
        c
end

module CoordinateMap = Map.Make (Coordinate)

let make_claims lines =
  let populate acc (coordinate, id) =
    CoordinateMap.update
      coordinate
      (Core.Option.value_map ~default:(Some [id]) ~f:(fun l -> Some (id :: l)))
      acc
  in
  lines
  |> List.map parse_claim
  |> List.fold_left
       (fun acc (id, x, y, wide, tall) ->
         (expand_claim x y wide tall |> List.map (fun c -> (c, id))) @ acc )
       []
  |> List.fold_left populate CoordinateMap.empty


let part1 lines =
  CoordinateMap.fold
    (fun _ v acc -> match v with [] | [_] -> acc | _ -> acc + 1)
    (make_claims lines)
    0


let%test _ = part1 [] = 0

let%test _ = part1 ["#1 @ 1,3: 4x4"] = 0

let%test _ = part1 ["#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2"] = 4

let part2 lines =
  let ids =
    lines
    |> List.map parse_claim
    |> List.map (fun (id, _, _, _, _) -> id)
    |> Core.Int.Set.of_list
  in
  let collisions_ids =
    CoordinateMap.fold
      (fun _ v acc ->
        match v with
        | [] | [_] ->
            acc
        | l ->
            Core.Int.Set.union (Core.Int.Set.of_list l) acc )
      (make_claims lines)
      Core.Int.Set.empty
  in
  Core.Int.Set.diff ids collisions_ids |> Core.Int.Set.to_list


let%test _ = part2 [] = []

let%test _ = part2 ["#1 @ 1,3: 4x4"] = [1]

let%test _ = part2 ["#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2"] = [3]
