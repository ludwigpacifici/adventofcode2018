let parse line = Scanf.sscanf line "%d, %d" (fun x y -> (x, y))

let%test _ = parse "1,2" = (1, 2)

let size projection points =
  points
  |> Core.List.map ~f:projection
  |> Core.List.reduce ~f:max
  |> Core.Option.map ~f:(fun x -> x + 1)
  |> Core.Option.value ~default:0


let max_x points = size fst points

let%test _ = max_x [] = 0

let%test _ =
  let points = [(1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9)] in
  max_x points = 9

let max_y points = size snd points

let%test _ = max_y [] = 0

let%test _ =
  let points = [(1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9)] in
  max_y points = 10

let l1_norm (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let%test _ = l1_norm (0, 0) (0, 0) = 0

let%test _ = l1_norm (1, 2) (3, -4) = 8

let is_closest c (min_norm, closest) p =
  let p_norm = l1_norm c p in
  if p_norm < min_norm
  then (p_norm, Some p)
  else if p_norm = min_norm
  then (min_norm, None)
  else (min_norm, closest)


let%test _ = is_closest (0, 0) (4, Some (2, 2)) (1, 1) = (2, Some (1, 1))

let%test _ = is_closest (0, 0) (4, Some (2, 2)) (3, 1) = (4, None)

let%test _ = is_closest (0, 0) (1, None) (2, 2) = (1, None)

let%test _ = is_closest (0, 0) (1, Some (1, 1)) (2, 2) = (1, Some (1, 1))

let closest_from points c =
  Core.List.fold
    ~init:(Core.Int.max_value, None)
    ~f:(fun closest p -> is_closest c closest p)
    points
  |> snd


let build_array points =
  let y_len = max_y points + 1 in
  let x_len = max_x points + 1 in
  Array.init y_len (fun y ->
      Array.init x_len (fun x -> closest_from points (x, y)) )


module Area = struct
  type t =
    | Size of int
    | Infinite

  let bind f x = match x with Infinite -> Infinite | Size s -> f s

  let empty = Size 0

  let incr (xl, yl) (xu, yu) (x, y) size =
    match (x, y) with
    | x, y when x = xl || x = xu || y = yl || y = yu ->
        Infinite
    | _, _ ->
        Size (size + 1)
end

let compute_area point_number arr =
  let y_len = Array.length arr - 1 in
  let x_len = Array.length arr.(0) - 1 in
  let m = Hashtbl.create point_number in
  let find_or h default key =
    Hashtbl.find_opt h key |> Core.Option.value ~default
  in
  for y = 0 to y_len do
    for x = 0 to x_len do
      arr.(y).(x)
      |> Core.Option.map ~f:(fun c ->
             find_or m Area.empty c
             |> Area.bind (Area.incr (0, 0) (x_len, y_len) (x, y))
             |> Hashtbl.replace m c )
      |> ignore
    done
  done ;
  m


let part1 lines =
  let points = Core.List.map ~f:parse lines in
  build_array points
  |> compute_area (List.length points)
  |> Hashtbl.to_seq_values
  |> Seq.fold_left
       (fun acc s ->
         match s with
         | Area.Size s when s > acc ->
             s
         | Size _ | Infinite ->
             acc )
       0


let%test _ =
  let lines = ["1, 1"; "1, 6"; "8, 3"; "3, 4"; "5, 5"; "8, 9"] in
  part1 lines = 17

let range l = Core.List.init l ~f:(fun i -> i) |> Core.Sequence.of_list

let%test _ = range 0 |> Core.Sequence.to_list = []

let%test _ = range 2 |> Core.Sequence.to_list = [0; 1]

let l1_sum points c =
  Core.List.fold_left ~init:0 ~f:(fun acc p -> acc + l1_norm c p) points


let%test _ = l1_sum [] (0, 0) = 0

let%test _ =
  let points = [(1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9)] in
  l1_sum points (0, 0) = 54

let part2 ~total_distance_threshold lines =
  let points = Core.List.map ~f:parse lines in
  let y_len = max_y points + 1 in
  let x_len = max_x points + 1 in
  Core.Sequence.cartesian_product (range x_len) (range y_len)
  |> Core.Sequence.map ~f:(fun c -> l1_sum points c)
  |> Core.Sequence.count ~f:(fun s -> s < total_distance_threshold)


let%test _ =
  let lines = ["1, 1"; "1, 6"; "8, 3"; "3, 4"; "5, 5"; "8, 9"] in
  part2 ~total_distance_threshold:32 lines = 16
