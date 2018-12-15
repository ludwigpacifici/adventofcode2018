type action =
  (* Guard In *)
  | Guard of int
  (* Minute when guard fall asleep *)
  | Asleep of int
  (* Minute when guard wakes up *)
  | WakeUp of int

let parse_guard_id line =
  Scanf.sscanf
    line
    "[1518-%d-%d %d:%d] guard #%d begins shift"
    (fun _ _ _ _ id -> Guard id )


let%test _ =
  parse_guard_id "[1518-11-01 00:00] guard #10 begins shift" = Guard 10

let%test _ =
  parse_guard_id "[1518-11-01 23:58] guard #99 begins shift" = Guard 99

let parse_falls_asleep line =
  Scanf.sscanf line "[1518-%d-%d %d:%d] falls asleep" (fun _ _ _ minute ->
      Asleep minute )


let%test _ = parse_falls_asleep "[1518-11-01 00:05] falls asleep" = Asleep 5

let parse_wakes_up line =
  Scanf.sscanf line "[1518-%d-%d %d:%d] wakes up" (fun _ _ _ minute ->
      WakeUp minute )


let%test _ = parse_wakes_up "[1518-11-01 00:25] wakes up" = WakeUp 25

exception Failure of string

let find_action line =
  match String.sub line 19 5 with
  | "guard" ->
      parse_guard_id line
  | "falls" ->
      parse_falls_asleep line
  | "wakes" ->
      parse_wakes_up line
  | _ ->
      raise (Failure line)


let sort_chronological lines =
  lines
  |> List.map String.lowercase_ascii
  (* Not robust but alpha-sort does the job with the given inputs *)
  |> Core.List.sort ~compare:String.compare
  |> List.map find_action


let%test _ =
  sort_chronological
    [ "[1518-11-05 00:45] falls asleep"
    ; "[1518-11-04 00:46] wakes up"
    ; "[1518-11-04 00:36] falls asleep"
    ; "[1518-11-03 00:24] falls asleep"
    ; "[1518-11-05 00:55] wakes up"
    ; "[1518-11-05 00:03] Guard #99 begins shift"
    ; "[1518-11-01 00:30] falls asleep"
    ; "[1518-11-01 00:05] falls asleep"
    ; "[1518-11-01 00:25] wakes up"
    ; "[1518-11-04 00:02] Guard #99 begins shift"
    ; "[1518-11-03 00:29] wakes up"
    ; "[1518-11-02 00:50] wakes up"
    ; "[1518-11-01 00:00] Guard #10 begins shift"
    ; "[1518-11-02 00:40] falls asleep"
    ; "[1518-11-01 00:55] wakes up"
    ; "[1518-11-01 23:58] Guard #99 begins shift"
    ; "[1518-11-03 00:05] Guard #10 begins shift" ]
  = [ Guard 10
    ; Asleep 5
    ; WakeUp 25
    ; Asleep 30
    ; WakeUp 55
    ; Guard 99
    ; Asleep 40
    ; WakeUp 50
    ; Guard 10
    ; Asleep 24
    ; WakeUp 29
    ; Guard 99
    ; Asleep 36
    ; WakeUp 46
    ; Guard 99
    ; Asleep 45
    ; WakeUp 55 ]

let max_minute = 59

let make_map sorted_records =
  let f (guard_id, asleep_start, m) = function
    | Guard id ->
        (id, 0, m)
    | Asleep asleep_start ->
        (guard_id, asleep_start, m)
    | WakeUp asleep_end ->
        let m =
          Core.Int.Map.update m guard_id ~f:(function
              | None ->
                  Core.Array.init max_minute ~f:(function
                      | i when asleep_start <= i && i < asleep_end ->
                          1
                      | _ ->
                          0 )
              | Some arr ->
                  for i = asleep_start to asleep_end - 1 do
                    arr.(i) <- arr.(i) + 1
                  done ;
                  arr )
        in
        (guard_id, 0, m)
  in
  let _, _, map =
    sorted_records |> Core.List.fold ~f ~init:(0, 0, Core.Int.Map.empty)
  in
  map


(* Optional type would be more cleaner *)
let maxi arr =
  Core.Array.foldi arr ~init:(0, 0) ~f:(fun i (i_max, v_max) v ->
      if v > v_max then (i, v) else (i_max, v_max) )


let%test _ = maxi [||] = (0, 0)

let%test _ = maxi [|1; 2; 3; 2; 1|] = (2, 3)

let checksum = ( * )

let part1 lines =
  let m = lines |> sort_chronological |> make_map in
  let id, _, max =
    Core.Map.fold m ~init:[] ~f:(fun ~key ~data acc ->
        let sum arr = Core.Array.fold arr ~init:0 ~f:( + ) in
        let only_i =
          let i, _ = maxi data in
          i
        in
        (key, sum data, only_i) :: acc )
    |> Core.List.fold
         ~init:(0, 0, 0)
         ~f:(fun (max_id, max_sum, max_max) (id, sum, max) ->
           if sum > max_sum then (id, sum, max) else (max_id, max_sum, max_max)
       )
  in
  checksum id max


let%test _ =
  part1
    [ "[1518-11-05 00:45] falls asleep"
    ; "[1518-11-04 00:46] wakes up"
    ; "[1518-11-04 00:36] falls asleep"
    ; "[1518-11-03 00:24] falls asleep"
    ; "[1518-11-05 00:55] wakes up"
    ; "[1518-11-05 00:03] Guard #99 begins shift"
    ; "[1518-11-01 00:30] falls asleep"
    ; "[1518-11-01 00:05] falls asleep"
    ; "[1518-11-01 00:25] wakes up"
    ; "[1518-11-04 00:02] Guard #99 begins shift"
    ; "[1518-11-03 00:29] wakes up"
    ; "[1518-11-02 00:50] wakes up"
    ; "[1518-11-01 00:00] Guard #10 begins shift"
    ; "[1518-11-02 00:40] falls asleep"
    ; "[1518-11-01 00:55] wakes up"
    ; "[1518-11-01 23:58] Guard #99 begins shift"
    ; "[1518-11-03 00:05] Guard #10 begins shift" ]
  = 240

let part2 lines =
  let m = lines |> sort_chronological |> make_map in
  let id, _, max =
    Core.Map.fold m ~init:[] ~f:(fun ~key ~data acc ->
        let i, m = maxi data in
        (key, m, i) :: acc )
    |> Core.List.fold
         ~init:(0, 0, 0)
         ~f:(fun (max_id, max_sum, max_max) (id, sum, max) ->
           if sum > max_sum then (id, sum, max) else (max_id, max_sum, max_max)
       )
  in
  checksum id max


let%test _ =
  part2
    [ "[1518-11-05 00:45] falls asleep"
    ; "[1518-11-04 00:46] wakes up"
    ; "[1518-11-04 00:36] falls asleep"
    ; "[1518-11-03 00:24] falls asleep"
    ; "[1518-11-05 00:55] wakes up"
    ; "[1518-11-05 00:03] Guard #99 begins shift"
    ; "[1518-11-01 00:30] falls asleep"
    ; "[1518-11-01 00:05] falls asleep"
    ; "[1518-11-01 00:25] wakes up"
    ; "[1518-11-04 00:02] Guard #99 begins shift"
    ; "[1518-11-03 00:29] wakes up"
    ; "[1518-11-02 00:50] wakes up"
    ; "[1518-11-01 00:00] Guard #10 begins shift"
    ; "[1518-11-02 00:40] falls asleep"
    ; "[1518-11-01 00:55] wakes up"
    ; "[1518-11-01 23:58] Guard #99 begins shift"
    ; "[1518-11-03 00:05] Guard #10 begins shift" ]
  = 4455
