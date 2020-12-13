(*
https://adventofcode.com/2020/day/13
*)

open Printf

let input_test =
    (939, [7; 13; 59; 31; 19]);;

let input =
    (1000052, [23; 37; 863; 19; 13; 17; 29; 571; 41]);;

let solve (timestamp, buses) =
    let wait_time bus =
        (bus, bus - timestamp mod bus) in

    let cmp (_, wt1) (_, wt2) =
        wt1 - wt2 in

    let wait_times = List.map wait_time buses in
    let sorted = List.sort cmp wait_times in
    List.hd sorted
;;

let (bus_id, wait) = solve input in
printf "%d * %d = %d\n" bus_id wait (bus_id * wait);;
