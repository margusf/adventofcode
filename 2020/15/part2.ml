(*
https://adventofcode.com/2020/day/14
*)

open Printf

let input_test1 = [0; 3; 6];;

let input_test2 = [1; 3; 2];;

let input_test3 = [2; 1; 3];;

let input_test4 = [3; 1; 2];;

let input = [9; 3; 1; 0; 8; 4];;

module IntMap = Map.Make(struct type t = int let compare = compare end)

let solve input count =
    let initialize_game input =
        let rec loop lst mem previous prev_count =
            match lst with
            | [] -> (mem, previous, prev_count)
            | h :: t ->
                printf "add_item %d %d\n" (prev_count + 1) h;
                loop t (IntMap.add h (prev_count + 1) mem) h (prev_count + 1)
            | _ -> raise (Invalid_argument "empty list") in

        loop input IntMap.empty 0 0 in

    let rec loop mem previous prev_count count =
        let cur_count = prev_count + 1 in
        (* printf "loop %d  %d\n" cur_count previous; *)
        if prev_count >= count then
            previous
        else
            let cur =
                if IntMap.mem previous mem then begin
                    let prev_idx = IntMap.find previous mem in
                    let current = cur_count - prev_idx in
                    (* printf "was present, idx = %d, diff = %d\n" prev_idx current; *)
                    current
                end else begin
                    (* printf "was not present\n"; *)
                    0
                end in
            (* printf "storing %d to idx %d\n" previous cur_count; *)
            loop (IntMap.add previous cur_count mem) cur cur_count count in

    let (mem, previous, prev_count) = initialize_game input in
    loop mem 0 (prev_count) count
;;

printf "result = %d\n" (solve input 29999999);;
