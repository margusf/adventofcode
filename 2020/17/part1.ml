(*
https://adventofcode.com/2020/day/17
*)

open Printf

let input_test = [
    ".#.";
    "..#";
    "###";
];;

let input = [
    "#.##.##.";
    ".##..#..";
    "....#..#";
    ".##....#";
    "#..##...";
    ".###..#.";
    "..#.#..#";
    ".....#..";
];;

let solve input count =
    let make_input_arr input =
        let ret = Array.make_matrix (List.length input) (String.length (List.hd input)) '.' in
        let add_string y str =
            String.iteri (fun x c -> ret.(y).(x) <- c) str in

        List.iteri add_string input;
        [|ret|] in

    let array_dimensions input =
        let zdim = Array.length input in
        let ydim = Array.length input.(0) in
        let xdim = Array.length input.(0).(0) in
        (xdim, ydim, zdim) in

    let count_active input =
        let (xdim, ydim, zdim) = array_dimensions input in
        let count = ref 0 in
        for z = 0 to zdim - 1 do
            for y = 0 to ydim - 1 do
                for x = 0 to xdim - 1 do
                    if input.(z).(y).(x) = '#' then
                        count := !count + 1
                    else
                        ()
                done
            done
        done;
        !count in

    let print_arr arr =
        let print_dim idx arr =
            let print_row row =
                print_string "    ";
                Array.iter print_char row;
                print_newline () in

            printf "  Level %d\n" idx;
            Array.iter print_row arr in

        Array.iteri print_dim arr in

    let make_new_array xdim ydim zdim =
        let ret = Array.make_matrix zdim ydim [||] in
        for z = 0 to zdim - 1 do
            for y = 0 to ydim - 1 do
                ret.(z).(y) <- Array.make xdim '.'
            done
        done;
        ret in

    let get_input input x y z =
        let (xdim, ydim, zdim) = array_dimensions input in
        if x - 1 >= xdim || x < 1 || y - 1 >= ydim || y < 1 || z - 1 >= zdim || z < 1 then
            '.'
        else
            input.(z - 1).(y - 1).(x - 1) in

    let count_neighbors input x y z =
        let count = ref 0 in
        for dx = -1 to 1 do
            for dy = -1 to 1 do
                for dz = -1 to 1 do
                    if  (dx <> 0 || dy <> 0 || dz <> 0)
                            && (get_input input (x + dx) (y + dy) (z + dz)) = '#' then (
                        printf "  Found neighbor (%d, %d, %d)\n" (x + dx) (y + dy) (z + dz);
                        count := !count + 1)
                    else
                        ()
                done
            done
        done;
        !count in

    let transform input x y z =
        let neighbors = count_neighbors input x y z in
        printf "  (%d, %d, %d) = %c -> %d neighbours\n" x y z (get_input input x y z) neighbors;
        if (get_input input x y z) = '#' then
            if neighbors = 2 || neighbors = 3 then
                '#'
            else
                '.'
        else
            if neighbors = 3 then
                '#'
            else
                '.' in

    let run_cycle input =
        let (xdim, ydim, zdim) = array_dimensions input in

        let new_zdim = zdim + 2 in
        let new_ydim = ydim + 2 in
        let new_xdim = xdim + 2 in

        let output = make_new_array new_xdim new_ydim new_zdim in
        for z = 0 to new_zdim - 1 do
            printf "  Transform level %d\n" z;
            for y = 0 to new_ydim - 1 do
                for x = 0 to new_xdim - 1 do
                    output.(z).(y).(x) <- transform input x y z
                done
            done
        done;
        output in

    let rec loop input count =
        printf "Loop %d\n" count;
        print_arr input;
        if (count = 0) then
            count_active input
        else
            loop (run_cycle input) (count - 1) in

    let input_arr = make_input_arr input in
    loop input_arr count
;;

printf "result = %d\n" (solve input 6);;
