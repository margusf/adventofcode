(*
https://adventofcode.com/2020/day/11
*)

open Printf

let input_test = [
    "L.LL.LL.LL";
    "LLLLLLL.LL";
    "L.L.L..L..";
    "LLLL.LL.LL";
    "L.LL.LL.LL";
    "L.LLLLL.LL";
    "..L.L.....";
    "LLLLLLLLLL";
    "L.LLLLLL.L";
    "L.LLLLL.LL";
];;

let input = [
    "LLLLL.LLLLLLLLLLLLLLLLL.LLLLLL.LLLLL.L.LLLLLL.LLLLLL.LLL.LLLLLLLL.LLLLL.L.LLLLLLLLLLLLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLL.LLLLLLLLLLLLL.L.LLLLLLLLLL.LLLLLL.LLLLLLL";
    "LLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLL.LLL..LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL";
    "LLLLLLLL.LLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL.L.LLLLLLLLLLLLLLLL.LLLLL.L";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLL.L.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLL";
    "L.LLLLLLLLLLLLL.LLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LL.LLLL.LLLLLLLLL.LLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL";
    "LLLLLLLLLLLLLLLLLLL.LLL.LLLLLL.L.LLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LL.LLLL";
    "LLLLL.LLLLLLLLL.LLLLLLLLLLL.LL.LLLLL.LLLLLL.LL.LLLLLLLL..LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL";
    "LL.LL.LLL.LLLLLLLLLLLLL.LLLL.L.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL.L.LLLLLLL";
    "......L....L...L...L.....L........LLL..LL..LL..L.L..LL.L..L.....LL......L.LLL.L..L....LLL.......LL";
    "LLLLL.LLLLLLLL.LLLLLLLL.LLLLL..LLLLL.LLL.LLLL..LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL";
    "LLLLL.LLLLLLLLL.LL.LLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLL.LL..L.L";
    "LLLLL.LLLLLLL...LLLLLLLLLLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLL..LLLLLLL.LLLLL.LL.LLL.LLLLLLLLL.LLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLL.LLLL.LL.LLLLLLLLLLLL..LLLLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLL.LL.LLLLLL.LLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL.LL.LLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLL.LL.LLLL";
    "L.LL..LLLLLLLLL.LLLLLLL.L.LLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL..LLLLLLLLL.LLLLLLLLLLLLLL";
    ".L.......L..LL.....L.L.....L..............L.L.L.LL..LL.L.L..LLL.L..LL.............LLL.L......L.LL.";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLLL.LLLLL.LL.LLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.L.LLLL.LLLLLLL";
    "LLLLL.LLL.LLLLL.LLLLLLL.LLLLLL..LLLLLLLLLLLLLLLLLLLL.LLL.L.LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLL..L.LLLL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLL..LLLLLL.LLLLLLLLL.LLLLLLL.LL.LL.";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLLL.LLLLL.LLLLLLL.L.LL.LLLLL..L..LLL.L.LLLLLLL..LLLLLLLL..LLLLL.LLLLLLL";
    "LLLLL.LLL.LLLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLL.LLLLL.LLL.LLLLL.LL.LL.LLLL.LLLLLL.LL.LLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLL..LLLLLLLLLLLLL.L.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LL.LLLL";
    "LLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.L.LLL.LLLLLL.LL.LLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLL.LLLLLLLLLL";
    "......L.....L...LL.L.L..L...L....L...LLL...LLLLL..L.....L.....L..L......L...L.L.......L..L.LL...L.";
    "LL.LLL.LLLLLLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLL.LL.LLLL.LLLLLLLLL.LL.LLLLLLLLLLL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLLL..LLLLLLLLL..LLLLLLLL.LLLL.LLL..LLLLLLLLLLLLLLLL.LLLL.L.LLLLLLL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLL..LLLLLL.LLLLLL.LLLLLL.LL.LLLLLLL";
    "LLLLL..LL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLL.LLLL..LLLLLLL";
    ".LLL...LL.....L.L..L.LL..LL......LL.L.L........L.L.....LLLL.L.L.......L......LL.....LLLL....L.L.LL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLL.LL.LLLL.LLLLLLLLLLLLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLL";
    "LLLL..LLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.L..LLLLLLL..LLLL.L.LLLLLLLLL.LLLLLL.LLLLLLL";
    "LLLLLLLLLLLL.LL.LLLLLLL.LLLL.L.LLLLLLLLLLLL.LL.LLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL..LLLLL.LLLLLLL";
    "LLLLLLLLLLL.LLL.LLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLLL.LLLL.LLL.LLLLLLLLLLLLLLL.L.LLLLLLLLLLLLLL";
    ".L.L...L...L.....L...LLLLLLL.LL....LL..LLL...L...L.LLL..L.L...L.L.L....L.LL.LLL..L....L..LL..L...L";
    "LLLLLLLLLLL.LL..LLLL..L.LLLLLL.LLLLLLLLLLLL.LL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL....LLLLLLLLLLL";
    "LLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLL.LL.LLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLL.LLLLL.LLLLLLLL.LLLL..LLL.LLLLLLLLLLLLLLL.LLLLLL.L.LLLL.LL.LLLL.LLLLLLLLLL..LLLLLLL";
    "LLLLL..LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLL";
    "LLLLL.LLLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.";
    "LLLLL.LLLLLLLLLLLLL.LLL.LLLLLL..LLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLL..LLLL.LLLL.LLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL";
    "LLLLLLLLLLLLLLL.LLLLLLL.LLLLLL.LLL.L.LLLLLLLLL..LLLLL.LL..LLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL";
    "L..L.L.LLL.LL.......LL.LL..LL.L...L..LLL........L.LL....LL...LL.L.....LL......L.L.L...L.L.L..L..L.";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLL..LLLL.LL.LLLLLLLLL.LLLLLL.LLLLLLL";
    "LL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLL.L.LLLLL";
    "LLLLL.LLLLLLL.L.L.L.LLL..LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLL.LLLLLLLLLLLLLL.LLLLL";
    "LL....LLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLL";
    "L.LLL.LLLLLLLLL.LL.LL.L.LLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL..LLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL";
    "LLLLLLLLLLLLLLL.LLLLLLLLLLLLLL..LLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL";
    "LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL..LLLLLLLLLLLLLL.LLLL.LLLLLLLL..LLLLLLLLLLLLLLL..LLLLLL..L.LLLL";
    "LLLLL.LLL.LL.LL.L.LLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLL..LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL";
    "..LL....L.LL...L.......L.L...L.L..LLL....L...L.L..L...L......L...LLL.L.L...L..L.LL...L..L..L.L...L";
    "LLLLL.LLLLLL....LLLLLLL.LLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLL..LLLLLL.LLLLLLL";
    "LLLLL..LLLLLLL.LLLLLLLLLLLLLLL.LLLLLL.LL.LLLLLLLLLL.LLLL..LLLLLLLLLLLLL.L.LLLLLLLLLLLLLLLL.LLLL.LL";
    "LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLL.L.LLLLLLL";
    "LLLLL.LLLLLLLLL.L.LLLLL.LLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLL..LLLLL.LLLLLLL";
    "LLLL..LLLLLLLLLLLLLLLLL.LLLLLL.LLLLL.LLL.LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.L";
    "LLLLL.LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.L";
    "LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLL.LLLLL..LLLLLLLLL.LLLLLL.LLLLLLL";
    "LLLLLLLLLLLLLLLLLLLLLLL.LL.LLL.LLLLL.LLLLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLL.LLLL.LL";
    "LLLLL.LLLLLLLLL.LLLLLLL..LLLLL.LLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.L.LLLL.LLLL.LL";
    ".L.....L.LL.......L.L..L.L...LLL.L.LL..LLL..L.L....L........L..L.L...L....L...L.LL..L.....L..LL..L";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLL.LLL.LLLLLLLLLLLLL.LLLLLLLLL.LL.LLLLL.LLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLL";
    "LLLLLLLLLLLLLLL.LL.LLLL.LLLLLL.LLLLL.LL.LLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLL.LLLLLLL";
    "LLLLL.LLLL..LLL.LLLLLLL.LL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLL..LL.LLL";
    "LLLLL.LLLLLLLLLLLLLLLLLLLLLL.L.LLLLLL.L.LLLLLLLLLLL.LLLL.LL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLL..LLLLLLL";
    "LLLLL.L.LLLLLL..LLLLL.L.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL..LLLLLLLL.LLLLLL..LLLLLLLLLLLLLLLLLLLLLLLL";
    "LL.LL.LLLLLLLL.LLLLLLL.LLL.LLL...LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LL.L.LLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLL.LL.LLLLLL.LL.LLLL";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLL..LLLLL.LLL.LLLLLLLLL.LLLLL.LLLLLL.L.LLLLLLL.LLLLLL.LL.LLLLL.LLLLLLLL";
    "LLLLL.LLL.LLLLL.LLLLLLLLLL.LLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLL..LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL";
    "........LL..LLL.....L.L...LLL...L.........L.L...L..L...LL.L.....L....L....L.....L....L....L.LLL...";
    "LLLLL.LLLLL..LLLLLLLLLLLLL.LLL.LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLL.LLL.LL.LLLLLLL";
    "LLLLL.LLLLLLLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLL.LL.LL";
    "LL.LL.LLLLLLLL.LLLLL.LLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL..LLLLLLLLLLLLLLLLLLLLLLL";
    "LLLLL.LLLLLLLLLLLLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.L";
    "LLLLL..LLLLLLLL.LLLLLLL.LLLLLLLLL.LL.LLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL";
    "LLLLLLLLLLLLLLL.LLLLLL..LLLLLL.LLLLL.LLLLLLLL.LL.LLLLLLL.LLLLL.LL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLL";
    "L.LL.....LL......LL.L.LL...LL..........LL.LLL...L..L..LL....L......L.....LL.....LL.L..L.LL.L.L....";
    "LLLLL.LLLLLLLLLL.LLLLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLL.LL.L.LLLLL.LLLLLLLLLL.LLLLLLLLLLLLLL.L.LLLLLLL";
    "LLLLLLL.LLLLLLL.LLLLLLL.LLLLLL.LL.LLLLLLLLLLLL.LLLLLLLLL.L.LLLLLL.LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL";
    "LLLLLLLLLLLLLLL.LL.LLLL.LLLLLL.LL.LL.LLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLL";
    "LLLLL.LLLLLLLLL.LLLLLL..LLL.LL.LLLLL.LLLLLLLLLLLLL.LLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLL.L.";
    "LLLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLL.LLLL.LL";
    "LLLLL.LLLLLLLLL..LLLLLL.LLLLLL.LLLL.LLLLLL.LLL.LLLLL.LLL.LLLLLLL..LLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLL";
];;

type seat = Floor | Empty | Full;;

let solve input_lst =
    let neighbours = [
        (1, 1);
        (1, 0);
        (1, -1);
        (0, 1);
        (0, -1);
        (-1, 1);
        (-1, 0);
        (-1, -1)
    ] in

    let to_arr lst =
        let arr = Array.make_matrix (List.length lst) (String.length (List.hd lst)) Floor in
        let do_string y str =
            let do_ch x ch =
                arr.(y).(x) <- match ch with
                | '.' -> Floor
                | 'L' -> Empty
                | _ -> raise (Invalid_argument "input") in
            String.iteri do_ch str in
        List.iteri do_string lst;
        arr in

    let count_full arr row col =
        let num_rows = Array.length arr in
        let num_cols = Array.length arr.(0) in

        let out_of_bounds row col =
            (row < 0) || (row >= num_rows) || (col < 0) || (col >= num_cols) in

        let count_neighbour ret (drow, dcol) =
            let new_row = row + drow in
            let new_col = col + dcol in
            if out_of_bounds new_row new_col then
                ret
            else if arr.(new_row).(new_col) = Full then
                ret + 1
            else
                ret in
        List.fold_left count_neighbour 0 neighbours in

    let count_occupied arr =
        let count_row ret row =
            Array.fold_left (fun ret v -> if v = Full then ret + 1 else ret) ret row in
        Array.fold_left count_row 0 arr in

    let print_array arr =
        let print_item item =
            print_char (match item with
                | Floor -> '.'
                | Empty -> 'L'
                | Full -> '#') in
        let print_row row =
            Array.iter print_item row;
            print_char '\n' in
        Array.iter print_row arr in

    let rec loop input =
        printf "\n\n";
        print_array input;
        let num_rows = Array.length input in
        let num_cols = Array.length input.(0) in
        let output = Array.make_matrix num_rows num_cols Floor in

        let transform row col =
            match input.(row).(col) with
            | Empty ->
                if (count_full input row col) = 0 then
                    Full
                else
                    Empty
            | Full ->
                if (count_full input row col) >= 4 then
                    Empty
                else
                    Full
            | Floor -> Floor in

        let changed = ref false in
        for row = 0 to num_rows - 1 do
            for col = 0 to num_cols - 1 do
                output.(row).(col) <- transform row col;
                if output.(row).(col) <> input.(row).(col) then
                    changed := true
                else
                    ()
            done
        done;
        if !changed then
            loop output
        else
            count_occupied output in

    let arr = to_arr input_lst in
    loop arr
;;

printf "result = %d\n" (solve input);;
