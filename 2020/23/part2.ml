(*
https://adventofcode.com/2020/day/22
*)

open Printf
open Batteries

let input_test = "389125467";;

let input = "925176834";;

let solve input =
    let make_queue input =
        let rec add_iter node v =
            (*if (v mod 10000) = 0 then
                printf "Add, count = %d\n" v
            else
                ();*)
            if v > 1000000 then
                ()
            else begin
                let n2 = BatDllist.append node v in
                add_iter n2 (v + 1)
            end in

        print_string "MAKE QUEUE\n"; print_newline ();
        let chars = BatString.to_list input in
        let lst = List.map (fun c -> (int_of_char c) - (int_of_char '0')) chars in
        let ret = BatDllist.of_list lst in
        add_iter (BatDllist.prev ret) ((BatList.max lst) + 1);
        ret in

    let make_lookup queue =
        let count = BatDllist.length queue in
        let ret = Array.make count queue in

        let rec additem node idx =
            if idx >= count then
                ()
            else begin
                ret.((BatDllist.get node) - 1) <- node;
                additem (BatDllist.next node) (idx + 1)
            end in

        additem queue 0;
        ret in

    let remove3 queue =
        let n1 = BatDllist.next queue in
        let v1 = BatDllist.get n1 in
        let n2 = BatDllist.drop n1 in
        let v2 = BatDllist.get n2 in
        let n3 = BatDllist.drop n2 in
        let v3 = BatDllist.get n3 in
        let _ = BatDllist.drop n3 in
        [v1; v2; v3] in

    let rec skip_removed search_val removed =
        if search_val < 1 then
            skip_removed 1000000 removed
        else  if List.exists ((=) search_val) removed then
            skip_removed (search_val - 1) removed
        else
            search_val in

    let add_after lookup node lst =
        let [x1; x2; x3] = lst in
        let n1 = BatDllist.append node x1 in
        let n2 = BatDllist.append n1 x2 in
        let n3 = BatDllist.append n2 x3 in
        lookup.(x1 - 1) <- n1;
        lookup.(x2 - 1) <- n2;
        lookup.(x3 - 1) <- n3 in

    let rec play queue lookup count =
        if count = 0 then
            queue
        else begin
            let removed = remove3 queue in
            let search_val = (BatDllist.get queue) - 1 in
            if (count mod 100000 = 0) then
                (print_string ("Play, iter = " ^ string_of_int count); print_newline ())
            else
                ();
            let dest_val = skip_removed search_val removed in
            let dest_node = lookup.(dest_val - 1) in
            add_after lookup dest_node removed;
            play (BatDllist.next queue) lookup (count - 1)
        end in

    let make_output queue =
        print_string "Make output\n";
        let n1 = BatDllist.find ((=) 1) queue in
        let n2 = BatDllist.next n1 in
        let v1 = BatDllist.get n2 in
        let n3 = BatDllist.next n2 in
        let v2 = BatDllist.get n3 in
        (v1, v2) in

    print_string "BEGIN\n"; print_newline ();
    let queue = make_queue input in
    let lookup = make_lookup queue in
    printf "STARTING PLAY\n";
    let after_play = play queue lookup 10000000 in
    make_output after_play
;;

let (x, y) = solve input in
printf "Result: %d * %d = %d\n" x y (x * y);;
