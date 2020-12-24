(*
https://adventofcode.com/2020/day/23
*)

open Printf
open Batteries

let input_test = "389125467";;

let input = "925176834";;

let solve input =
    let make_queue input =
        let chars = BatString.to_list input in
        let lst = List.map (fun c -> (int_of_char c) - (int_of_char '0')) chars in
        BatDllist.of_list lst in

    let remove3 queue =
        let q1 = BatDllist.next queue in
        let lst = BatDllist.to_list q1 in
        let d1 = BatDllist.drop q1 in
        let d2 = BatDllist.drop d1 in
        ignore (BatDllist.drop d2);
        BatList.take 3 lst in

    let rec skip_removed search_val removed =
        if search_val < 1 then
            skip_removed 9 removed
        else  if List.exists ((=) search_val) removed then
            skip_removed (search_val - 1) removed
        else
            search_val in

    let add_after node lst =
        let [x1; x2; x3] = lst in
        let n1 = BatDllist.append node x1 in
        let n2 = BatDllist.append n1 x2 in
        ignore (BatDllist.append n2 x3) in

    let rec play queue count =
        if count = 0 then
            queue
        else begin
            let removed = remove3 queue in
            let search_val = (BatDllist.get queue) - 1 in
            printf "Play, current = %d\n" (BatDllist.get queue);
            let dest_val = skip_removed search_val removed in
            let dest_node = BatDllist.find ((=) dest_val) queue in
            add_after dest_node removed;
            play (BatDllist.next queue) (count - 1)
        end in

    let make_output queue =
        let pos1 = BatDllist.find ((=) 1) queue in
        let pos2 = BatDllist.drop pos1 in
        let lst = BatDllist.to_list pos2 in
        let chars = List.map (fun i -> char_of_int (i + (int_of_char '0'))) lst in
        String.of_list chars in

    let queue = make_queue input in
    let after_play = play queue 100 in
    make_output after_play
;;

printf "Result = %s\n" (solve input);;
