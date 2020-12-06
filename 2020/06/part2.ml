(*
https://adventofcode.com/2020/day/6
*)

open Batteries
open Printf

module ChrSet = BatSet.Make(Char)

let sum_forms filename =
    let string_to_set str =
        String.fold_left (fun f c -> ChrSet.add c f) ChrSet.empty str in

    let init_set = string_to_set "abcdefghijklmnopqrstuvwxyz" in

    let finish_record (fields, result) =
        let num_of_answers = ChrSet.cardinal fields in
        (init_set, result + num_of_answers) in

    (* Easier to write my own intersection than figuring out how to use BatSet. *)
    let intersection set1 set2 =
        let do_element e ret =
            if ChrSet.mem e set2 then
                ChrSet.add e ret
            else
                ret in
        ChrSet.fold do_element set1 ChrSet.empty in

    let do_line (fields, result) line =
        let parse_line () =
            let cur_line_set = string_to_set line in
            (intersection cur_line_set fields, result) in

        if line = "" then
            finish_record (fields, result)
        else
            parse_line () in


    let filelines = File.lines_of filename in
    let (_, result) = finish_record (Enum.fold do_line (init_set, 0) filelines) in
    result
;;


Printf.printf "%d\n" (sum_forms "input.txt");;
