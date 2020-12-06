(*
https://adventofcode.com/2020/day/6
*)

open Batteries
open Printf

module ChrSet = Set.Make(Char)

let sum_forms filename =
    let finish_record (fields, result) =
        let num_of_answers = ChrSet.cardinal fields in
        (ChrSet.empty, result + num_of_answers) in

    let do_line (fields, result) line =
        let parse_line () =
            (String.fold_left (fun f c -> ChrSet.add c f) fields line, result) in

        if line = "" then
            finish_record (fields, result)
        else
            parse_line () in


    let filelines = File.lines_of filename in
    let (_, result) = finish_record (Enum.fold do_line (ChrSet.empty, 0) filelines) in
    result
;;


Printf.printf "%d\n" (sum_forms "input.txt");;
