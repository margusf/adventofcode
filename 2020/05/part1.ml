(*
https://adventofcode.com/2020/day/5
*)

open Batteries
open Printf

let max_id filename =
    let get_row line =
        let row_part = "0b" ^ String.sub line 0 7 in
        int_of_string row_part in

    let get_column line =
        let col_part = "0b" ^ String.sub line 7 3 in
        int_of_string col_part in

    let to_bin c =
        match c with
        | 'F' -> '0'
        | 'B' -> '1'
        | 'R' -> '1'
        | 'L' -> '0'
        | _ -> raise (Invalid_argument "x") in

    let do_line cur_max line =
        printf "%s " line;
        let line_bin = String.map to_bin line in
        let row = get_row line_bin in
        let column = get_column line_bin in
        let id = row * 8 + column in
        printf "%d:%d ID = %d\n" row column id;
        max cur_max id in

    let filelines = File.lines_of filename in
    Enum.fold do_line 0 filelines
;;


Printf.printf "%d\n" (max_id "input.txt");;
