(*
https://adventofcode.com/2020/day/5
*)

open Batteries
open Printf

let empty_seat filename =
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

    let seats = Array.make 1024 false in

    let do_line line =
        printf "%s " line;
        let line_bin = String.map to_bin line in
        let row = get_row line_bin in
        let column = get_column line_bin in
        let id = row * 8 + column in
        printf "%d:%d ID = %d\n" row column id;
        Array.set seats id true in

    let find_empty arr =
        let check_empty idx b =
            if b then
                ()
            else
                printf "empty: %d\n" idx in

        Array.iteri check_empty arr in

    let filelines = File.lines_of filename in
    Enum.iter do_line filelines;
    (* Does not actually find the single answer. Just print out the empty seats and
       look manually for the one in the middle. *)

    find_empty seats
;;


empty_seat "input.txt";;
