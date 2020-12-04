(*
https://adventofcode.com/2020/day/4

Thought hard about preprocessing the input in vim, but decided to learn
string processing in OCaml.
*)

open Batteries

module StringSet = Set.Make(String)

let solve filename =
    let required_fields = List.fold_right StringSet.add [
            "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"] StringSet.empty in

    let do_field (missing_fields, count) field =
        let (p1, p2) = BatString.rsplit field ":" in

        Printf.printf "%s\n" p1;
        (StringSet.remove p1 missing_fields, count) in

    let finish_record (missing_fields, count) =
        Printf.printf "Finish, %b\n" (StringSet.is_empty missing_fields);
        if StringSet.is_empty missing_fields then
            (required_fields, count + 1)
        else
            (required_fields, count) in

    let do_line (missing_fields, count) line =

        let parse_line (missing_fields, count) =
            let fields = String.nsplit line " " in

            let rec do_fieldlist (missing_fields, count) fl =
                match fl with
                | [] -> (missing_fields, count)
                | h :: t -> do_fieldlist (do_field (missing_fields, count) h) t in


            do_fieldlist (missing_fields, count) fields
        in

        if line = "" then
            finish_record (missing_fields, count)
        else
            parse_line (missing_fields, count)
    in

    let filelines = File.lines_of filename in
    let (missing_fields, count) =
        finish_record (Enum.fold do_line (required_fields, 0) filelines) in
    count
;;

Printf.printf "%d\n" (solve "test_val.txt");;
