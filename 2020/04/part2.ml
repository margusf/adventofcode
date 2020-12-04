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
    let eye_colors = List.fold_right StringSet.add [
            "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] StringSet.empty in

    let validate_field name value =
        let byr v =
            let i = int_of_string v in
            (i >= 1920) && (i <= 2002) in
        let iyr v =
            let i = int_of_string v in
            (i >= 2010) && (i <= 2020) in
        let eyr v =
            let i = int_of_string v in
            (i >= 2020) && (i <= 2030) in
        let hgt v =
            if String.ends_with v "cm" then
                let num = int_of_string (String.sub v 0 (String.length v - 2)) in
                (num >= 150) && (num <= 193)
            else if String.ends_with v "in" then
                let num = int_of_string (String.sub v 0 (String.length v - 2)) in
                (num >= 59) && (num <= 76)
            else
                false in
        let hcl v =
            let r = Str.regexp "^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$" in
            Str.string_match r v 0 in
        let ecl v =
            StringSet.mem v eye_colors in
        let pid v =
            let r = Str.regexp "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$" in
            Str.string_match r v 0 in

        match name with
        | "byr" -> byr value
        | "iyr" -> iyr value
        | "eyr" -> eyr value
        | "hgt" -> hgt value
        | "hcl" -> hcl value
        | "ecl" -> ecl value
        | "pid" -> pid value
        | "cid" -> true
        | x -> raise (Invalid_argument x) in

    let do_field (missing_fields, count) field =
        let (fname, fvalue) = BatString.rsplit field ":" in

        Printf.printf "%s : %s" fname fvalue;
        if validate_field fname fvalue then
            (print_string " T\n";
            (StringSet.remove fname missing_fields, count))
         else
            (print_string " F\n";
            (missing_fields, count)) in

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

Printf.printf "%d\n" (solve "input.txt");;
