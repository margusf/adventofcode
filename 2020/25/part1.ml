(*
https://adventofcode.com/2020/day/25
*)

open Printf
open Batteries

let input_test = (5764801, 17807724);;

let input = (335121, 363891);;

let solve (pub1, pub2) =
    let do_step v subj =
        (v * subj) mod 20201227 in

    let find_loopsize pub =
        let rec loop prev iter =
            if prev = pub then
                iter
            else
                loop (do_step prev 7) (iter + 1) in

        loop 1 0 in

    let transform pub iter_count =
        let rec loop prev iter =
            if iter = iter_count then
                prev
            else
                loop (do_step prev pub) (iter + 1) in

        loop 1 0 in

    let pri1 = find_loopsize pub1 in
    printf "Private 1 = %d\n" pri1;
    transform pub2 pri1
;;

printf "Result = %d\n" (solve input);;
