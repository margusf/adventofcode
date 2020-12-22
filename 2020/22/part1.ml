(*
https://adventofcode.com/2020/day/22
*)

open Printf
open Batteries

let input_test = (
    [
        9;
        2;
        6;
        3;
        1;
    ],
    [
        5;
        8;
        4;
        7;
        10;
    ]);;

let input = (
    [
        40;
        28;
        39;
        7;
        6;
        16;
        1;
        27;
        38;
        8;
        15;
        3;
        26;
        9;
        30;
        5;
        50;
        17;
        20;
        45;
        34;
        10;
        21;
        14;
        43;
    ],
    [
        4;
        49;
        35;
        11;
        32;
        12;
        48;
        23;
        47;
        22;
        46;
        13;
        18;
        41;
        24;
        36;
        37;
        44;
        19;
        42;
        33;
        25;
        2;
        29;
        31;
    ]);;


let solve input =
    let make_decks (lst1, lst2) =
        (BatQueue.of_enum (BatList.enum lst1), BatQueue.of_enum (BatList.enum lst2)) in

    let rec play_game (deck1, deck2) =
        if (BatQueue.is_empty deck1) || (BatQueue.is_empty deck2) then
            ()
        else
            let card1 = BatQueue.take deck1 in
            let card2 = BatQueue.take deck2 in
            printf "Play %d vs %d\n" card1 card2;
            if card1 > card2 then begin
                BatQueue.push card1 deck1;
                BatQueue.push card2 deck1
            end else begin
                BatQueue.push card2 deck2;
                BatQueue.push card1 deck2
            end;
            play_game (deck1, deck2) in

    let score_deck deck =
        let (sum, _) = BatQueue.fold
                (fun (sum, multiplier) card -> (sum + card * multiplier, multiplier - 1))
                (0, BatQueue.length deck)
                deck in
        sum in

    let (deck1, deck2) = make_decks input in
    play_game (deck1, deck2);
    let score1 = score_deck deck1 in
    let score2 = score_deck deck2 in
    max score1 score2;;

printf "Result = %d\n" (solve input);;
