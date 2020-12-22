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

let input_test2 = (
    [
        43;
        19
    ],
    [
        2;
        29;
        14
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
        printf "Make decks %d %d\n" (List.length lst1) (List.length lst2);
        (BatQueue.of_enum (BatList.enum lst1), BatQueue.of_enum (BatList.enum lst2)) in

    let get_play_state deck1 deck2 =
        (BatList.of_enum (BatQueue.enum (BatQueue.copy deck1)),
                BatList.of_enum (BatQueue.enum (BatQueue.copy deck2))) in

    let copy_deck deck count =
        let enum = BatQueue.enum (BatQueue.copy deck) in
        BatQueue.of_enum (BatEnum.take count enum) in

    let rec play_game deck1 deck2 =
        let rec play_loop used =
            printf "Play Loop, used = %d, decks = %d, %d\n"
                    (BatSet.cardinal used)
                    (BatQueue.length deck1)
                    (BatQueue.length deck2);
            let play_state = get_play_state deck1 deck2 in
            let new_used = BatSet.add play_state used in
            if BatSet.mem play_state used then begin
                printf "Loop detected, p1 wins\n";
                (* p1 instawins, copy all cards to p1 *)
                BatQueue.transfer deck2 deck1
            end else if (BatQueue.is_empty deck1) || (BatQueue.is_empty deck2) then
                printf "Game finished, p1 wins = %b\n" (BatQueue.is_empty deck2)
            else begin
                let card1 = BatQueue.take deck1 in
                let card2 = BatQueue.take deck2 in
                printf "Play %d vs %d\n" card1 card2;
                if card1 <= BatQueue.length deck1 && card2 <= BatQueue.length deck2 then begin
                    printf "Playing subgame\n";
                    let new_deck1 = copy_deck deck1 card1 in
                    let new_deck2 = copy_deck deck2 card2 in

                    play_game new_deck1 new_deck2;
                    if BatQueue.is_empty new_deck1 then begin
                        printf "Subgame finished, p2 won\n";
                        BatQueue.push card2 deck2;
                        BatQueue.push card1 deck2
                    end else begin
                        printf "Subgame finished, p1 won\n";
                        BatQueue.push card1 deck1;
                        BatQueue.push card2 deck1
                    end;
                    play_loop new_used
                end else begin
                    if card1 > card2 then begin
                        BatQueue.push card1 deck1;
                        BatQueue.push card2 deck1
                    end else begin
                        BatQueue.push card2 deck2;
                        BatQueue.push card1 deck2
                    end;
                    play_loop new_used
                end
            end in

        printf "PLAY GAME\n";
        play_loop BatSet.empty in

    let score_deck deck =
        let (sum, _) = BatQueue.fold
                (fun (sum, multiplier) card -> (sum + card * multiplier, multiplier - 1))
                (0, BatQueue.length deck)
                deck in
        sum in

    let (deck1, deck2) = make_decks input in
    play_game deck1 deck2;
    let score1 = score_deck deck1 in
    let score2 = score_deck deck2 in
    max score1 score2;;

printf "Result = %d\n" (solve input);;
