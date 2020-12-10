(*
https://adventofcode.com/2020/day/10
*)

open Printf

let input_test1 = [
    16;
    10;
    15;
    5;
    1;
    11;
    7;
    19;
    6;
    12;
    4;
];;

let input_test2 = [
    28;
    33;
    18;
    42;
    31;
    14;
    46;
    20;
    48;
    47;
    24;
    23;
    49;
    45;
    19;
    38;
    39;
    11;
    1;
    32;
    25;
    35;
    8;
    17;
    7;
    9;
    4;
    2;
    34;
    10;
    3;
];;

let input = [
    8;
    131;
    91;
    35;
    47;
    116;
    105;
    121;
    56;
    62;
    94;
    72;
    13;
    82;
    156;
    102;
    12;
    59;
    31;
    138;
    46;
    120;
    7;
    127;
    126;
    111;
    2;
    123;
    22;
    69;
    18;
    157;
    75;
    149;
    88;
    81;
    23;
    98;
    132;
    1;
    63;
    142;
    37;
    133;
    61;
    112;
    122;
    128;
    155;
    145;
    139;
    66;
    42;
    134;
    24;
    60;
    9;
    28;
    17;
    29;
    101;
    148;
    96;
    68;
    25;
    19;
    6;
    67;
    113;
    55;
    40;
    135;
    97;
    79;
    48;
    159;
    14;
    43;
    86;
    36;
    41;
    85;
    87;
    119;
    30;
    108;
    80;
    152;
    158;
    151;
    32;
    78;
    150;
    95;
    3;
    52;
    49;
];;

let solve lst =
    let try_adapter (previous, count1, count3) elem =
        let diff = elem - previous in
        match diff with
        | 1 -> (elem, count1 + 1, count3)
        | 2 -> printf "Found 2: %d\n" elem; (elem, count1, count3)
        | 3 -> (elem, count1, count3 + 1)
        | x -> raise (Invalid_argument (string_of_int x)) in

    let sorted = List.sort compare lst in
    List.fold_left try_adapter (0, 0, 1) sorted
;;

let (_, count1, count3) = solve input in
printf "%d * %d = %d\n" count1 count3 (count1 * count3);;
