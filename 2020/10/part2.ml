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

let mapping = Array.of_list [
    1;
    1;
    2;
    4;
    7;
    13;
    24;
    44;
    81;
    149;
    274;
    504;
    927;
    1705;
    3136;
    5768;
    10609;
    19513;
    35890;
    66012;
    121415;
];;

let solve lst =
    let rec make_diffs lst prev =
        match lst with
        | [] -> []
        | h :: t -> (h - prev) :: (make_diffs t h) in

    let rec loop lst prev_count ret =
        match lst with
        | [] -> ret
        | 1 :: t -> loop t (prev_count + 1) ret
        | 3 :: t -> loop t 0 (ret * mapping.(prev_count))
        | _ -> raise (Invalid_argument "2") in

    let sorted = List.sort compare lst in
    let diffs = List.append (make_diffs sorted 0) [3] in
    loop diffs 0 1
;;

printf "result = %d;\n" (solve input);;
