(*
https://adventofcode.com/2020/day/3
*)

open String

let trees_test = [
    "..##.......";
    "#...#...#..";
    ".#....#..#.";
    "..#.#...#.#";
    ".#...##..#.";
    "..#.##.....";
    ".#.#.#....#";
    ".#........#";
    "#.##...#...";
    "#...##....#";
    ".#..#...#.#"
];;

let trees = [
    "......#..........##......#.####";
    ".##...###....#.....#...#.#.....";
    "#..##..#.....#............#.#.#";
    "##.#....#####..#....#..#.#.....";
    "..#.#...##.##.......#.#..#..##.";
    "##.#.......#.#.#..#...#.#...#..";
    "...#...#..#.##....##..#.#......";
    ".......##.#.#.#.##...#.........";
    "..#...##.##...##..##.##...#....";
    ".#.#...#.....####..#.#...#.##..";
    ".#...#......##......##....#....";
    "..#.....#.........##.#...#.#...";
    "...#.#...#..##...#....#.....##.";
    "..#.....#..#..#......###.......";
    "...##.#....##..##...........#..";
    "....#......#..#....###...#.....";
    ".....#...#.#.....#..##........#";
    "....#...#....##.#.##.#...#..#.#";
    ".......##.#......##....#....#..";
    "...#.#...##..#...#..#..#..##.#.";
    "##.#...#..#..................##";
    "##........#....##...#..#..#....";
    ".#.#..............#######.##...";
    "##..#..#.#.##..#...............";
    "..#........#..#...##.......#...";
    "............##.##.#..........##";
    ".....##..#.....##..#.....#.....";
    "..#.##.###.#..##.............#.";
    ".........##...........#.....#..";
    "..#....#.#.###.#.#.......##....";
    "..###..##..#.#.##......#.#.##..";
    "##......#.#....##.#..#.#..#.#.#";
    "..##.#.###.#...#...............";
    "..####.......#...#.##....#.....";
    "..#....##...#.#.#.#....#.##..##";
    ".#...####..###....#.###...##...";
    "..#.#..........#.#..#..#.....##";
    ".#....#.........###...#.....##.";
    "..#.#.#.##........#.##.#.....#.";
    "#....#....###...#..#.........#.";
    "#..#.###....#..............#...";
    "............#....##.#......#.#.";
    "...#..#.####...............##..";
    "....##......#.#.........####..#";
    ".#....###..#.#..##........##...";
    "#..##.....###..#...............";
    "..#...........#........#...#..#";
    "......................#.#..#...";
    ".#.##.#..#.#....#...#...#.#....";
    "..#..#.........#..#.#..........";
    ".#......#####...#......#..#....";
    "..........#.....#..#.##.####.##";
    "##.##..#............#####...#.#";
    "..##..#..###......#...#...#....";
    "....#####........#.##...###....";
    "......#...##..#..#............#";
    "...#....##.##...#..#...#.......";
    "....#####.#...............###..";
    ".#....#..##....#.#.#..##.##...#";
    "...#..#..#........#.#####.....#";
    "......##.#...#..#..#.....#..###";
    "###.......#.#........#......#.#";
    "..#.#..#..#........#..#......#.";
    "...##.........#..........#.....";
    "...#..###.#.......#.#.........#";
    "....#..#.##...##.....#.....##..";
    "#.#.#.#.....##.##.###..#.#....#";
    "..#....#.....##.####..#........";
    "...#..#.##.....##.#..#....###..";
    ".#..#.....#....#...#.#.......##";
    "..#..#.......#.#.###......#.##.";
    ".###.####....##............##.#";
    "#....###.#......##.#......##.#.";
    ".##...........#.#....#.........";
    "#.##..##...#...........###....#";
    "#.#..#...#.#..#..###.#.##...#..";
    "..#...#.#..##....#..#..#.......";
    "#..##..#.####...#...#..####.##.";
    "###..#.##....#...#.###..##...##";
    "##..#..#.#....#.....##.......#.";
    "..#..##.##.#.......###.#.....#.";
    "..........#.####....#.......#..";
    "#...#.#..#.......##......##..##";
    "##...##.##..###...............#";
    "....##.#...#.......##...##..#..";
    ".#.........#...#.#...##.#.....#";
    ".#...#.#..#...#..##....#..#...#";
    ".#.#...#..#..###...##....#.....";
    ".........#.#...####..#...#.#...";
    "...#.............#.#..........#";
    "...#...#..##.#........#.#......";
    "...#...#.....#....#..###.##.###";
    ".#.#........#....#...#.###.#.#.";
    "##.....#.......#..##.#....#..##";
    "...###...#.#.#.#....#.#....#...";
    "#...#.#.......##.#..#....#.#...";
    "#...#......###.....###........#";
    "..#.##...##....#...#....#.#....";
    "#....#..###....##.#......##...#";
    "##.#...#..........#.##....#..#.";
    ".##....#............###.#...#..";
    "###.##.#####.##.....##..#####..";
    "..###.###.......#.#...#....#...";
    ".#...#....####.........#.......";
    "..##.#.#......#....#.#....#.#.#";
    "#.####.....#....#..#.....#.##..";
    "###.###.##...##.#.#.#.....#.#..";
    ".......#.....#.......##.#.....#";
    "#..#.##...#........#.#.......##";
    "#.#........#...#....#..........";
    "..#....##.#......#..#..........";
    "#....##.....#.....#.##.#...#...";
    "....#.#.....#....####...#.#.##.";
    "......#.......##...##.#......#.";
    ".#.........##...#..#..##..#....";
    ".#...##.....##.#....#..........";
    "....#.###..##..#...#..........#";
    "......#...#.#.#........##......";
    ".#..........#.#.....#..##..#.#.";
    ".......###.#......#....#.#..#..";
    "..##.......#....#....#.#...##.#";
    "#.##.#.......#..###..##...#.#..";
    "......####....#.#.....#...#..#.";
    "#.##.###..#..#.#.....###..#.#.#";
    "#.#.#..#.#..##...#...#..##.###.";
    "....##..##.#...............#.#.";
    "..###.#.#.##..#....##.......#..";
    "#.#....#..........##......#####";
    ".#.#.......##.#.#......##..#.#.";
    "......#.###.##.#..#....#.##....";
    "..###........#.......##.#.#....";
    ".#..##.............#.##.###...#";
    ".#####...#......#.......##.....";
    "##..###.#...#....#..#....#.#..#";
    ".#.........###.##.....##.....##";
    ".##.#....#..#.#..##..#....##...";
    ".#..#..#......###...#.......#..";
    "#.#...#.....#..#.#.#..#..###...";
    "....#....#..#..#....#..#.#.#...";
    "......#.......#.#.#.#.....#....";
    "###...#...#......#..#.#.#..#.#.";
    "#...##.##.##........##....#....";
    ".....#.......#...#...#.#.#....#";
    "...##.....##.#...#.#.#.#..#..#.";
    ".#.......##...........#...#.##.";
    ".##..........#......#.#...###..";
    ".....##...#.....#...#......#...";
    "...........#.....#..#...#..#.#.";
    "#.....##..#...........##....#..";
    "#.##...###.###....##..#..#....#";
    "#.#.##...##....###....##.##....";
    ".#..###.....#......#...#...#..#";
    "..#...#....#.#.###.#..#......#.";
    "......#.........#..#.##...#...#";
    "..#.#....##.#..##..##...#....#.";
    "#.....#....##.........##.#.....";
    "...#...#..###.###......##...###";
    ".##.###...##..#.##....##.#..#..";
    "..#..#.......#................#";
    ".....#..#.#.#..........##..#...";
    "......###.#.#............#..#.#";
    "..#.##.....##....#...#...#.#...";
    "..#......##...#...##........#..";
    "#.....#.....#..#......#.###...#";
    "....#..#.#.....#...#....#.#...#";
    "#.......#..#...##..#.#..#.##...";
    "..#......###...#.........##...#";
    "...#.......##.....#..##........";
    ".#....#.#.....##.#.#...........";
    "##..#..#...#.##.#.#.#.#.#..##.#";
    "##...####.#.#.##...#..#......#.";
    "#.##..####.##.#.........#...###";
    "#...#.......#.#..####.#.#.#....";
    "#....#........#........#.......";
    "..#..####.....#....##...###.##.";
    "...#.#..####.........#....#.##.";
    "##.#...#...#..#.#..##.....##...";
    "....#.........#.##........##.#.";
    "##...#......#....#..#....#....#";
    "###.....#......##...#...##...#.";
    "#.##...............#.......#...";
    ".##.#...#..#....#.#.....###..#.";
    ".....##...#.##.....##...#....#.";
    "#.#..#..........#####..##......";
    "..#.........##...#.........#.##";
    "...#..##.#.#..#......#..###.###";
    "#..#...#.#...##..........#.....";
    ".###..#....###.....#....#..###.";
    "#..#....#...#........##.....#..";
    ".#..###........#....#..####..##";
    ".#..#.#.#.......##.#..##.#..##.";
    "..#..###......##....#..#..#..#.";
    ".......###..##....#......#...##";
    "#........#.##.............##.#.";
    "...#.#.#....##....##.###...#...";
    "..#.....#..##..#.#.......#.####";
    ".#......##.........##...#.....#";
    ".#.###........##....###.#.#...#";
    "##...#.#....#.....##.......#..#";
    "#...........#...........####...";
    "#..#.#..##..#...#....#.##....#.";
    "................##.............";
    "..##...#.#....##....#...#......";
    ".#.....#....#....#..#..#.#..##.";
    ".....######.#.#.##.###.#.......";
    "..#####....#..#...........#.#..";
    ".......#..#..##.#...###.#.#.###";
    "###...#...#..##.#.##..#...#..#.";
    ".#..#..............#...........";
    ".#.....#.....##....#....##..#..";
    "....#####.#....#......#.......#";
    ".#.#.....##.####..#...#.......#";
    ".#...##.#.......#.....##.#..##.";
    "..........#...#....###....#...#";
    "..#......#...#...#..#.#........";
    ".......#.......#..####..##.....";
    ".#..#.....###...#...#...#...#..";
    "##..#.......#.#...#..#..#.##..#";
    "#..#...#.#.....#.##.#........#.";
    "......#......#.#..###.##..###..";
    ".#..#..#.##.#...........#...##.";
    ".#....#...#.#..#.#.#...##.#..#.";
    "##.#....#..#..#.#...#......#.#.";
    "..#.#............##...#........";
    "...####...#...#.....##..#...###";
    "....###.......###.##..#.###....";
    "#......#.#....#.#.##.#.##..###.";
    ".....##.....#..##.....##....#..";
    "..#...#..##.#.##.#.#.#.......##";
    "#....#..##.......#......#..#.##";
    "#.....##...#..##......##.#.#..#";
    "....#..##..#.##...#.#.##..#..##";
    "#..#...##....##..#...#....#...#";
    ".##.#.#....#.....#........##.#.";
    "..##..#....#........#.....#....";
    ".##.#..##...#.....#...###.....#";
    "#..#..#........#..#.....#.#.#.#";
    "..##..###.#..#...#.#......#..#.";
    "#.....#.....#.###......##..#.#.";
    ".........#...##.........#...#..";
    ".##.#.##......#.#...###..#....#";
    "...##.#..###........##......#..";
    "...#.#...#......#.#.#....#..#..";
    "..####.........#..#....#.......";
    "#..#.........##.#.##....#.....#";
    "..#..#..#.#........#.###.......";
    "##.#..#..#....#...##.......#..#";
    "..#.#.....#.............#...##.";
    "..........#...##.....#..#.#..#.";
    "....#..#...#..##..#...##.#.....";
    "##....#......#..#.....#..#.....";
    "...#.#.#.#...........##...#.#..";
    "....#.###...#............#.....";
    ".#.#.#.......#.#......#....#.#.";
    "#.#.#.#..##.#..#..##...##.#..#.";
    ".#.##....##..#........#....#...";
    "####...#....#.#...#..#..###...#";
    ".....#.#.##.......##..#.######.";
    ".......#.#.#.....#.#..##....#..";
    "..#....#.#..#.#.#..#..#........";
    ".....##......#.........#.#...##";
    "#....##.#.....#..........#.#...";
    "#...#.#..#.#..#.#....#..#.#....";
    "....##........#................";
    "###.#.#...#..##...#...#.##...#.";
    "...#....###..#..##..#..#.......";
    ".....#..........#.#........##.#";
    ".#........#.##.#..##..#...#...#";
    "..##....#...#.#.........##.#...";
    "......#...#......#.....#.......";
    "....##.##..#.##...#.#.#.##.#.#.";
    "..#...#.....#.#....##.#........";
    ".#.#.......#.......###..#..#...";
    "#...#..#..#..##....#...#.....#.";
    ".#..####.##.....##.........#.#.";
    "#...###.......#...####..##.....";
    "#.##.#....#.#.##.......#...#...";
    "..#.......#.#.##.##..#...##....";
    ".#.......#.#..#.....#.....#.#..";
    "..#..#.......##.....#.#.....#.#";
    "#...###..#..#..##...#.....#..##";
    "......#................#.......";
    "..#.....##..#.......#...#...##.";
    "...##...####.#..#...#.......##.";
    "..#...#..#...#...#..#..#####...";
    "#..#...#....#....#...........#.";
    "..#.......#..#.##...##..###...#";
    ".#..#..#......##...#....#......";
    "...#..##....#..........#.....#.";
    "###...#.#......#.#.....#.....##";
    "#.#..#.....#........#.##.#.##..";
    "....#...#.....#..#.......#.#...";
    "#.#...##....#..#.....#...#.#.#.";
    ".#......#...##..#.......#......";
    "...#...#.#.#.###.#..#.#..#.....";
    "###...#..###.#...#..##...####..";
    ".#.#.#..#........#..#......#..#";
    ".#..#....#......#....#.#...#...";
    ".##..........###...##.....#.#..";
    ".#...#.#.##.##..###.#...#..###.";
    "......#......#......#.##......#";
    "..#.##..#.#..#....##..##...#...";
    ".#......#..#...##....#...#.....";
    ".#.....#.##..........#..#......";
    "###.#..#.##..#..##...#..#...#..";
    "#.....###........#.#..##.#.....";
    ".....#.......##.....##.....#.##";
    "...##.#......####....##........";
    "..#..#..#....#.##.....##.####..";
    "...#..#....#.#..#.#..#.#.#..#..";
    "#..........#....#.#.#.#...#..#.";
    "...####.##...#..#.......#.#..##";
    "#........#..#..................";
    ".#..#....#.#.#..#..........#...";
    "###...#....####....#......#..#.";
    "#.........####..#..#...........";
    ".....##..#..##.##.##.#..#.....#";
    ".#..#.#.##..#..#.#.#.##.###....";
    "......##......#...#.##....#..#.";
    ".#.#....#..#......#..#...###...";
    ".##...#......##...###...#.#...#";
    ".......#.#....#............#..#";
    ".#..##.#.######...#...#......#."
];;


let rec count_hits lst pos result =
    let next_pos size pos =
        (pos + 3) mod size in
    let is_hit str pos =
        let c = get str pos in
        match c with
        | '#' -> 1
        | '.' -> 0 in
    match lst with
    | [] -> result
    | h :: t -> count_hits t (next_pos (length h) pos) (result + is_hit h pos)
;;

Printf.printf "Result = %d\n" (count_hits trees 0 0);;