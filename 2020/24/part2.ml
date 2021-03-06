(*
https://adventofcode.com/2020/day/24
*)

open Printf
open Batteries

let input_test = [
   "sesenwnenenewseeswwswswwnenewsewsw";
   "neeenesenwnwwswnenewnwwsewnenwseswesw";
   "seswneswswsenwwnwse";
   "nwnwneseeswswnenewneswwnewseswneseene";
   "swweswneswnenwsewnwneneseenw";
   "eesenwseswswnenwswnwnwsewwnwsene";
   "sewnenenenesenwsewnenwwwse";
   "wenwwweseeeweswwwnwwe";
   "wsweesenenewnwwnwsenewsenwwsesesenwne";
   "neeswseenwwswnwswswnw";
   "nenwswwsewswnenenewsenwsenwnesesenew";
   "enewnwewneswsewnwswenweswnenwsenwsw";
   "sweneswneswneneenwnewenewwneswswnese";
   "swwesenesewenwneswnwwneseswwne";
   "enesenwswwswneneswsenwnewswseenwsese";
   "wnwnesenesenenwwnenwsewesewsesesew";
   "nenewswnwewswnenesenwnesewesw";
   "eneswnwswnwsenenwnwnwwseeswneewsenese";
   "neswnwewnwnwseenwseesewsenwsweewe";
   "wseweeenwnesenwwwswnew";
];;

let input = [
   "swswswswswnwswswswsweswsesw";
   "wneswseseneswnweneswwswseswwnwseswswe";
   "ewswswswwswnwnwsweswwwwwswwwswwsw";
   "senwseseswseseswswesewseseseseseswsese";
   "seswnwseseswseseswswseseswswseswesese";
   "swnwnenwnenwnwnwswnwnwnwnwnwnwnwenwnwnw";
   "seseeseneswnwseseneeeseeswsewseseese";
   "senenwnwnewnwnwnwnenwnenwnwnwnenwswnenw";
   "eneneneweeneneeneneneeneneenesew";
   "nenesenewwenwseseseseswsesewneseweswse";
   "swswweswswswswswswswseswnenwswswsweswsw";
   "nwswnwnwnwnwnwnenenenwnwnw";
   "seseeneseeesenesewseswsewseeeese";
   "wwseswwnwneswsewswwswswswwswswswswne";
   "wwewwnwwwswsenwnewnwwnwsewwww";
   "wnwnwnwenwnwnwnenwnweswnwnwwswnwnwnw";
   "neeneneswnenenenenwnenenwnenenenenewnwe";
   "nenwsenwnwnwnwnwswswnwnwnwenwnwnwnwnwnw";
   "swenwsenweenwswnwswseseeswswesenwne";
   "ewseswsewswneewneneneeneseneeswnwsw";
   "nwswnwswewswswswesesweswswse";
   "swswswswenwswswswwswneswneswswswsesww";
   "seswswseseswswsenwswse";
   "wwwwwwwswsewnewwnwwwwwew";
   "swwwswewswswswswswnwswswswswseswsww";
   "sesewsewneseneseseseswswseeseseswnesw";
   "wwswswwwwswwewwswwwsewnwnww";
   "nwnenwnenenwnenwnwnwsenwnwne";
   "neewwwneswseseeeneeseesesesewee";
   "senwneeneneenewneneneneneneneswnenenene";
   "swswswneswwswswwswswsewneswesw";
   "nenenenenwnenesewnenenenenenenwnwnenwne";
   "nwnwnwsesenwnwwsewnwnenwnewnwnwwwnwnw";
   "nwnenwwnweswseseeswenenwweneneeswswsw";
   "eeesweeeenweseseweeneeseeese";
   "swswseseswseswnwseseswswswseseseneswswsese";
   "swnwnwewnewwwwnwnwwnwwswnwse";
   "sesenwnenenwnenwnwnwnenwnewnwnwnw";
   "neswneneeneneenenenenewnwneneswnenwewne";
   "nwnenwswnwenwnwnwnwnwnwnwswnwnwnwnwnwnwnw";
   "nwwwewwneewswneswnwwwwwsewsew";
   "wswswswswswweswwwwswseswswswneswswe";
   "wnwwnwnwnewnwnwnwwnwwsenwwnwnwwnw";
   "wswwwswswwwwnesew";
   "nwnwneenenwnwswnwnwnwswnwsenwnwwenenenwnw";
   "swsewseseswswseswneseseswseseswseseseese";
   "swseswseswnwsewneweseseseeswsesesesew";
   "sweswwwneswswwswesenenwsweswswswswse";
   "wswseneeseweeeeweseesenwnwseew";
   "wwnwnwwwnwnwnwnwwnwnwnwnwswwnwwe";
   "swnwnwwseneneneneneneswswswnewenwsese";
   "seseseseseesesenwseseneesewswseswnwnw";
   "neswswsenwesenesenesenesesewwneswnwswnwne";
   "nesesesenenwnwnwnenwnwswnwneenewnenwne";
   "neneneneneeneenewseewnenesenenenenene";
   "eseseseseneseeseseesenwseseseseseseesww";
   "neeenenesweeeeeeweneeeeeene";
   "enwwwwwsewwwnwnwwnwwwwwnwnwnw";
   "neseeeeseseseeseeseneeseseseenwwsw";
   "enenesewneenenenwseseneeweeswenwene";
   "wneneseneneneenenenenenwnenenenenenene";
   "wnwneeswewnwnwseswswsweswsenewswsw";
   "eseeswsenwenweseseeseseseesenesesese";
   "wswswseseseseneenesesesewsesenewsew";
   "wnwwwwwwewwswwwwwwwswsww";
   "swenewwwseswwnesesenewneswseneswswswsw";
   "eswseseseseesewseseneseenwsewseee";
   "swneneswneneneneeneeenwneeenenwwenene";
   "neswnenesenenwnenenenenenenenenenenenwnene";
   "sesesesesewsesesweseseseseseseesesenee";
   "nwnwnwnwnwnwswnwnesewnwnwnwwnwnwnwnenw";
   "eenesewwwwnwnwwwwnwwwsw";
   "swnenwwswswswneseeseneswneseswwseswsw";
   "neneenenwnenenwnwnwseswnwnwnenwnenenenenw";
   "neenenwneneseneeneneeneneeeeneswnee";
   "swswswswneseswwswswseswswweneenwnew";
   "neseewsesesewswsese";
   "enenweneeneneeeeneeswswneneeeneenw";
   "seeeeneeeenwseewswenewnwseesew";
   "sweeeneesenweswewsweseeseenenwee";
   "seesesewswenenwnweswsewseneenwsesw";
   "swwswwswwwneswwsww";
   "wwwwwwwwwwwwewwwweww";
   "neneneseneeneneeswneenesenwnewnenwe";
   "nwnwnwnwnwnenwnwnwnwsenwnwnwwnwnwnwnwnw";
   "eeeeeseseeenweeeesweeenee";
   "nwnenwnwneneseneswnenwnenwwnwnwnenenwnwnw";
   "sewswswwsewwneesewnewnewne";
   "nwnwwnweswnwneswswnwnwnwnenwnweswenwe";
   "nwneenenenenwneneneeneseeneneswnenenene";
   "swwwwswswswswnewwwwwsw";
   "neneneneeneeneeneneeewnwneswneneesene";
   "nenenewnenenenenenenewnenwnwneesenene";
   "swnwswswwwswswwweswnewswswwswnwswsee";
   "neseseeseseeseseseeeseseewesewse";
   "nwnenenesesenwnwswnwwswwnwnwnenwnwsew";
   "seesesewseneseseseseseseswsesesenwswsese";
   "nwsenwwnenwneseswwwnwwnewesewwswnw";
   "sesesesenesesesesesewsewsesesesesesesesee";
   "seswwnwseeswnwwneeneseweeseenee";
   "nwseenwnwnwnewswnw";
   "nwnwnenwsenwnwnwnwnww";
   "wnwwwnwenwnwwsenewnwnwnwwsewwnenw";
   "sesenwsenwseswseseseseeewsenesesesesese";
   "seeneeeeseeenwnwsweeeeneeene";
   "eeeeseeeeswneeneeneesweenew";
   "swseneswswnwseswswseswswswswneswswswsesw";
   "nesesweneseseneeweesenwnwseeswnwswse";
   "senwswnenenwnwneeesw";
   "nweeseeenweesweeeeeseeswese";
   "sweneenwneeneeneesenwnwnwnesweswswnee";
   "swswnwewswnwswwwwswwswwwewswswswe";
   "swswswsenwswwswsweswswnwsweswswesesw";
   "swswsesesenenwewseseswseeswseseswsesesw";
   "seswnewewswnenwenwnenenenenenenwwene";
   "wnwswswwwsewnwnwswweseswsenewwww";
   "wswswweswwwnwsewwswswwswsw";
   "swswseswsweswswswswswswswswswswnwswswsw";
   "nenwnewnenenenwnenenwnwnenwneneneenese";
   "seswswswswseneswsesw";
   "neseneneenewsenwwnwnwwswwnwwee";
   "swswswwswswswwnwnewwwneeneswwnwse";
   "nwswwwwnwswesewsenwseswwswwwwwnew";
   "wsenweeeseeeseeseeeeeeeseese";
   "seseseseseseeswseesesesenesesewnesese";
   "nenesesewsenwneseseseneseswseswwswsesese";
   "sewseseewsenenweewswseseneeswsee";
   "swseewnwsenwnenwswneswnwwswnesenwnewnw";
   "neeeewseseneswnenwneneweneneseseew";
   "nenwnenwswneneenenwnesesenewneneswnenwnw";
   "nenewnenesweneneenesenenenenenenenwneee";
   "wnwsenwnwsenenenesesesenesewnwnwnwnwnenw";
   "nwswswswnewnwsweseswnesenweswenwesww";
   "nwnwnenesenwnwwnwneswnenwnwnwnwnwenwnw";
   "seswseseswswneswnwswseswsesesenwswswswswsw";
   "nenwnwsenwnwnwnenwnenenenwsenwwnwnwnenwnwne";
   "eswwswswwswswswswswswwsww";
   "nenesenwenwsweesweeneenw";
   "nenenesenenenenwnwnenenewnenwswnwnenenwne";
   "eeseeeneseewneeeeseeweeeese";
   "swswwseseswsenwseeswswswsesesw";
   "nwnwsenwsenenenwswseswsewseswswwnwswnew";
   "nwwnwnwnwenwnwwwnwwnwwwnwnwwwsw";
   "wewnwwswswnwwnwnwnwnwewswnwesenwwne";
   "nenenwnwnwnwenewenwsenwnenenwnwnewnwne";
   "seseseenwseseseseseswwseswnwsee";
   "swseswseseseswsesesesenwseseswsese";
   "swswneseswseswwswswswseswsweswswswswswsw";
   "seneneeseseswseseswsenwseeseseswsesee";
   "nwneneseneneneneneenesenewseneewnene";
   "nwwnenwnenenwnwsenwnwsenwnwnwnwnenenenw";
   "wwnwwsewwwwwsewwwnwnwnwnwwnese";
   "seseswnwseswseswsesesenwsesesese";
   "wnweswsenwnwswnenwnwnwswwsewnenwnwwnw";
   "wseswnwseswswseswswsenwswswsesene";
   "neeeneenwneneeeeneeesweenenweesw";
   "wwwnwnwwwwwwwwwnwwwwesew";
   "neseeneneneneneneneneneneenwnenwneneswnene";
   "neeeesenweeswenewne";
   "nwswnwenwwwwnwswwwwwwnwnwenwnwe";
   "wwwwswwwwwwswwwwwewsw";
   "eeseeswswswneeeweewneneneenenene";
   "swswswesesesenwswseswnese";
   "sesewswseseseswseseseseseseseseseswnese";
   "neeeeeneeweeeesweeenenwe";
   "nwwnesenwneneswenenenenenwnwnenwnwwnene";
   "neeenwneneswneneneswneeeneswneene";
   "nwwewwnwnwnwswwswwwnwwwwwwnew";
   "nenwnwnwwswnwwnwnenwesee";
   "wswswswswwseswswswswnewswswnwswwswsww";
   "neneneneswsenenenwne";
   "neeeweeeeeeeseneneneneeesewe";
   "nwswnwnwnwnwenwneeswnwnwwnwnw";
   "swsesenwseswseseswwseneseneseseesesesese";
   "nweeweeeeeseeseeswseeee";
   "nwsesesesesewesenwnenwswneeseneswsesw";
   "nwneneenenewsweeneneneneneneeewnene";
   "swseeseneswwneswnwwswneewnesw";
   "neseseswswseswseswseswsenwsweseswsesesese";
   "seswswwswwwwewswswwnewwwswswnww";
   "senwseesesesesesesesesesesesenwsenwsese";
   "seswnwsesesewseseneseseseesenwsesesesewse";
   "neeenwnesweenwneeeeeeeeeneswene";
   "seenwsenweswnwswwnwnwnwnwnewnwswenw";
   "swswswswswswswswswswwswswseswne";
   "enwswswnwswneswsewnwswseneseenwwnwsw";
   "swneswnenenwnwneeeneneneeswneswnenesew";
   "nwswnwwswwseewseewsenenwnwwwnwwwse";
   "eeeseesenesesewseeseswseneesesesese";
   "swnewswseswwwnwwewew";
   "eeeeeeseeeeeswnwe";
   "nenenenenwnwneeneswwneeneneneswnenenesw";
   "eswwswswswsweswnw";
   "newnwsenwnwnenwnwenwnenenwnwneswnwwswnene";
   "eeneeneneeeneeweeswneeneseeewne";
   "nwwnenewenenwnwnwnwe";
   "neneswwneswneneneswene";
   "swwneneweneeswwneenesenwnwenenese";
   "seesewsesesesesenesesesesewsesesesesese";
   "nesenwesesesenwwswsesewesesesesesesesese";
   "nwsenwneeweenweswnwnwnwwwenwwne";
   "seswswswnwswseswswswseswenwswswswswnesw";
   "swnwsesweswswseswwswswwseeswswsenesesw";
   "ewseseneeseeeneeenwnweeesenwne";
   "neswswswswswswswswseseswwenwswnwswswsw";
   "nwswswwseeswswswswswswswswswswswswswne";
   "nenwnenenenenwnenenenewnenenwneenwnw";
   "senweeeenwseeseeenwseeseseeeese";
   "seseseswseseswswseseseneneseseswswwseswswse";
   "swswwswwswwswswswswwwwswwswwe";
   "sesesewseswseseseseseseseesesesese";
   "nwwnwnwnwnwenwwnwnwnwwnwnw";
   "nwnwnwnwnewnenwnenwnenwnesenenwnenenwnw";
   "nwwseneesenwwswwneseenwseswwwnwe";
   "enwnwswsesenwswsesweeseseseseewnwswse";
   "eeeeseseeesweneeeewewwse";
   "ewneneeseeneseswswnwswwseswesesene";
   "eeeneeeneeweeneeeeeeweese";
   "nwnesenwnwnewnwnenwnenwnwneeswnwnwneswe";
   "swseweeeseseneeeeseneseseeneeew";
   "seewnenenwenesesewnwnwnwseswe";
   "nesenewsesweseseswwsesewneswswswese";
   "neenenesenwneeneeneswswneneenenenene";
   "wneswsenenwsewseeswseswswnesewswnese";
   "senwnwnenenewwwnenwenwnenwseenenenw";
   "nwwwsewneswswwnenwswnewnenwseesenenwnw";
   "eneneneeneseswweeneeewnweneee";
   "seenenwswswswnwnwwseneneseeeeeswsw";
   "nenenesenewnenenwneneneneneneenenenenene";
   "wswwwwwwswwswwnwwswwswewwew";
   "neenweneneseeneswwswnew";
   "eeeenweeseneeneeeeeneeswee";
   "nwsesewsesesewnwseswneseseesesesenwse";
   "wnwnenenenenwwesweeneneswsenwnwswsene";
   "swswwswswswswswswwsweneswnwwnwswesw";
   "wwewwwwnwww";
   "eeeeneswsweneenwnenwneneneeneene";
   "sewseswswswswswseseswseewnwnenwsw";
   "neeneseweeneeneesweeneenweese";
   "sweeeeeeneeeeeeee";
   "wnwnwwnenewwnwnwwsewswwwwnwnwwnww";
   "seswwseseseswnwswseeswswswnwseswseswswse";
   "eseswnenwweeswnwwneeswnese";
   "sesesewnewwwwneneww";
   "nweneesweneesenewnenw";
   "eesesesenwseseeseseseseseese";
   "neswswnewenenenenenenwenenenwne";
   "swnenwnwnwsenwwnwwwneewwnwwwnenwwse";
   "seswswsesewweneenenwsesewswsw";
   "seeseeeswenwswnweeeswnweeeeee";
   "sweswneswwwwswne";
   "seseswseswswseneswswseswseswneswswseswsese";
   "wnwwnwnwnwwwwwnwwwwnwenww";
   "wswseswswswswsweseswswnwseswneswswswsw";
   "enwwnwnwwseeesewnww";
   "nwnwsweseswswswwswswseswswweswnwswe";
   "swseswswswwwswswnewsw";
   "wswnewnewnewenwsewwwseseswnwnwwse";
   "eseneseseeesesweesesewseseesesewse";
   "neewseeseeeswswsesesenenw";
   "newneweenenesewseewwnwwsenesesee";
   "wnenwwswwswswnwnweeenwneswseenwnwnw";
   "swnwnwnwnwenwnenwnwnwnwnwnw";
   "neneesewnweseeneeneseewsewnwswne";
   "wwwwwenwwwwwwswwwwwnwsew";
   "seesweneseeseweeseneseeseseeee";
   "nwnwnwsewwnwwnwnwnwnwnwnwnwnwnwneswwse";
   "newnwswnwwnwnwnwnwwwnwnwnwwwnwnwenw";
   "seweeeseeeseeseeeeeseswnweseene";
   "eseseeeeeswesesweeseneeesenesee";
   "eseswwsesenwseseswseseseeswsenese";
   "wnwnwnwnwnenwnwenenw";
   "neneenesweswsenwseneneswnenwwwneswneww";
   "wswneswewswseswswswswwsw";
   "seseeneeseswsewneswnwseneswswneenwnee";
   "nwwnesenwsenwnenwnwnesewnwswswnwneswwnw";
   "enweneeeneeneneeneneswseneseenwne";
   "nwnenwnenenwnwnwsenwnwnewwnwnenwene";
   "wwnwneswwewnwweswwneseswwswnesew";
   "enewneewsewenenenenesenenewene";
   "nwsenwnwnwnenenenenenenenwnwnwnenw";
   "nenenenenenwnwneneneswne";
   "enwneseeneneneneswwneeneneswe";
   "swswwswsewsewwwwneeeswwwswnwww";
   "swnwseenenenwenwnenwnwwnwswneswnwnwnenw";
   "wswswwswwwswswswswswswwswweswnwsw";
   "seseswwneswwswswswswswswneswswswswswsww";
   "seseswnwnwnwnwnwnwnwnwwewnwnw";
   "wwnwnwwwnwnewnwsewwnwnwnwnwnwewsw";
   "wseswnwwneswnenewwseewseswswwwne";
   "wswwwwwwwwwnwewwwnw";
   "senwswseneseseseswswswsweswswseswseswsw";
   "wwwswwwswwwsewnwswwnewwswswswsw";
   "swseswswwswseswswseswseseswseswne";
   "ewswswsweswswnewswseneseswseswsenwnwe";
   "wwwwwsenwwwwwwwwwwne";
   "nwsewwwwsewnwwwnwne";
   "swswswswswnewswwwwwswswswswswsw";
   "nwwseenwsenenwwnwnwnwnwenwwnwnwwsw";
   "eswseswswsenenenenwswseswswswseswsenwswse";
   "wswwswwswwsewwwswswswwnwwnene";
   "sesesesesesenewwswwseneseseeswsesesese";
   "swnwnwnwnwnwnwnwnenwnwnwenenwnwnwwnwnwnw";
   "wneenenwneenwwnenwnwnwneneneneeneswnw";
   "swwwwwwwwwwwwwwwnewseww";
   "senwneswseswsesenwsesee";
   "neswwseseeswseseswswsesww";
   "nwswseswseseswseneeseseneseseseswseswnesw";
   "eneeeneneneewenenenenenweenesene";
   "wnwwnenenwnenwswnwnenweenwnwnenese";
   "wswseswswneswswswnweswswsweseseswsene";
   "senwwseneseeesewnesewswsewsesenwnese";
   "esenesesesesesesesesesesewsesesewsesw";
   "neeneneseswnwnesenwwnwnwswnenwne";
   "neenenwnenewneswnewneneneenenenenenwe";
   "swseswswswswseswswnwswsweswsesesesesw";
   "nwenwsewnwnenenwnwwnwnwnwswnwnwnwnwwnw";
   "wsenwwnwswneweswwweewnewesene";
   "swwnwwswwwnwnwewewewwwnwnwnww";
   "wsewwnwswsweswswswswswswswswswswswswsw";
   "seswwswwwewswewwwwnewwwwne";
   "esesesesesewneseeseseseseeswseneesesese";
   "nwnwnenenwneneeneweneneneseseswnenwnenw";
   "neneneeneswseswwnenenenenenwneneneneenene";
   "nwnwnwnwnwnenwsenwnwnwnenwnwwnwnenw";
   "eeeeeeewnweeeseeeeeeee";
   "wwwnwwnwewnwwnwwnwswnwwnwnewwnw";
   "swenwswnwswesesenwswswswswnwwnweseesw";
   "neeneneeneneswneneneswneneneneenenenene";
   "nwsenwseeeesesese";
   "wnwwnwnwwnweesewnwswnwswnwnwseswneww";
   "nwseseswseseeneseneseseseseseswswesesee";
   "neneeenenwswneeeswneneneenwnenenene";
   "seswseeswwswswswswswsweswnwse";
   "neeeneneeneswenenwnenwneeneweswnee";
   "ewewwswnwneewwnwswsewswwenew";
   "swwswswswsweswswswswswswswsenewswswsesw";
   "nwsewnwswnwnwsenwnewwnenwnwnwwwnwewnw";
   "nwseswwwwwswswswwswneswwswwwwsww";
   "nenwnwnwnenenenwnenenenenenesenenenene";
   "eseseswsweseswseswsenesesewwsesesesese";
   "sesesesesesesesenewnwseseseseseseesese";
   "nenenenesenewnesenewwenesenenwnenene";
   "wewewswwswnwwwwwwwswwww";
   "enwwnwnwnwwwswnewnwnwwnwseewnwsenew";
   "swseseenwnweswseseswswnenenwnesesewsw";
   "wwnwwwwsewwwwwwewwwwwww";
   "nwewnwnwwenwswnwnwenwnwnwnwnwsw";
   "wsesesenwsenwneeseseewseeswseseese";
   "enweneeeswnewneswewneneeneeneee";
   "seswwseswseseseswswesesesesesesesenwsese";
   "swwnwswwnewsewesesenwwewnew";
   "nweneeneseneneneenenee";
   "newnenenenenenwnenesenenenenenene";
   "wwwwwwwwswwnwswwwsewwwwwne";
   "swnwswnwenesenwnenew";
   "nwwwwwwwwwwsewwwwwnewww";
   "swwsweswnwnenwnwnenwnenewewswsenwswnese";
   "enenewneswneneneeeeneeseneneneneenee";
   "nenwnenenenenwnenwwnenenenesesenenwnewne";
   "swnenenenesweswnenwneswswnwswnwnwneswswne";
   "wwwwsesewneswwwnewwnewwnewsew";
   "seswseseseseseeseseswsesesesenenwsesesese";
   "wwwswwnwnwewwnwwnwwww";
   "seseseseswsesenwswsese";
   "swnwswswswswswnwswsweswswswswswswswseswswsw";
   "nwnenwswnenwnwnwnwsenenwnwnwenwnwnwnwnwne";
   "seneneneneenwneneswwnwswneneswnesenenw";
   "swswnewwnenwewswwswswswsesewswsww";
   "neeswwnwnwnwnwswnewenenwnwnwnwnwesewne";
   "nwswseswewnewswnwswwsweswseeneseswne";
   "sesenwswesenweenwwnwesw";
   "seeseseseswnwseseseswseseswsesesesesese";
   "esewseeesweseseseseesenenweseese";
   "nwswseenweeneeeeeseenwseenwsesesw";
   "enweeeseeesenwsweee";
   "nenwneneeeneneneeneeseeneeneneneew";
   "swswseswneswseswswswse";
   "enenenenwnwnwneswnenwnwnwnwneeneswnenenw";
   "nenenwneswnenwnwnenwnweswnwnenwnwnenenwne";
   "swseesewseeenwenwnwsenenwweseese";
   "swseenenenenenenenenenwwnwnenwwenenene";
   "nwnwnweneneewwnwsenenenenwswswnesene";
   "seeswswesenwneeneeenweswesesenww";
   "eeeeeneseeeeweeeeeesweee";
   "enwswswneswswswwswswswswseswswswswswswsw";
   "sewsesesesesesenesesesesesesesesenesesw";
   "nesenesenenewnweneeenenenenewnee";
   "swseseseswsweswsesesesweswnwswswseswsesenw";
   "eeeeeeeweeeeeeweeeeee";
   "nwnwnwnwnenwnenwnwesenwwnwnwnenwnwnwnw";
   "wswwnwewnwwwswswwwswswswwswswe";
   "eeenweeneeeeeeeweeeseseee";
   "seswseseseswsesesesesenesesesese";
   "wnwnwwsewswwwewwwnwwneweseswne";
   "nwnwnwnweswnwnwnwnwwnewnwswnwnwnwnwnw";
   "seseeseewseswsewsesesewneeseswsenene";
   "seseneseswseesenwsesesenenwseseswwsesese";
   "wswwnwenwwnwswnewwwwwnwnenwwnw";
   "ewewwwwwwswwwwwewnwwsww";
   "wwsewswwwwnewneswwwswwnenwwne";
   "wsewnwnwnwwwwwnweeswnewnwwnwnw";
   "nwnwwnwnwnwnwnwswnwenwewswnwwwwnwnw";
   "newwwewswsenwnwnwwwsew";
   "swswwseswnwnweswwsweswwwwswnwswnwse";
   "eseseseenweewseseseseseeseeeenwe";
   "eseseeeswesesewnwenwseenwseseesene";
   "wwnwwnwseewnwnwnenwsesenwnwswnwsene";
   "eseeseseewseseseeseese";
   "nwenwneswweneeeswseeseenwneee";
   "wweswwwswwseswwswswswwwswwnew";
   "seenwwseswwseeneseenwsenwswe";
   "nenenenwwneswnenenwnwsesewseneneneenwswnw";
   "nwnenenenenwneneneswnwnenwnwenene";
   "sewnwswesewsewneneswwwsenwne";
   "wnwnwnwnwnenwnenwwswnwnwnwnwnwwwswnw";
   "sweseneseseswswswswseswnwswseswwswsww";
   "wenwwwsenwseswnwnwwwnenwnwnwnwnwww";
   "seneswswswswneswswswswswswswwswswwsw";
   "nwnwnwenwnwnwwnwnwnwnwnwnwnwnwnwnwswnw";
   "nwnwesenenenwnenwnwnewnenewnenwnwswnwsenw";
   "nwwnwwenwnwnwsenwnwnwwewwnwnwwnw";
   "nwnwnwnwnwnwswnwnwnwnenwnwnenwswnenw";
   "nwnwnwnenwnwnwnwnwnesenenenenwnwnwnww";
   "swsenwnwwnesenwnwwnwnwsewnwseenwnwne";
   "swswenwswseswswswwswswswseswswnwseswswse";
   "wneneneweseneswswswsenww";
   "nwneswswnwswnwenenwwneswnwnenenenesenwne";
   "nenenwneseswnenenenenwneneeneenewee";
   "nwwwewwnwnwnewsewswnewesewnww";
   "nwswneswseseseswseseswseseswseseseseswse";
   "swswwwwwswwwnwswsew";
   "swsweswneswswswnweswwswswswswswwsww";
   "swswswswswswswswenwwwswswwswneswswsw";
   "seeeeseweswswenwneseeseenwnwswseww";
   "enweeneeenweneeseseneeweseee";
   "nwswnwenenwnwnwnwswnwnwnwenwsenwwwse";
   "nweewwnwesenewswnwwswsenwwnwsww";
   "enwnwswwseswnewsewneeeswseew";
   "neneenesweeneeneneneneewnwenenese";
   "swwneneseenesewsesesenewesesesesewsenw";
   "wnwwnwwwwwewwwwsww";
   "wnwswwnwsesewwwwwnewnenww";
   "nwwnwnwnenwswnwneneenwnwswnenwnwnenenw";
   "eswswewwswwnenwewneneswswewsesw";
   "neswwswswwswswswwswswswwswwewswww";
   "eseenwweseseeeeneeeseeenwese";
   "neeeeweeeeeneenwesesweswneswse";
   "nwwwnwwwewwnwswnwnwwenwsenwwwse";
   "nwnwnwnwnwnenenwnwnwnwnwnwnwnenwnesenwsene";
   "nwseswswswswseswswseswswswswswswswswneswsw";
   "wnwnenwwenwnwwewwenwwsenwwswwswnw";
   "seseswneswnwswwswwseseswneenenenesww";
   "esesewseeneseswwsesesesesesesesesese";
   "wwnwnwnewnwnwsenwnwwnwenwnwsenwsenw";
   "swwwneswwwwwwwwwewswwwww";
   "seneneeseneewswnwswswnewnenwwnenenwswne";
   "swswseswwwswwwswwswwwwwwwnew";
   "eeseseewseseseenwseeeeeseseesese";
   "enenenenwnwswnenwnenwnenenwneswswnenw";
   "nesewswsesesesesenesesenwseeseneseswswe";
   "neeneesweswneeswneenwenweenenwsw";
   "swnenwwsenwneswneneswnenenenenesweene";
   "wnwewwnwwwwwnwnwsew";
   "nwnwneswwnwnwswneenwnenwneneeenenwnw";
   "neeeneseeewneseswnwseswnw";
   "nwseenenewenwnewneswnenwenwswsenene";
   "wnenwnewnwnwnwnwnwenwnesenwnwnenenwnwnwnw";
   "newwwwswwnwwswwswswewsewweww";
   "neeenenenwneneseneseseenenenwnewnene";
   "wnweseswseseenwnw";
   "senwswseswnweseseswseswswswswswswswenw";
   "nwnwnwnwnwsenwswnwnwnwnenwwnwnwnenesenw";
   "eneneneenesenesweeeeeeeeeewe";
   "nwnenwneneswnwnwnwnwnenwnwnenenwewnwnee";
   "neeenwseeeneeeeneeeeseenenew";
   "wsweeswseswswswswweswewnwneswseswnw";
   "swswnwseswswswnwswswswswswswwwswswenwse";
   "wnweswwswswnwseswswneswswsewswswsw";
   "nwnenenwnenwnwnenenwewnenwnwnwsenwnwnesw";
   "wnenenesenwnenenenenesenenenenewnenenenene";
   "swnwnewswswseseswseswwnwnwsweneswsew";
   "neeeeneenenenenenwneneswweenenenene";
   "nwewswswnenenwnwwswweweneeesenwne";
   "sesewenesesewsewsesenesesenwnesesenwsw";
   "nwewwnwwwwnwwwwswwwwe";
   "eseseesewsesesesewneseneeseeseesesew";
   "neneneeneneeeeeneenenesw";
   "newewswsesewnenwwwswwwnwwsw";
   "sewwnwewwsesenenwnwswswnenenw";
   "nwnwnwsewwswesenee";
   "ewnwseewswnwwswwwwsw";
   "wwwenewwesewwwwwwwwwww";
   "wnwseeswenenwww";
   "neeseseeesesewwneeeeseswneeseswe";
   "wwsewswwswwswwswwswnewswswswwsw";
   "eeseeweseenweseseswsesenesesesese";
   "senenwwnwwwnwswnwewwnwwswnwwnww";
   "nwnwnwswnwnenwnwnwsesenenenwnwnenenenenw";
   "wswnwswswewnwwwnwseneesenewswwew";
   "nweeswesweeeeeeeeeeeenwe";
   "eeneeenenwenweeeeseneesweew";
   "seewswseswsweswwenwswswseswswnweswswsw";
   "seswenewwswswnewnwwsenenewswswwwse";
   "nenewweeneeseneeneeswnenenewnene";
   "neweswnenwnwswneenwwsenwsenwenewnw";
   "nwnwneseswnenweenenenenesewnenwsenenwwe";
   "nesweswenesweneenweenewsenenenwnw";
   "seeeseeeseseseenenweeseeenweseew";
   "nenewneeswneneneeneneseneeeneneenenee";
   "swswswswnwwseseseswnwsesenwseseeesesene";
   "swnwsesenwneseenwsesesesewsesweseeee";
   "nwneneneneseneneneneneneneew";
   "wswwwwwwwsenwnwwnwnwwwwneww";
];;


let solve input =
    let path_coordinates path =
        let rec loop idx prev_char (x, y) =
            if idx >= (BatString.length path) then
                (x, y)
            else
                let new_idx = idx + 1 in
                let cur = BatString.get path idx in
                match (prev_char, cur) with
                | ('#', 'n') -> loop new_idx 'n' (x, y)
                | ('#', 's') -> loop new_idx 's' (x, y)
                | ('#', 'e') -> loop new_idx '#' (x + 2, y)
                | ('#', 'w') -> loop new_idx '#' (x - 2, y)
                | ('s', 'e') -> loop new_idx '#' (x + 1, y - 1)
                | ('s', 'w') -> loop new_idx '#' (x - 1, y - 1)
                | ('n', 'e') -> loop new_idx '#' (x + 1, y + 1)
                | ('n', 'w') -> loop new_idx '#' (x - 1, y + 1)
                | _ -> raise (Invalid_argument "invalid path") in

        loop 0 '#' (0, 0) in

    let flip tiles coord =
        if BatSet.mem coord tiles then
            BatSet.remove coord tiles
        else
            BatSet.add coord tiles in

    let rec loop tiles commands =
        match commands with
        | [] -> tiles
        | path :: rest ->
            let coord = path_coordinates path in
            let new_tiles = flip tiles coord in
            loop new_tiles rest in

    let get_neighbors (x, y) = [
        (x + 2, y);
        (x + 1, y - 1);
        (x - 1, y - 1);
        (x - 2, y);
        (x - 1, y + 1);
        (x + 1, y + 1)] in

    let get_all_coords tiles =
        let lstlst = BatList.map
                (fun (x, y) -> (x, y) :: (get_neighbors (x, y)))
                (BatSet.to_list tiles) in
        let coords = BatList.flatten lstlst in
        BatList.unique coords in

    let run_day tiles =
        let process_tile ret (x, y) =
            let neighbors = get_neighbors (x, y) in
            let blacks = BatList.map (fun coord -> BatSet.mem coord tiles) neighbors in
            let black_count = BatList.length (BatList.filter ((=) true) blacks) in
            if BatSet.mem (x, y) tiles then
                if black_count = 0 || black_count > 2 then
                    ret
                else
                    BatSet.add (x, y) ret
            else
                if black_count = 2 then
                    BatSet.add (x, y) ret
                else
                    ret in

        let all_coords = get_all_coords tiles in
        List.fold_left process_tile BatSet.empty all_coords in

    let rec play_game tiles day =
        if day > 100 then
            tiles
        else
            let new_tiles = run_day tiles in
            printf "Day %d, tiles = %d\n%!" day (BatSet.cardinal new_tiles);
            play_game new_tiles (day + 1) in

    let tiles = loop BatSet.empty input in
    let tiles_after = play_game tiles 1 in
    BatSet.cardinal tiles_after
;;

printf "Result = %d\n" (solve input);;
