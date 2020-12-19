(*
https://adventofcode.com/2020/day/19
*)

open Printf

type rexp = Rule of int | Lit of char;;

module IntMap = Map.Make(struct type t = int let compare = compare end)

let input_test = (
    [
        (42, [[Rule(9); Rule(14);  ]; [Rule(10); Rule(1); ]]);
        (9, [[Rule(14); Rule(27);  ]; [Rule(1); Rule(26); ]]);
        (10, [[Rule(23); Rule(14);  ]; [Rule(28); Rule(1); ]]);
        (1, [[ Lit('a')]]);
        (11, [[Rule(42); Rule(31); ]]);
        (5, [[Rule(1); Rule(14);  ]; [Rule(15); Rule(1); ]]);
        (19, [[Rule(14); Rule(1);  ]; [Rule(14); Rule(14); ]]);
        (12, [[Rule(24); Rule(14);  ]; [Rule(19); Rule(1); ]]);
        (16, [[Rule(15); Rule(1);  ]; [Rule(14); Rule(14); ]]);
        (31, [[Rule(14); Rule(17);  ]; [Rule(1); Rule(13); ]]);
        (6, [[Rule(14); Rule(14);  ]; [Rule(1); Rule(14); ]]);
        (2, [[Rule(1); Rule(24);  ]; [Rule(14); Rule(4); ]]);
        (0, [[Rule(8); Rule(11); ]]);
        (13, [[Rule(14); Rule(3);  ]; [Rule(1); Rule(12); ]]);
        (15, [[Rule(1);  ]; [Rule(14); ]]);
        (17, [[Rule(14); Rule(2);  ]; [Rule(1); Rule(7); ]]);
        (23, [[Rule(25); Rule(1);  ]; [Rule(22); Rule(14); ]]);
        (28, [[Rule(16); Rule(1); ]]);
        (4, [[Rule(1); Rule(1); ]]);
        (20, [[Rule(14); Rule(14);  ]; [Rule(1); Rule(15); ]]);
        (3, [[Rule(5); Rule(14);  ]; [Rule(16); Rule(1); ]]);
        (27, [[Rule(1); Rule(6);  ]; [Rule(14); Rule(18); ]]);
        (14, [[ Lit('b')]]);
        (21, [[Rule(14); Rule(1);  ]; [Rule(1); Rule(14); ]]);
        (25, [[Rule(1); Rule(1);  ]; [Rule(1); Rule(14); ]]);
        (22, [[Rule(14); Rule(14); ]]);
        (8, [[Rule(42); ]]);
        (26, [[Rule(14); Rule(22);  ]; [Rule(1); Rule(20); ]]);
        (18, [[Rule(15); Rule(15); ]]);
        (7, [[Rule(14); Rule(5);  ]; [Rule(1); Rule(21); ]]);
        (24, [[Rule(14); Rule(1); ]]);
    ],
    [
        "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa";
        "bbabbbbaabaabba";
        "babbbbaabbbbbabbbbbbaabaaabaaa";
        "aaabbbbbbaaaabaababaabababbabaaabbababababaaa";
        "bbbbbbbaaaabbbbaaabbabaaa";
        "bbbababbbbaaaaaaaabbababaaababaabab";
        "ababaaaaaabaaab";
        "ababaaaaabbbaba";
        "baabbaaaabbaaaababbaababb";
        "abbbbabbbbaaaababbbbbbaaaababb";
        "aaaaabbaabaaaaababaa";
        "aaaabbaaaabbaaa";
        "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa";
        "babaaabbbaaabaababbaabababaaab";
        "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba";
    ]);;

let input = (
    [
        (27, [[Rule(116); Rule(44);  ]; [Rule(127); Rule(69); ]]);
        (19, [[Rule(60); Rule(116);  ]; [Rule(55); Rule(127); ]]);
        (91, [[Rule(127); Rule(13);  ]; [Rule(116); Rule(127); ]]);
        (11, [[Rule(42); Rule(31); ]]);
        (30, [[Rule(80); Rule(116);  ]; [Rule(100); Rule(127); ]]);
        (53, [[Rule(78); Rule(116);  ]; [Rule(100); Rule(127); ]]);
        (47, [[Rule(116); Rule(129);  ]; [Rule(127); Rule(76); ]]);
        (21, [[Rule(57); Rule(127);  ]; [Rule(100); Rule(116); ]]);
        (26, [[Rule(86); Rule(116);  ]; [Rule(28); Rule(127); ]]);
        (86, [[Rule(127); Rule(122);  ]; [Rule(116); Rule(29); ]]);
        (74, [[Rule(127); Rule(39);  ]; [Rule(116); Rule(105); ]]);
        (41, [[Rule(118); Rule(116);  ]; [Rule(132); Rule(127); ]]);
        (13, [[Rule(116);  ]; [Rule(127); ]]);
        (115, [[Rule(82); Rule(116);  ]; [Rule(72); Rule(127); ]]);
        (100, [[Rule(116); Rule(116);  ]; [Rule(116); Rule(127); ]]);
        (5, [[Rule(78); Rule(127);  ]; [Rule(91); Rule(116); ]]);
        (31, [[Rule(101); Rule(127);  ]; [Rule(121); Rule(116); ]]);
        (134, [[Rule(116); Rule(117); ]]);
        (73, [[Rule(123); Rule(116);  ]; [Rule(62); Rule(127); ]]);
        (69, [[Rule(116); Rule(124);  ]; [Rule(127); Rule(77); ]]);
        (28, [[Rule(71); Rule(116);  ]; [Rule(66); Rule(127); ]]);
        (14, [[Rule(131); Rule(116);  ]; [Rule(128); Rule(127); ]]);
        (111, [[Rule(127); Rule(59);  ]; [Rule(116); Rule(117); ]]);
        (6, [[Rule(127); Rule(53);  ]; [Rule(116); Rule(79); ]]);
        (104, [[Rule(26); Rule(116);  ]; [Rule(65); Rule(127); ]]);
        (61, [[Rule(78); Rule(127);  ]; [Rule(56); Rule(116); ]]);
        (67, [[Rule(127); Rule(30);  ]; [Rule(116); Rule(130); ]]);
        (32, [[Rule(81); Rule(127);  ]; [Rule(27); Rule(116); ]]);
        (2, [[Rule(116); Rule(117);  ]; [Rule(127); Rule(72); ]]);
        (128, [[Rule(57); Rule(116);  ]; [Rule(91); Rule(127); ]]);
        (81, [[Rule(127); Rule(96);  ]; [Rule(116); Rule(51); ]]);
        (38, [[Rule(127); Rule(59);  ]; [Rule(116); Rule(54); ]]);
        (36, [[Rule(22); Rule(127);  ]; [Rule(107); Rule(116); ]]);
        (35, [[Rule(116); Rule(61);  ]; [Rule(127); Rule(24); ]]);
        (7, [[Rule(116); Rule(100); ]]);
        (18, [[Rule(16); Rule(116);  ]; [Rule(82); Rule(127); ]]);
        (116, [[ Lit('a')]]);
        (54, [[Rule(116); Rule(116);  ]; [Rule(127); Rule(116); ]]);
        (84, [[Rule(20); Rule(116);  ]; [Rule(93); Rule(127); ]]);
        (92, [[Rule(38); Rule(127);  ]; [Rule(111); Rule(116); ]]);
        (64, [[Rule(127); Rule(57);  ]; [Rule(116); Rule(82); ]]);
        (105, [[Rule(116); Rule(43);  ]; [Rule(127); Rule(110); ]]);
        (44, [[Rule(127); Rule(106);  ]; [Rule(116); Rule(88); ]]);
        (93, [[Rule(33); Rule(116);  ]; [Rule(56); Rule(127); ]]);
        (82, [[Rule(116); Rule(13);  ]; [Rule(127); Rule(116); ]]);
        (75, [[Rule(2); Rule(127);  ]; [Rule(9); Rule(116); ]]);
        (65, [[Rule(127); Rule(97);  ]; [Rule(116); Rule(36); ]]);
        (4, [[Rule(5); Rule(127);  ]; [Rule(98); Rule(116); ]]);
        (1, [[Rule(114); Rule(127);  ]; [Rule(58); Rule(116); ]]);
        (122, [[Rule(54); Rule(127);  ]; [Rule(59); Rule(116); ]]);
        (70, [[Rule(116); Rule(67);  ]; [Rule(127); Rule(75); ]]);
        (117, [[Rule(127); Rule(127); ]]);
        (68, [[Rule(48); Rule(127);  ]; [Rule(40); Rule(116); ]]);
        (97, [[Rule(127); Rule(95);  ]; [Rule(116); Rule(113); ]]);
        (132, [[Rule(127); Rule(33);  ]; [Rule(116); Rule(54); ]]);
        (51, [[Rule(23); Rule(127);  ]; [Rule(30); Rule(116); ]]);
        (119, [[Rule(127); Rule(37);  ]; [Rule(116); Rule(103); ]]);
        (63, [[Rule(57); Rule(127);  ]; [Rule(59); Rule(116); ]]);
        (34, [[Rule(57); Rule(13); ]]);
        (48, [[Rule(116); Rule(73);  ]; [Rule(127); Rule(47); ]]);
        (23, [[Rule(82); Rule(127);  ]; [Rule(59); Rule(116); ]]);
        (66, [[Rule(127); Rule(82);  ]; [Rule(116); Rule(78); ]]);
        (95, [[Rule(127); Rule(100);  ]; [Rule(116); Rule(82); ]]);
        (123, [[Rule(117); Rule(116);  ]; [Rule(82); Rule(127); ]]);
        (24, [[Rule(116); Rule(33); ]]);
        (120, [[Rule(49); Rule(127);  ]; [Rule(99); Rule(116); ]]);
        (103, [[Rule(127); Rule(90);  ]; [Rule(116); Rule(83); ]]);
        (102, [[Rule(72); Rule(13); ]]);
        (58, [[Rule(46); Rule(116);  ]; [Rule(45); Rule(127); ]]);
        (106, [[Rule(56); Rule(127);  ]; [Rule(117); Rule(116); ]]);
        (85, [[Rule(126); Rule(127);  ]; [Rule(133); Rule(116); ]]);
        (3, [[Rule(74); Rule(116);  ]; [Rule(85); Rule(127); ]]);
        (20, [[Rule(33); Rule(127);  ]; [Rule(56); Rule(116); ]]);
        (127, [[ Lit('b')]]);
        (45, [[Rule(116); Rule(102);  ]; [Rule(127); Rule(18); ]]);
        (126, [[Rule(127); Rule(41);  ]; [Rule(116); Rule(112); ]]);
        (94, [[Rule(116); Rule(87);  ]; [Rule(127); Rule(21); ]]);
        (114, [[Rule(35); Rule(116);  ]; [Rule(19); Rule(127); ]]);
        (0, [[Rule(8); Rule(11); ]]);
        (99, [[Rule(127); Rule(34);  ]; [Rule(116); Rule(76); ]]);
        (80, [[Rule(116); Rule(127);  ]; [Rule(127); Rule(116); ]]);
        (50, [[Rule(15); Rule(116);  ]; [Rule(7); Rule(127); ]]);
        (55, [[Rule(127); Rule(91);  ]; [Rule(116); Rule(59); ]]);
        (90, [[Rule(92); Rule(127);  ]; [Rule(94); Rule(116); ]]);
        (25, [[Rule(57); Rule(116);  ]; [Rule(54); Rule(127); ]]);
        (124, [[Rule(116); Rule(57);  ]; [Rule(127); Rule(91); ]]);
        (77, [[Rule(78); Rule(116);  ]; [Rule(78); Rule(127); ]]);
        (56, [[Rule(127); Rule(127);  ]; [Rule(116); Rule(116); ]]);
        (79, [[Rule(116); Rule(59);  ]; [Rule(127); Rule(57); ]]);
        (12, [[Rule(127); Rule(64);  ]; [Rule(116); Rule(115); ]]);
        (130, [[Rule(17); Rule(127);  ]; [Rule(33); Rule(116); ]]);
        (8, [[Rule(42); ]]);
        (60, [[Rule(116); Rule(72);  ]; [Rule(127); Rule(59); ]]);
        (113, [[Rule(117); Rule(127);  ]; [Rule(117); Rule(116); ]]);
        (131, [[Rule(16); Rule(127);  ]; [Rule(56); Rule(116); ]]);
        (46, [[Rule(134); Rule(116);  ]; [Rule(62); Rule(127); ]]);
        (83, [[Rule(6); Rule(127);  ]; [Rule(50); Rule(116); ]]);
        (33, [[Rule(116); Rule(116); ]]);
        (88, [[Rule(116); Rule(57);  ]; [Rule(127); Rule(54); ]]);
        (118, [[Rule(116); Rule(100);  ]; [Rule(127); Rule(54); ]]);
        (125, [[Rule(100); Rule(116); ]]);
        (121, [[Rule(32); Rule(116);  ]; [Rule(104); Rule(127); ]]);
        (29, [[Rule(127); Rule(17);  ]; [Rule(116); Rule(57); ]]);
        (101, [[Rule(127); Rule(1);  ]; [Rule(116); Rule(68); ]]);
        (22, [[Rule(116); Rule(117);  ]; [Rule(127); Rule(100); ]]);
        (43, [[Rule(127); Rule(125);  ]; [Rule(116); Rule(22); ]]);
        (10, [[Rule(29); Rule(127);  ]; [Rule(63); Rule(116); ]]);
        (112, [[Rule(98); Rule(116);  ]; [Rule(107); Rule(127); ]]);
        (87, [[Rule(80); Rule(127);  ]; [Rule(57); Rule(116); ]]);
        (129, [[Rule(54); Rule(116);  ]; [Rule(17); Rule(127); ]]);
        (98, [[Rule(116); Rule(80);  ]; [Rule(127); Rule(117); ]]);
        (57, [[Rule(116); Rule(116);  ]; [Rule(127); Rule(13); ]]);
        (107, [[Rule(127); Rule(72);  ]; [Rule(116); Rule(59); ]]);
        (40, [[Rule(116); Rule(14);  ]; [Rule(127); Rule(4); ]]);
        (39, [[Rule(12); Rule(116);  ]; [Rule(84); Rule(127); ]]);
        (133, [[Rule(116); Rule(108);  ]; [Rule(127); Rule(10); ]]);
        (71, [[Rule(127); Rule(56);  ]; [Rule(116); Rule(54); ]]);
        (76, [[Rule(116); Rule(78);  ]; [Rule(127); Rule(33); ]]);
        (15, [[Rule(127); Rule(59);  ]; [Rule(116); Rule(78); ]]);
        (42, [[Rule(119); Rule(116);  ]; [Rule(3); Rule(127); ]]);
        (9, [[Rule(127); Rule(72);  ]; [Rule(116); Rule(100); ]]);
        (72, [[Rule(127); Rule(127);  ]; [Rule(127); Rule(116); ]]);
        (17, [[Rule(13); Rule(13); ]]);
        (16, [[Rule(116); Rule(116);  ]; [Rule(13); Rule(127); ]]);
        (49, [[Rule(53); Rule(127);  ]; [Rule(25); Rule(116); ]]);
        (62, [[Rule(127); Rule(80);  ]; [Rule(116); Rule(100); ]]);
        (52, [[Rule(72); Rule(116);  ]; [Rule(33); Rule(127); ]]);
        (37, [[Rule(70); Rule(127);  ]; [Rule(120); Rule(116); ]]);
        (110, [[Rule(89); Rule(127);  ]; [Rule(87); Rule(116); ]]);
        (96, [[Rule(116); Rule(52);  ]; [Rule(127); Rule(55); ]]);
        (78, [[Rule(116); Rule(127); ]]);
        (108, [[Rule(127); Rule(109);  ]; [Rule(116); Rule(71); ]]);
        (109, [[Rule(116); Rule(59);  ]; [Rule(127); Rule(16); ]]);
        (59, [[Rule(116); Rule(127);  ]; [Rule(127); Rule(127); ]]);
        (89, [[Rule(57); Rule(127);  ]; [Rule(33); Rule(116); ]]);
    ],
    [
        "babababababababaabbbbbabaaabbabbabbabbaa";
        "abbbbbaababbbabbbbbaaaaaaaababbbaabbabab";
        "bbaaaaababababbaaaaabbababbabaabaaaaaaabaaaaaaaa";
        "aabaabbaaaaaabaaaaaaabaabaabbbbb";
        "bbaababbaaabaabaababbaaaabaababbaabbbaababbaaabababaabab";
        "bbabbabbabaabbaabbbbaabbbbaabbaa";
        "baaaabbbabaaababbababbbbababbbbbaaaabaabaabbbbaa";
        "aaaabbabbaaaabbbaabaaabbaaabbbbababaabab";
        "bbbaabaabbaabaaaaaababbbbaaaabbaaabaaabbbbbbbbbbbababbab";
        "aabaabbaaabaaaabbbbaababbaabababaabbbbab";
        "bbaabbbbbaaabbbbbabbbbaaaabaabbaabbaabbabbababbb";
        "aaabaabaabaaababbabbabbb";
        "abbababbabbababbaaaaaaab";
        "ababbaaaabbabbabaaaabbaaaabaaabbbaabbbbaabbbabbbbbbaabbabbabbaaaababaabaabbabbaa";
        "abbababaabbbabbbaaababbbabaaababbaababaabaaaaaaaaabababbabababab";
        "baabbbbabaababbabaababbbababbbaa";
        "aaabaabbbbbbaaaabaaaaabaaaaaabaaaababbaaaaaabaab";
        "babababbabaaabbabbbbbbbabaaaabbaabababaaaabbbaab";
        "aabaabbaabbabaababaaaaaabbbababaaabbbbab";
        "aabaaabbbaabaabbabaaabbabbababaabbbbabbb";
        "abababbaaaabbababaaaaabaaaaaabbaabbbaabb";
        "bbaaabbbbaababaabbbaabbbabbbababbbbabbaa";
        "babaaaaabbbbbbabbabaaaba";
        "baaababbaabbbabbaaaababbababbabbbbbbbbbbaabbbaaaaaababbbbbaabbbb";
        "bbbbaaabbaaaabaabbbbbbbabaaababaabbbbaaa";
        "bbabbabbbbabaabbaaaaabab";
        "baaababbbbbbaababaabaabbbbbbaaaaaaaaababaabbbaabaabaaaba";
        "baababaaaababaaaaaaabbbaabbbbbaabaaababaaabbabaaaaabbaaa";
        "abbbbbbbaaabaababaaabbaa";
        "bbbabaaaabbabbbaaaaabaaabbbaabaabbbaaabbbabaaaba";
        "abaabbaabaaabbabaaaaaaaa";
        "aaabaabbaabaaaabaaaabbaabbabbaab";
        "bbaabaaaabbaabaaababaabb";
        "bbabaaababbaaabbbbabbabbabaaaaabbaaaabbbabbababbabbbbaaa";
        "abbaaabbababbabbbaaaabbababaabab";
        "aabababababbbaabaaabababababaaaaaaabbaaabbabaaba";
        "abaaaaabbbbbbaabbaaaabababaaaabb";
        "bbbbabbabababbbbaaaaaaba";
        "aaaaaabbbaabbbababbabbbabaaabbbbabbabbaa";
        "bbbababbbababbbbbbbbbabaabbaabaababbaaaabbababbb";
        "abbabbabaaaabaaabaaabbbaabbaabababbbaabaaaaaaaabbaaababa";
        "abbbabbbabaababbaababaabbabababbaaaaabaababaabaabaabbabbbabbbbbb";
        "bbbaabbbbbbababbaaabbabaabbbbababbaababbabbabaaababaabbb";
        "bbaaaaababbaaaaaabababaaaababbbabaabababbabbbaaa";
        "aabbbabbbbabaabbabbaabba";
        "ababbababbbbaaaabbabbaab";
        "abbaaaababbbbbabbaaabaaabbabbabbbabababbabaabbba";
        "bababaabbbaabaabbaaabbaa";
        "bbbaaaabbababaabbbababab";
        "babaabbbbaaababaababaaabbbaabbab";
        "bbbabaaabaaaababbaabaaabbababbbbbaabbaab";
        "abbbbbbbaababaabaabababb";
        "abbbabaabababababbaaaaabaaabbabb";
        "baaabbabbbbaababbbaabbba";
        "bbbbaabababaabbabaabbbaaaababbab";
        "abbaaaaaaabaaabbbaaabbababbaaaaa";
        "aaaabbabaababbababbbbaababbbbaaa";
        "abbbbbbbbbbaaabbbabbabbb";
        "abaaababbababbbbbabaaaabbabbaabbaaaababb";
        "bbaabbbabbbabbbbabaabbbb";
        "babbaababbbbbbabaababaaabbaabaababaaaaba";
        "bbbaaaabbbbbaaabbaabbaba";
        "ababbaaabbbbbaabbbaabbbbbaabbbbb";
        "baabbbabbaababbbbbbabbbb";
        "abbaabaaaaaabbabbaaabbabababbaaaababbbbbaaaababb";
        "bbbbbaabbbbaaabbbbabaaba";
        "bababbaaaababaaabbbabbba";
        "bbbbaabbaababaabababbbaa";
        "bbbaaaaaabaabbabaabaabaa";
        "bbbaaabbbbabbabbaababbbb";
        "aabababababbaabaabbaaaaaabbabbabaabbabab";
        "babbbabbaaababbbbbabaabbabaaaabb";
        "aababaaabbbbbaaabaabbabb";
        "bbbbbaaabaaabbababbaaaaaabaabaaa";
        "bbaaaaaababaaabbaabbaaaa";
        "baababbbaaaabbaaabaabbba";
        "aababaaaabababaaaaaabbabababbbbb";
        "bbaababaabbababbbbbbbaaaabbbaabbabbbaaaa";
        "bababbbbaababaaabaabbbbb";
        "bbaaaaaababaaaababababaaabaabbba";
        "bbbaaaaaabbaabaaabbaabaaabbabaaababbbabaaabbaaababaaabbb";
        "aabbbbbbbbbaaaaaaabbabbb";
        "bbabaababbbaabaabaaaabbaabbabbbbbabaabbabbbababaaababbababbabbbabbabbbbabbabbbababbabbaababbaaaa";
        "abaaababbabbbaabbabaabbaabaaabbaabbbaaabbaabbaabbbaaaabb";
        "aabaabbbbbbaababbabbaababbbbbbaa";
        "baabaaabbbbbbaaaabbbbbabaabbbaba";
        "bbbbbbbabbaababbabababab";
        "aaabaaabbbbbbababaaababbbbbbbbabaaababababbbbaaaabaababaaabaabababbaabba";
        "abababaabbaabbbbaabbaaaa";
        "bababbbbbbbbbaabbaaabbaa";
        "abbbbbbaaabbbaaaaabbabab";
        "baaaababbbbaaabbaaaaaabbababaabb";
        "abbbbbabbbabaabbbabbaabb";
        "aababbbabbbbaabbaababbaa";
        "bbbbaabbbbaaaabaaabbabaa";
        "abbaaaabbbaaabaabbbababbbbbbbbbabbbaababbaababbabababbabbbbabbbabbaaabba";
        "aaabbabaababaaaaabababbaabbababbbbbbbababbbbbabb";
        "abbbbbababbbaaabaabbaaba";
        "abbbbbbaaabbaaabaabbbbbaaabbaababaaaaaaa";
        "ababaaaaaaababbbaaabaaaa";
        "babbbbaaabaaababbbbbbbaa";
        "abaaaaabbabaaaabbbbbbbaa";
        "bbbaaaabbbbbabbababbbbbb";
        "baabbaaaabaabbaabbbbbbaaabaaabababbbbabaabbbabbaaaababbaaaababbaaabbaaababbbbaba";
        "bababbbabaaababbbabbbbaaaaaabbbaabbabaab";
        "bbbbaabbbababbbbabbbabbbaabbbbba";
        "aaabbbbaabbbaaabaababbaa";
        "aaabbabababbbabbbbbbabbb";
        "ababaaabbbababaabbbbbbbbbabaaabbabbbbbbbababaabb";
        "abbbaaababbbbabaabbaaaaabbbabbaaaaabbabb";
        "baabbbababbabaaabbbbbaaaaaabaabaabaaabbababbaaab";
        "aabaabbabababbbbbbbababbbbabbbabbbaaabba";
        "baaababbbbbaabaabaabbaba";
        "abbbbabaabbbaaabbaabbbabbbbbaaabbbababab";
        "babaaabbabaabbababaaabaa";
        "abaaabbabaaaaaababaaabababaaaaabaabababb";
        "abbbabbbaaababbbababaaaabbabababbbaabbab";
        "aaaaabaaabbbbbbbbaaabbbaabbbbbbabbaabbabaaabbaab";
        "bbbbbbbabbbaaabbaaabaabaababbabaabaaabaaabaaabbb";
        "abbbbbbaabbabbbabbbbabbabbbbabbaaaaababb";
        "bbbbbaaababbbaabaabbbbbbbbbbbbbbbababaaa";
        "abbbbbbbbbbbbaabaabbaaba";
        "abbaaaababbbbbaaabbabbbaaabaabbabbbbbbbababaabaa";
        "abaaabababbbbabaabbaabbaaabababaaabbbabbabbababaabbbaaaaabaabbaaaaaaabba";
        "bbaaabbbaabaaabbbbbaaaaababbabab";
        "baaabaaabaababbabbabaaabaaabbabb";
        "bbbbabbaabababaaabbbaaba";
        "aabbbabbbaabbbbaaaabaaab";
        "abaaabbabbaabaabaababababbabaaabbbbbbabbabbbbbbbbbbbbbbbababbbaabbabbaababbaaaba";
        "bbbaabbbabbbaaabbbbbbabaabaaaaba";
        "abbbbabababbbaabaaaababa";
        "abbabbbaabbbbbbbaaabaabbbbbbaabaaabbabaabaabbbbb";
        "babbbabbbbbaaabbabbaaaaaabbaaaabbbbbbbbb";
        "bbbbaaaaabbabababbaaabbaabaaabbaababababbbbabbbabbaaaabaabbaabaaabbababb";
        "abbabbbabaabbbabaaabbaab";
        "bbaababbbaababaaaabaabab";
        "bbbbababaaabababbbabbbbbbabababaabbaabaababbaaab";
        "baaaababaaaaabbaaaabbbbbbbaaaaabbabbababaaaabbbb";
        "baaaaaabbbaaaaabaaabbaabbabbbaaababbabba";
        "abbbbabababbaabaababbbab";
        "aaabaabaaaabaaabbbbababbaabbaaaabbbabbaa";
        "aaaaaabbbbaaabaaaaaaabaabbbabababaabbaaa";
        "abbbbbbbabbbbabaaabbabaa";
        "bbaaaaabbaababbabbbaabba";
        "abbbbababaaababbbbbbabab";
        "abbababbababbbabaabbaabbaaaaaaaaaaabaaaa";
        "bababbbbbbaaaaaaabbbabba";
        "baaabbabbbbbabbabaabbaababbbaaaa";
        "baabaaababbbaaabbaabaaabaababbbbababaaba";
        "aaaabbaabbbaabbbbaabaabbbaabbaaa";
        "baaabaaaabbbbbabaaabaababaabababbaaababa";
        "abbabbabbaaaaaabbaaaabaababaaaababbbaabb";
        "bbaaaaaaaababbbaaabaaaabbaaaabaababaabaa";
        "baaaaaabaaaabbbaaabbaaba";
        "aababababbbbbaaababababbbbaaaaaabbaaabba";
        "bbbaaabbbabababbbababbaaaaabbaaa";
        "baababaabababaabbbaaaabb";
        "bbaababbaababbbaaababbbb";
        "aaaaaabbaabbbbbbbbaaaaabbaaaababbbbababbbbaaaababababaaaabbbbaab";
        "baaabbabababbabababaabbaabbaabaaabaababbbbabaaabbabbbaaaaaababbabbabbbabaabbbaab";
        "abbbaaabbababbbbabbbabba";
        "aababaababbbbbbbabbababbbbbbbaba";
        "abbbabbbbabaaabbbbaabbbbababbaaaaabababaabbaababbbbbbbaababbaabbbbbbbbaaabababbb";
        "bbbaaaaaaababbabbbbbabaabaabbbabaabbbbbbbababbbb";
        "baababbabaaabaaababaaabbbaabaabbbbbaaabbbabbabaaababaaba";
        "aaabaaababbaaaaaaaabababbabaaaba";
        "abaaababaabaaaabaaaabbbb";
        "bbbaabaababbaababaaaaabaaaabaababbababab";
        "bbbbbabaabbaaaaabaaaaabaabaabaab";
        "baaaabaaabbabaabbbbbbbbb";
        "bbaaaababbbbbbabbaababbaabababbabbbbaaaaabbbbbaababaabaabbbaaabaaaaababbababbbaaaaababba";
        "bbbababbababbabbbabbbaabaabaaabbbababaabaabbaaaa";
        "bbbbaabaaaaabbabbababbab";
        "baabaaabbabaaaaababaaaba";
        "abbbabaabaabbbbaabaababa";
        "abbbbbaabbbbbbabbbbbaabbbaaababbabbabaaaaabababb";
        "bbbbaaabaaababbbaaaaabab";
        "bbbaaaaabbbbaabababbbbba";
        "abbabbbaabbbabbbbaaaabbababbaaababbbaaaa";
        "aaabbaaababbbabaaaababaa";
        "ababaabbabaaaaaaaaaaababbbaaabbbbbbababababbabbaaaaabaabaaabbbab";
        "baababaabbbaabaabababbaabaabbbbabbbbbabb";
        "abbbabbbaabababaabbbaabb";
        "baaaabbaaababaabaabaaabbabbabaabbaaabbbababbbaaa";
        "baababaababbbbaabbaaaaaabbbbaaababaabbaaabaaaaabaaaaabbb";
        "abbabbbbbaaabbbbabbbaaaa";
        "abbbbabababaaaaaabbaaaaababbbbbabbbbabab";
        "bbbaaabbabaaaaaaaaaaaabbababaaaabbaaaaabaabbaaaa";
        "bbbbaabbbababbaaabababbabaaaabaabaabaaaa";
        "baaababbabbaaaababbabbaa";
        "abaaaaabbbabaaabaabaaaabaaabababbbaaababaaababaabbaaabab";
        "bbbaaaabbababbbbababbbba";
        "baaababbbababaababbbbabb";
        "aaabbbbbabbabaababbaaaabbaabbaba";
        "baabaabbaaababababbbbbaababababbaabaabbaaabaaababbabbbaabaabbabbaabbabba";
        "baaaabbbbabaaaaabbabaaabaabaaaabbbaaaababbaaaabaabaabaaaabaabaaaabbbabbaaabbaaab";
        "bababbbbbbbbaabbabbababbbabaaaaabbbabaab";
        "baaabbababaabbababaabaab";
        "abbaaabbbbbbaabaabbbbababababbbbababaaba";
        "bbbbaabaabbaabbaabaabbbb";
        "baababbbbbbbbababaaaabbbaaaaabbb";
        "aaaabbabbabbaababbabaaababbaabab";
        "aababbbabaabaabbbbbaabaabbabbbaa";
        "abaabbaabbaabbbbaabbaabb";
        "bbbaaaabaaabbbbabaaabaaabbababbbaabbbaab";
        "abaabbaaaaabaabbbbbbbababbbbbababbbaabbbbbaabbba";
        "baababbbbbbababbaaaaabaababbbbba";
        "abbabaaaaababaabababbaaabbaaabbabaaabbaa";
        "abaaabbaabababaaaabaaabbbababbbbbaabbbbbaabbabbaababbbab";
        "bbaabbbbbbabbbbaabbaabaababbbaababbababbbbabbabaabbaabbbaabbaaaa";
        "baabababababaaaaaabbabba";
        "babababbbbaaabaaabaabbbb";
        "abbabaaaaaaaabaaabaaabbababbbbaabababaabaaaaaaaaaabbbbbaaaaabababbababba";
        "bbabbabbabaaabbaaabbabab";
        "aabbbbbbbbaababbbaaabaaaaabaaaba";
        "bbaababbabbaaaabababaaba";
        "bbabaabbbaabbbbbabaaabaabbbbbbaa";
        "bbbabaaabbbaaaababababbaabaaababbbbaaabbbbbabbaabbabaababaaababa";
        "abababaaaababbbababbabab";
        "bbbbbabaabbababbbbabbabbaabbaaaa";
        "babbaabababaaaabbbaabbaa";
        "bbbaabbbbababababbabbaab";
        "bbbaabaaabaaaaaaababbbbbbbaaaabababbabababbaaabbababaabbabbaabaabbbabbaa";
        "bbbbabbabababbaaaaaabaab";
        "babbbaabbababbabaaababbbaaababbaaabbaabb";
        "abaabbababaababbabbbaaabbbbbabbb";
        "bbbabaaabaaaabbababbbabbabbbabbbabbbbbbabbabbaaa";
        "bbabaabbaaabaabaabbabbabaabaaabbaabbbbaa";
        "bbababaabaaaaaabbabbbbaabbaabaabaabbbababbaabbba";
        "abbaaabbbaaabbbbbabbbabbaaabbbaa";
        "bbbabaaaabaaabbaabbabbaa";
        "abbbbbbaabbabaaaababbbab";
        "abbbabbbaaaabaaaabbbbbaababaaaababbabbbaaabbbbba";
        "abbabaabbaaabaaaaabaabbbaaabaabaabbbaaaa";
        "bbbaaaaaabbaaaaabaabbbaaabaaaababaaaaabb";
        "baabbbbabababbbabababaabbaaabbabaabbbbaa";
        "aabaabbbabbabbabbabbabba";
        "babbbabbbbbaabbbaababbbabbabaaababbaabab";
        "bbbaabaaaaaabaaaabbbabaababaaaaababaabaaaabbaaabbabbabbb";
        "aabaabbbbaabababaaaaabbb";
        "baababaabaaabbabbaaabbaa";
        "abababbabaabbbabbbaaaabaabbaabbabbbbabbb";
        "abbbabbbabbababbbababaaa";
        "bbabbbbaabababbaabbabaabababbaaaaabbabaabbbbabab";
        "aaabbabababbbabbaabbbbab";
        "bbaabababbaaabbbaabbbbab";
        "baaabbaababbbbbaaabaaabaabaabbbababbaaab";
        "bababbbbbbbbaabbbbbbabaa";
        "bbaaaabaaaaabaaaabbababaaabbbaab";
        "aaabbaabaaaabbabbbaaabababbbbababbbaababbbaaaabbabaabbab";
        "ababbaabaabbbbbbbabaaaabaaaaaaab";
        "baaabbbaaaabbabaababbbab";
        "bbbbaababaabaaababbbabbbbaaabbbbaaaaaabbbabaabaaaabbabbb";
        "bababbaaaaaabaaabbbaababaaaaabbaabbaaaaaaabaaaaaabaabbbbbabbbbababbbaabb";
        "aaabbababaaabbbbabbaaaaaaaaabbbaaaabbbaababaabbb";
        "aabaaaabaaaabbbabababababbbbaabaaabaaaaa";
        "aaaabbaaabbabbbaabbabaaababababbabbabaaaabbaabbb";
        "aaaabbbbbbababbaabbbaabbbabbbbaaabbaaabbbababbaabbbababbabbbababbaababbbaababbaaaaababbb";
        "babababbbbabaabbbbaaaabb";
        "abaabbabaaabaababbababaabbabaaababbbbaaa";
        "aaaaabaabaaaabaaaaabbaaa";
        "baaabbbaabbaaabbaababaabbbaaaaaaabbbaabb";
        "babaaaabbaabbbabbbbabaab";
        "baaababbbbbbbaaaaaaaabaaababbbabaaaaabab";
        "bbbaabaabaabbbabbbaabbba";
        "aaaaabbbabaabbbaabaaabbaabbbbbabaaaabaabaabbabbbbbbabbbbbbabbbbaaaaabbaa";
        "abaaaaabbabbaababbaababbaaaaabaaababbaaaaaaabbbbbaabbaaa";
        "babaaaabbbaababbbaabbaaa";
        "babbbbaaabaabbaababaaaabbbababaabbbbbbaababaabaa";
        "baaaaaababababaabbbabbba";
        "bbaaabaabbaaabaaaabaabbaabbaabbaaabbaaaa";
        "aaabbbbbaaabaababbbbbaababbbaaababbabbbabbbbbbbbaaaababbabbbaaba";
        "bbaabaabbbbbbaaabababbab";
        "baaabbbbbbbbbabbbaaaaabb";
        "aabbbabbbaaaabbbaabaaabbaaabaabaabaabbbbbaabaaba";
        "abbaaaaabbbbbaaaaaaabbbababbabba";
        "bababbbaababbabaababaaab";
        "bababbaaabbbbbabbbabbaab";
        "bbabaabbaaabaababbbbabbaaaaabbbaaaabbbbabbabbaab";
        "bbabaabbbaaaabbaabbbbbaababbbbaaaabbbbba";
        "babaaaabbababababbaaaabababbaaab";
        "bbbbaabaaabababaaabbbbbababbbababaaaaaabbbaabbabaaabbbaa";
        "baaabbbbabbababaabababbaabbaabababaabaaa";
        "bbabaaabaababababaaaababbbbbbbababbaababaabbbaab";
        "bbbbbabbaababaaabbbbbababbbababa";
        "bbbabaaaaabbbbbbbaabbaab";
        "bbbbabbaabbbbbababaaabbaaaabbbbbbbbbbabababbaabb";
        "abbabbabbbbbabbabaaaaaaa";
        "bbaabaabbabbbbaabbbabbaababbbababbababbbbbbbabbb";
        "abababbbaaaababbbabbaabb";
        "bbabaabbaaaaabaaabbabaabaabaaaba";
        "bbabbbbaaaabababbbabbbbb";
        "bbaaaaabaaabbabaabbaabaaaabbbabaaabaaaaa";
        "baaaaaabaabaaabbbaabababaaabaaababaaabbbbabaabaabaabbaba";
        "bbbababbbaababbabaaaabaabbbaabaaabababbbababbbbb";
        "bbaababbbabbbabbaababbbababababbbbbaaaba";
        "baaabababbabbabbabbbbabbbabaaabaaabbaabababbbbbbbaabbbaa";
        "ababaaaabbbbbabaabaabbabababbabbaabbabba";
        "bbabbabbbabababaaabbaaba";
        "baaabbbaabababbbbaabbabbaabbaabbbababbbaabbaaabbbabaaabbaaabbbab";
        "aabababaabaaabbaaaaababb";
        "babaabbaaaabbbbbaaababaa";
        "aaabaaababbbbabbabbabaababababab";
        "abaaaaaabbabbabbababbaabbabaabaa";
        "aaaaabaaabababaabbababba";
        "babbaabbaaaabababbabbbbbaaaababa";
        "bbaabbbbbababaababaabbbb";
        "bbaaaaaabbbbaabbbbbaaabbaaabbaabaababbbb";
        "bbabbbbaaababbaaabbabbaaabbbaabaaababbab";
        "bababababaaaabaaabababbb";
        "baaabbbaaabaaaaaaabbbbba";
        "baabbbbabbbaabaaaaaabbababbaaaaabbaaabbb";
        "babaabbabbbbbabaabaaaaaabbbabbaaabaabbba";
        "babababbbabbbbaaabbaaaababaaaaabababbaaaaaabbbabababbbabababbbbbbbbbabbb";
        "aabaabbbaaabbbbaabbabbabbabbababbaaaaabb";
        "baaabaaabaaabbabaaaabbabbbaabbaaabaabaaa";
        "aaabaababbbbbbabaabbbbbbaaaabaab";
        "aabbabbbbbbbbbbbababbbabaaabbbaa";
        "bbbaabbbababbababbbbaaabbababbbabbababba";
        "bbabbbbabaaabaaababababbaabbaaaa";
        "abbabbabbaaababbbbbaaaba";
        "baaabbbabbbbabbabbbbabbb";
        "abbbabaabbaabaaabbbaaaababbababbabbbaaaaaabbaabb";
        "bbbbbaaabababbbabbbaaaba";
        "baaaababbbaababbaababbaa";
        "baabbbbabbaabaaaaabbaaba";
        "abbbabaabbbaababbbabaaabaaaabbaabbabaaba";
        "bbababaabbbabaaabababbbbabaaabbb";
        "ababbaaaabbbbbabbbabaaba";
        "bbbbaabbbbbaaaabbbaabbbaaaabbabb";
        "bbbbbbababbabbbbabbaaaababbbbbbbbbabbbbb";
        "abbaababbbaabbaaaaabbbababaababaabaaabaaaaaababb";
        "baaabaaabbbbabbababababaaabaaabbabbaaabbbaabbabb";
        "baabbbaaaaaaabbaabaaaaababbabbabbbaaabab";
        "ababababbabbaabbbabbabbbbabaaaaaaaaabbbb";
        "baababbabbbaabbbabaaabbaabbbabba";
        "bbaabbbbbbaababbbabababbbbbaaabbabbaaaaabbbbbbbbbbbbabaaabbbaaaa";
        "baaaaabaaabaaaabaaabbbbaababbaabbbbbbabaaababbaa";
        "abaabbaaabaababbbbaabbaa";
        "baaabaaabbaaaaaababaabab";
        "bbbbaaabbabababbbbaaabba";
        "aaaaaabbababbabbabbabababbabaabbaabbabbbbbbaaababbabbaba";
        "bbbbbabababaaaabaabbaaaa";
        "baaaabaaaaabbbbbbababbbbbbbaabbbbaaabaabababbbaa";
        "bbaaaabaabbbbbaabbaaabaaaababaaabaaaabaababbaaab";
        "bbaabbbbbbaaaabaaaabbbbbbbaaabaaababbaaaabbaaaabaabaaabaabbbabbaababbbab";
        "bbbbaababaaabaaaaabbbbba";
        "aaabbbbaabababaabbaababaaaabababaaaaaaaa";
        "bbaabaabaaaabbbabababbaaabbbbbaabbabaabaaaabbbab";
        "aaabaaabbbaabaaabaaabbababaabbabbbabbbaa";
        "abbabaababbabbbabbbaaabbbbbababa";
        "aaabbbbbaabaabbababbbbba";
        "baaaabaabbaababbababbbba";
        "bbbbbbabbabbbaaaababababaababbbaaababbbabaabbbbaabaabbaaabbbbaaa";
        "abbabaabbaaabaaabaabbaaabaabbaaababbbbbbbbbbaabaabaabbab";
        "abbbbbbbbbbababaaaabaaaaabaababaababbbbababbaaaa";
        "ababbabababbbaababbabbbaabbabbbbbbabbaaa";
        "baaaabaababaabbabbaaaaaabbbaaaaaabaaabababaaaabbbabbaaaa";
        "abbbbbaababbbaababbaaabbbbaaabbbbbababab";
        "aabaabbaabbbbbababaabaaaabbaaabaabbaaababbbbabaaabbababbabbbbabaabbbaaaaaaabaabb";
        "babbaababababababbaabbba";
        "abbabaabaababbbababaabaa";
        "bbaabaaabaabababbaabababbaaababbbbbaaaabbabbbbbbababbbbaababbbbbbabbbaba";
        "aaabaaabbbaaabbbbbaaabaabaaaababbbabbbabababababbbbabbaa";
        "bbbbbbaaabbbabaababaabaaababbbabaaabbbbababaaaabbbbbbbbabaaababbbaaabaabbbbbabbaaaabaaba";
        "aaabbbbabbbaabaaabbaabaabaaababaaabbbaab";
        "abbaaabbbbbbaabbbaababaababbbbba";
        "bbaaaaabbbbabaaabaaabaab";
        "bbbaaaaababababbbaaabbbbbaababbabaabaaaaababbbbabbababba";
        "ababbaaabbbbbbbaabaaaaaababaabaaabaaabaa";
        "baababbbbaaababbaabbbbba";
        "babbabbbabbabbaabababbbbaaaabbbbbbbbaaabbabbaabaaababbab";
        "bbabbabbabbaaaabbaabbbaaaaabaaababbbbbabbabbabba";
        "bbbbaabababababbbabaabab";
        "abbbbabbababbaabaabbabab";
        "aaabaabbbbbabaaaaababbab";
        "baaaaabaabaaabbabbaabbbbbaaabbaa";
        "bbbbaabaababbababbbabbab";
        "aababaaabbbbbbbabbbababbbabbaabbabaaabbb";
        "aabbbaaaaaaaabaabaaaaaabaabbbbbbaabababb";
        "aaaaaabbbaabaabbaabbabba";
        "aaaaabbaaaabaabbabaababa";
        "aaabababbbbaaabbabbaaabbbbbaaaaababaaaaaababbababbbabaab";
        "abaababbabaabbabbbbaababbbaabbba";
        "aababaaabbbbbabaabbabbbbabbabbbbaaabbbbbabbababbbbaaabababaaaaba";
        "aaabaaababbababbaaaabbbabbbbabaaaabbbaba";
        "bbababbbaaabbaababaabbbbaaaabbbabaaaabbababbabbbabbaaaaaabbaaabababaabba";
        "aaaabbaabaaaaaabbababbbbaababbaa";
        "abaababbaaabbbbabbaabaabbbbbaabaababbbabbabbaaab";
        "aababbbbaabbaaaabbaabbabababbabaaabaaaaabaaaabbbabababab";
        "babbbabbbbbaaabbaabbabbb";
        "bbaaaaaaaabbbabbaabaabbabbbbaaaabbababba";
        "bababbbbbbbaabbbbbbabbab";
        "bbbaaabbabbbbbaaababbbbb";
        "abbbbbbbbbbbbaabaabbabbabbbbbabbaabbabbaabaabaababbbabbbbbbbbbaabbbabaaaabbbaaba";
        "aabbbbbbaaaaaabbbbbbaaababbbbbabbbaabaaabbbbbbbb";
        "bbaabababaababbbaaabbbab";
        "bbbbbabbbbbbaabababbabbb";
        "bbabbabbabababbaaaababbbbaabababbaabbabb";
        "aabbbbbbbaaabbabaaabbbbbbaabbaaaaaaabbbb";
        "bababbbaababbaabbaaababbabbabbbbbababbbaaaabbbbbabbabbaa";
        "aabbbaaaaaaabaaaaababaaababbbbabaabaabab";
        "abbaabaaaaabaaabbbaaabbbaaabbbbbbbbbbbaaaabbaabaababbbbb";
        "bbbabbbababababaabbabbbaaaaaabaaaaaaabaabbbbaabaaabbabaabaababbbaaaaabaaabbbbbbbbaaabbaabaaaabaa";
        "baabaaabbaabbbaaaaabbbaa";
        "babaaaaaabbabbababbbabba";
        "bbbbaabbbbabbbbaaaabbbbb";
        "aabaaabbbabbbabbbbabaabbaabbbabbbbaaabaabababaaabbabbaaaabbaabbaaabababb";
        "bababababbbbaaaabaabbbbaaababbaaaaabbaab";
        "bababaabbaaaababaaaaaaba";
        "abbababbbabbaabababbbbaaaabababaaaabababbababaaa";
        "abbaabaaabbabaabbbaaabbbbaaabbbb";
        "abaaababbbaababbabbbabbbaaababaa";
        "bababbbbabbbbababbbabbab";
        "baaabbbababbbabbaabbbbba";
        "aaaabbbabbaaabaaaabbbaaabbababba";
        "bbbaaaabbbabbbbaabbbabba";
        "abaaaaabbbbaaaaabbabaaabaabbabbababbbbbb";
        "aaaaabbaaaabababbbbaaaba";
        "ababaaaaabaabbabbbbaababbbbbbbaababbaaaa";
        "babbbabbabaaaaaabababababaabaababbbabbaa";
        "aaaaaabbaaabaababbaaabba";
        "baabbbaabbababaabbbbaaaabbaabaabbabaabbbaaaababa";
        "abbabaaabaaaabbbaaaaabbb";
        "aababbbaabbbbbabbbababaabbbaabababbbabaaabbbbaab";
        "abbabaaababbabaaabbabbabbaabbbab";
        "bbabbbbaabbbbbabbbaabaabaaabaababbbabaababaaabbbabbaabba";
        "abaabbbabaabbabababaaabbbaabbabaaaababaababbbaabaababbab";
        "abababbabbbababbabbbbabbbabaabab";
        "aaaaabbabaaabaaaabaaabaa";
        "aaabbbbabbabbabbababaaaaaaaaabaabaabbbbb";
        "ababbababbbbabbabbbaaaaabbabbaba";
        "abbbbbbababbbbaaabbabaabaabababb";
        "bbaaabaaaaaabaababaabbabababbaaabbaababbaaabaababbaababbaabababbbaaababaaabbaaabbbbbbaaa";
        "aaaabaaabaababbbabbbbbbaabaabaaa";
        "bbaabaabaababaaaabbbabaabbaabbab";
        "abbbabaaaaaaaabbaababbbaabaaaaababaaaaaabbaabbaa";
        "abbabbababbbabbbabbbabbbaaaabaaabbaabbaaabbbabbabbbbbbbb";
        "bababaabaababbbaabbababaababaaab";
        "bbaaabaaababbaaabbaabbba";
        "aabaabbaabaaaaaabbababbb";
        "ababbbbbbbbbbabbbabbaabbbbbaabababababbabbaabbabaaaabbbbabbbbaab";
        "abaabbaaabbababaabbbaabb";
        "ababaaaaabbabbbababaaaba";
        "baabaabbabaabbaabaababaaabbbbbbbaababaaabaaabbaaabaabbbb";
        "baaabbabaaabbbbbaababaabbabaaabaaaababaa";
        "bbaaabbbbbbbaaabaaaaaaba";
        "aabaaaababbaaaabbbbbbabbaabbaaba";
        "abaabbababbaaaabbababbbaaaabbabb";
        "bbaaababbaabbbababaaaaabbababaabaaaabbaaababbaaaaaaaababaaabbaab";
        "aababbbabababbbaabbbabba";
    ]);;

let solve (rule_list, string_list) =
    let rules = List.fold_left (fun ret (k, v) -> IntMap.add k v ret) IntMap.empty rule_list in

    let rec do_match iter8 iter11 rule_idx str idx =
        let rec try_opt rule_list idx =
            let do_rule rule idx =
                match rule with
                | Rule(rule_idx) -> do_match iter8 iter11 rule_idx str idx
                | Lit(c) ->
                    if idx >= String.length str then
                        None
                    else if c = String.get str idx then
                        Some(idx + 1)
                    else
                        None in

            match rule_list with
            | [] -> Some(idx)
            | rule :: rest ->
                match do_rule rule idx with
                | None -> None
                | Some(idx) -> try_opt rest idx in

        let rec match_optlist opt_list idx =
            match opt_list with
            | [] -> None
            | opt :: rest ->
                match try_opt opt idx with
                | None -> match_optlist rest idx
                | Some(idx) as ret -> ret in (* Currently does not try the other option *)

        let opt_list =
            match rule_idx with
            | 8 -> [Array.to_list (Array.make iter8 (Rule(42)))]
            | 11 ->
                let r42 = Array.to_list (Array.make iter11 (Rule(42))) in
                let r31 = Array.to_list (Array.make iter11 (Rule(31))) in
                [List.append r42 r31]
            | x ->  IntMap.find rule_idx rules in
        match_optlist opt_list idx in

    let rec string_matches iter8 iter11 str =
        if iter11 = 0 then
            false
        else if iter8 = 0 then
            string_matches 6 (iter11 - 1) str
        else
            let ret = do_match iter8 iter11 0 str 0 in
            match ret with
            | Some(idx) when idx = String.length str -> true
            | _ -> string_matches (iter8 - 1) iter11 str in

    List.length (List.filter (string_matches 6 6) string_list)
;;

printf "result = %d\n" (solve input);;
