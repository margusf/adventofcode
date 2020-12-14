(*
https://adventofcode.com/2020/day/14
*)

open Printf

open Batteries

type instruction = Mask of string | Mem of int * int;;

let input_test = [
    Mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X");
    Mem(8, 11);
    Mem(7, 101);
    Mem(8, 0);
];;

let input = [
    Mask("00111X0X10X0000XX00011111110000011X0");
    Mem(52006, 4929712);
    Mem(43834, 524429393);
    Mem(12235, 5761436);
    Mem(46892, 35146);
    Mem(31939, 16563655);
    Mem(59302, 423);
    Mask("0110111X1110001X1X10XX1101010011X000");
    Mem(41405, 37218266);
    Mem(26084, 35933);
    Mem(56863, 117475013);
    Mem(62063, 3066);
    Mask("0110110110110000X0X00X00X0X011100000");
    Mem(17255, 1409501);
    Mem(3182, 922832);
    Mem(612, 155268);
    Mem(54089, 238718351);
    Mask("0011001X0X11100110100011110X001100X0");
    Mem(36228, 813819599);
    Mem(26530, 199116285);
    Mem(65466, 823);
    Mem(21514, 6079436);
    Mask("X0101000X00X011X010001X01XX110100100");
    Mem(55733, 400);
    Mem(12132, 220502402);
    Mem(59992, 63891518);
    Mem(28423, 3058);
    Mem(60385, 721572);
    Mask("101011011000X110010XX01X001100000010");
    Mem(17718, 5256);
    Mem(26460, 112823);
    Mem(5706, 1814611);
    Mem(63904, 4222);
    Mem(23015, 1178098);
    Mem(46616, 2037);
    Mem(34774, 229522722);
    Mask("00101111111X00101010011X01100100X1X0");
    Mem(37506, 7646);
    Mem(25724, 7532075);
    Mem(46734, 1856);
    Mem(56304, 6237594);
    Mem(17886, 51040);
    Mem(43844, 335);
    Mask("001101011110001XX00001100X00X111110X");
    Mem(28036, 122944);
    Mem(62839, 208592302);
    Mem(61962, 271691652);
    Mask("0110110X1011X000X00010010XX1101000XX");
    Mem(2761, 7531352);
    Mem(49376, 2355483);
    Mem(4216, 918613);
    Mem(56927, 5956);
    Mem(63510, 176827);
    Mask("0X1011X111100X10101000X0X100X0X1110X");
    Mem(29120, 2006354);
    Mem(19556, 3671899);
    Mem(2168, 5030);
    Mem(1739, 1419);
    Mem(16584, 4603711);
    Mem(1274, 754034);
    Mem(27061, 284641);
    Mask("0X111X010011X10011X10010001010100100");
    Mem(38703, 6183);
    Mem(10881, 50618);
    Mem(25559, 4348205);
    Mem(11367, 105492);
    Mask("0X1X11X1X01000X010X0000001X0001X01X0");
    Mem(24466, 281526);
    Mem(2756, 345582958);
    Mem(27821, 51329276);
    Mem(3182, 1004);
    Mem(64312, 14160);
    Mem(44904, 25504);
    Mask("001101X1111000111X0010110XXX00101010");
    Mem(9655, 8358);
    Mem(58805, 59610179);
    Mem(4017, 2556);
    Mem(21076, 84081646);
    Mem(12544, 29200337);
    Mem(58825, 849315134);
    Mask("X0101001101101X0100X0110X0001001110X");
    Mem(52416, 80677);
    Mem(24809, 107220);
    Mask("0X10111X1X10001X101X0000110011X0X11X");
    Mem(30145, 59849);
    Mem(3316, 1661693);
    Mem(20518, 2429070);
    Mask("0XX010111010110XX01100001X1001101010");
    Mem(10481, 534671);
    Mem(3232, 88771);
    Mem(42476, 829903139);
    Mem(23957, 82713);
    Mem(59410, 783351894);
    Mem(54338, 713731093);
    Mask("011XX110111000X0101101011000X1001111");
    Mem(49852, 266900676);
    Mem(27265, 226);
    Mem(28046, 160578);
    Mask("0011XX110011X00X10X0X001110X1010X010");
    Mem(48742, 401824);
    Mem(63122, 28688);
    Mem(47126, 7096560);
    Mem(16772, 1939);
    Mem(10570, 10714);
    Mem(61299, 509453);
    Mask("101010010000111001001X1X000X11X10X01");
    Mem(46759, 1542447);
    Mem(40767, 4698135);
    Mem(61684, 350);
    Mem(22031, 270);
    Mask("1X100001X1011110010010100011X10000X1");
    Mem(47812, 224148024);
    Mem(59070, 4675);
    Mem(8910, 87677);
    Mask("0110X1110010001X1X100X1X01100111X000");
    Mem(52883, 15219771);
    Mem(25333, 112222);
    Mem(3339, 1843398);
    Mask("1X101X01101X00X010X001XX0101100X0X11");
    Mem(4298, 639594);
    Mem(8377, 9230);
    Mem(10177, 66175935);
    Mem(10999, 1732);
    Mem(19417, 920705);
    Mask("00XXX1111110000X101X00X10100X1100111");
    Mem(21108, 579440);
    Mem(61811, 12022501);
    Mem(4298, 6241794);
    Mem(15882, 291892238);
    Mem(34076, 22758);
    Mem(44997, 401340);
    Mem(10203, 4880);
    Mask("0010X1010X1000XX10X001X111010001X110");
    Mem(10169, 84875);
    Mem(40007, 724198522);
    Mem(26317, 5062);
    Mem(59565, 14796158);
    Mem(35179, 9273);
    Mem(11775, 425020);
    Mem(61734, 277758);
    Mask("X11010X1X011110X101100X0011X00010000");
    Mem(1616, 5642);
    Mem(29008, 40378);
    Mem(54571, 2275046);
    Mem(56598, 54180);
    Mem(44904, 52919006);
    Mask("011011110010000110X010X0XX0X11X00010");
    Mem(58510, 826924986);
    Mem(20287, 2697255);
    Mem(12002, 58954);
    Mem(23957, 9816);
    Mem(20851, 781642);
    Mask("01X011011011XX00XX011100110X100X011X");
    Mem(35399, 170571736);
    Mem(7307, 174037504);
    Mask("XX1011100X00000X10100010X10001X0X0X0");
    Mem(59534, 32654);
    Mem(50813, 982116);
    Mem(17460, 660730376);
    Mask("0X101X01101X0X001X0X00000X1011X00000");
    Mem(2628, 179572);
    Mem(37874, 64485456);
    Mem(2757, 21090);
    Mem(63548, 18421);
    Mem(34496, 25814);
    Mask("X01XX111X110X00X10000001000100100101");
    Mem(29395, 1469411);
    Mem(39019, 129148834);
    Mem(34858, 2143);
    Mem(38555, 5407018);
    Mem(64659, 6721809);
    Mem(15877, 276533);
    Mem(13477, 1551765);
    Mask("0X111X11001000XX100001000X0X10011101");
    Mem(27701, 815118);
    Mem(47892, 675192690);
    Mem(1921, 186957);
    Mem(41394, 104302);
    Mem(46219, 391);
    Mask("XX1111110010001X10X0011010010X1X1101");
    Mem(18848, 944747776);
    Mem(65221, 1488890);
    Mem(8962, 1787);
    Mask("01101111X0110X00XX00000110X00101X001");
    Mem(869, 196011);
    Mem(59565, 218484);
    Mem(3695, 30633);
    Mem(29495, 63295076);
    Mem(41574, 28810);
    Mask("0010100X10X00XX0X100000XX00011X00100");
    Mem(26317, 418);
    Mem(55269, 194823);
    Mem(2788, 107445850);
    Mem(58602, 40976);
    Mem(52055, 97759722);
    Mask("100X100100XX0101110100010011X1111X01");
    Mem(33519, 52748);
    Mem(2628, 247114902);
    Mem(12920, 118);
    Mem(19314, 24512);
    Mem(17460, 16794);
    Mask("00X1X0X1X0X011000100010X10111X1X11X1");
    Mem(40330, 64426);
    Mem(30794, 88457);
    Mem(46817, 3402);
    Mem(48648, 224349);
    Mem(13744, 16799);
    Mem(36227, 2196);
    Mask("00X011X110110X0X1101XX00101011100001");
    Mem(50422, 1636652);
    Mem(46734, 566);
    Mem(8709, 5494892);
    Mem(35996, 11312587);
    Mem(47720, 63678);
    Mem(37938, 21);
    Mask("001X1000000X0X1001000010001001100XX0");
    Mem(4298, 789383);
    Mem(49605, 29507);
    Mem(4850, 23534344);
    Mask("10101011101010000X001X001X0110X011X0");
    Mem(36247, 55966312);
    Mem(47774, 7827);
    Mem(47104, 179521);
    Mem(10169, 8480531);
    Mem(64125, 1496);
    Mem(9860, 102302115);
    Mask("XX10X1110010000XXX0010001000010XX10X");
    Mem(63122, 2529);
    Mem(15234, 3390);
    Mem(47165, 131784350);
    Mask("X110XX1X10100XX0101000001110001011XX");
    Mem(42598, 205125350);
    Mem(45213, 875385);
    Mem(1630, 374698);
    Mask("001X110X10100000110000001X00100X0101");
    Mem(869, 262);
    Mem(1729, 7311);
    Mem(38532, 37873869);
    Mask("0X10111X111000101XX000010110X1100111");
    Mem(40337, 110094841);
    Mem(23200, 5963831);
    Mask("0XX0X101XX1101X010100X11001X11X00111");
    Mem(39850, 194184);
    Mem(15113, 14352);
    Mem(37359, 62840981);
    Mask("001010X11X10X100X1000X1X0X0X0X101101");
    Mem(14025, 1248);
    Mem(4446, 60798259);
    Mem(54198, 747825);
    Mem(8222, 2843610);
    Mem(46819, 134827);
    Mask("001X10X11010110X010X01010000011X01X1");
    Mem(4571, 3);
    Mem(45153, 354688624);
    Mem(23739, 1747241);
    Mem(2180, 1501);
    Mem(31640, 314996015);
    Mem(22838, 1456);
    Mem(9279, 1793);
    Mask("0110X1X1X0110000110X00001110011X0111");
    Mem(2293, 860);
    Mem(52148, 31128529);
    Mem(6212, 62);
    Mem(43300, 57169);
    Mask("1000X001101X010011X100X110100X001X01");
    Mem(2628, 2308);
    Mem(2849, 57923736);
    Mem(12286, 287875);
    Mem(12201, 72);
    Mem(34425, 59856);
    Mask("0110X101101X010011010X0101100X1001X0");
    Mem(7749, 185738);
    Mem(4446, 594);
    Mem(51626, 351621474);
    Mem(24035, 739934921);
    Mask("1XX011011011XX001X1X0110X1110X000111");
    Mem(54919, 2711949);
    Mem(2709, 208984376);
    Mem(2761, 28699);
    Mem(45547, 34108);
    Mem(55552, 4945);
    Mem(18632, 150986875);
    Mem(10947, 400341);
    Mask("011010X1101XX1X010X1X000X100X00X0000");
    Mem(22250, 2516);
    Mem(34496, 31839);
    Mask("0110X1X000100X00XXX0X00111100X001000");
    Mem(11889, 506);
    Mem(34264, 2791);
    Mem(35884, 3473536);
    Mask("1X1XX11110X0110011X1001X1001XX01000X");
    Mem(33997, 1179307);
    Mem(50647, 669335);
    Mem(61780, 9982626);
    Mem(13748, 1981);
    Mem(29233, 950);
    Mask("0110111111100X1010X0X00X100000110110");
    Mem(22910, 1180108);
    Mem(5562, 21631);
    Mem(1222, 504800403);
    Mask("001X100X00X0011X01001010010010000001");
    Mem(24198, 119566);
    Mem(35001, 16283323);
    Mem(59436, 119739774);
    Mem(62948, 64713);
    Mem(2849, 255);
    Mem(23156, 1485);
    Mask("001X111X0X0X0000X000100100000XX01X1X");
    Mem(35160, 1583418);
    Mem(43805, 771);
    Mem(8313, 216593668);
    Mem(43300, 4138437);
    Mem(26057, 513262);
    Mem(3182, 46056);
    Mem(10789, 8045);
    Mask("001X101X1110010XX10010XX011000X00011");
    Mem(46633, 4097498);
    Mem(41631, 626);
    Mem(35179, 922);
    Mem(63510, 6031);
    Mem(18031, 6879);
    Mask("10X0X001100X111011000X1X0010X11XX000");
    Mem(28323, 16663);
    Mem(55733, 11506187);
    Mask("0X1011110X100010X010011X10001010X11X");
    Mem(6698, 11724271);
    Mem(55597, 10930);
    Mem(11310, 56566);
    Mem(4411, 489);
    Mem(7361, 431285);
    Mask("1X10100110110100100100X00000XX010100");
    Mem(16972, 963);
    Mem(34942, 1374714);
    Mem(33641, 34676);
    Mem(53248, 1039);
    Mem(50381, 239);
    Mem(41003, 271150);
    Mem(59255, 1046488);
    Mask("10111X01110XX11001001001X0001XX01X11");
    Mem(22876, 1261577);
    Mem(23514, 83628146);
    Mem(46492, 16023174);
    Mem(42168, 22907486);
    Mem(56233, 1208);
    Mask("0X1X111100XX00001000000110XX001X01X0");
    Mem(41579, 106791107);
    Mem(37293, 76855011);
    Mem(23712, 3066);
    Mem(53928, 206585650);
    Mem(38356, 58860);
    Mask("0010X001101X0100X001101X1100100111X0");
    Mem(40721, 1522);
    Mem(35179, 41746958);
    Mem(19534, 58840945);
    Mem(32324, 147835050);
    Mem(48430, 245);
    Mask("00XXX1110001000X00000011101001X01011");
    Mem(47713, 371239);
    Mem(12557, 896);
    Mem(23039, 728213024);
    Mem(39609, 19414);
    Mem(44321, 11334054);
    Mem(36247, 398030);
    Mask("00101101001X0X0XX0X0001X0X01X110011X");
    Mem(46734, 676353);
    Mem(60374, 267786);
    Mem(38508, 446859055);
    Mem(64904, 14216866);
    Mem(31959, 813920705);
    Mem(27255, 784);
    Mem(36553, 255261);
    Mask("0011X1X1X01X0X111000101100000X001000");
    Mem(46633, 1221);
    Mem(33954, 680347);
    Mem(21297, 35894);
    Mem(41405, 6184);
    Mask("001X111111100X001XX000X01001XXX0X010");
    Mem(39635, 477960);
    Mem(39405, 161170);
    Mem(36252, 453585);
    Mem(55397, 2058746);
    Mem(33107, 2663);
    Mask("101X100X1001XX00010010001X1X1X010000");
    Mem(35277, 200785);
    Mem(19680, 1119384);
    Mem(46603, 2780262);
    Mask("111X01110X1000001X0010011000011000X0");
    Mem(30145, 7700);
    Mem(61472, 979688);
    Mem(14460, 21055);
    Mem(16944, 313655989);
    Mask("0110111X00X0000X10X00X0X010X000X11X0");
    Mem(2168, 26991523);
    Mem(5264, 980832681);
    Mem(36646, 813667866);
    Mem(48602, 1783);
    Mask("0X101000X010001001001110110XX11X111X");
    Mem(23627, 807818);
    Mem(61811, 23479);
    Mem(64, 219255);
    Mem(37128, 1553397);
    Mem(14691, 67418150);
    Mask("1X1010011000110001X01XX0010X100110X0");
    Mem(27135, 64061097);
    Mem(19834, 2824449);
    Mem(50521, 3939);
    Mem(58503, 8393);
    Mem(28423, 43394);
    Mask("XX1X1111X01011X011X100010X1100100XX1");
    Mem(23429, 130936);
    Mem(48602, 7532488);
    Mem(6436, 310907);
    Mem(24886, 27122161);
    Mem(27957, 50861195);
    Mem(54279, 180122731);
    Mask("X0X01X1011100X01110XX0X1X11101X00010");
    Mem(14025, 691);
    Mem(2825, 249);
    Mem(3925, 3303251);
    Mask("00XX111X111000101X10000X10X011X11110");
    Mem(17498, 634241);
    Mem(15524, 20855180);
    Mask("0X1X1101X01X0100110100XX1X10XX100100");
    Mem(30145, 625);
    Mem(9797, 1359);
    Mem(12286, 1127042);
    Mask("101X100100X011101X00X100X1X1111X1X11");
    Mem(10972, 42023592);
    Mem(61376, 3427840);
    Mem(27255, 6685615);
    Mem(13520, 10945);
    Mem(55597, 807895898);
    Mem(60531, 5121);
    Mask("0X10100X101X0X0X1X000000X01011100100");
    Mem(10352, 861247);
    Mem(2656, 3492337);
    Mem(55397, 2392591);
    Mem(29495, 98579);
    Mem(2757, 3455299);
    Mem(11236, 4020);
    Mask("011011100X1XXX00100XX1001X00X0000100");
    Mem(45068, 386);
    Mem(8960, 151);
    Mem(17784, 108694472);
    Mem(26289, 4159);
    Mem(3665, 674);
    Mem(54896, 131398121);
    Mask("00101X11X110000X1010X0100100X1010110");
    Mem(34512, 162);
    Mem(36639, 15024013);
    Mem(34942, 80023258);
    Mem(24555, 418619);
    Mem(50642, 27165886);
    Mem(345, 110421710);
    Mask("X0101101101100X010000X00111011X00101");
    Mem(1153, 62642);
    Mem(56846, 5129);
    Mem(20775, 4212056);
    Mem(19328, 216506);
    Mem(29495, 8683991);
    Mask("0010111X111000X010100X00X1XX11X0X11X");
    Mem(3771, 4382805);
    Mem(57881, 16921);
    Mem(63654, 6152);
    Mem(23552, 5702333);
    Mem(6083, 220540005);
    Mask("X01X1X0110001110010X011XX100X001X110");
    Mem(59847, 564882739);
    Mem(51385, 221685661);
    Mem(61811, 204871661);
    Mem(56244, 31583475);
    Mem(6980, 1527);
    Mem(26289, 15857);
    Mask("X010100XX000X110X100110X0XX1111X0101");
    Mem(4571, 54901211);
    Mem(57199, 45702);
    Mem(42452, 43929335);
    Mem(19680, 48875);
    Mem(10352, 122542);
    Mem(34374, 2882);
    Mem(61522, 3971);
    Mask("01X011X10010001010X00110X1000X11X001");
    Mem(25036, 2952);
    Mem(61299, 2085542);
    Mem(19117, 186);
    Mem(53853, 187);
    Mask("0011X111X0100X1X10X0110XX00010111110");
    Mem(57399, 1006550549);
    Mem(63028, 186530);
    Mem(48554, 22962);
    Mem(56976, 47700);
    Mem(6575, 131534365);
    Mem(52761, 3853817);
    Mask("XXX0XX0110110100X1010000X010010X0101");
    Mem(11889, 29237617);
    Mem(17718, 1630);
    Mem(4636, 229985);
    Mem(52883, 84375864);
    Mask("X01010001011X1X0X10XX1010XX101100101");
    Mem(49740, 7868911);
    Mem(37506, 4002);
    Mem(32663, 869910098);
    Mem(8572, 125350);
    Mem(8342, 61042);
    Mask("X0101X0100100101X00000101X00XX1100X0");
    Mem(59565, 1105);
    Mem(27715, 1745);
    Mem(59206, 330363729);
    Mask("01101111X0110000100000X010X01010X10X");
    Mem(44171, 899);
    Mem(47812, 21673008);
    Mem(27608, 13645404);
    Mem(32326, 491141);
    Mem(63638, 694);
    Mem(53420, 426003787);
    Mem(51557, 3275141);
    Mask("X1101X11001X001010000010XXXX010XX000");
    Mem(54777, 804);
    Mem(34172, 6830067);
    Mem(49202, 63909);
    Mem(13477, 10986);
    Mask("101XX0011XXX11X00100101001001X011011");
    Mem(16846, 27316344);
    Mem(50094, 16967873);
    Mem(61780, 900178);
    Mem(15882, 23418);
    Mem(22876, 3337);
    Mem(47284, 230107);
    Mask("0111110X1X10010X1101001X1001011X0001");
    Mem(12201, 6745);
    Mem(25284, 182);
    Mem(44850, 1569);
    Mem(47949, 411159);
    Mem(30793, 845530);
    Mem(47029, 58274);
    Mask("X11011X11010X100110100001111X0000101");
    Mem(3182, 10552);
    Mem(48303, 111746);
    Mem(14883, 15066);
    Mem(4517, 28345405);
    Mem(25038, 178092778);
    Mask("1X001001X01101XX11X10X01001100100001");
    Mem(22910, 1977);
    Mem(15113, 102588);
    Mem(62218, 1881);
    Mask("X01X101110101XX00X0010100010000X111X");
    Mem(33012, 795);
    Mem(50671, 14579873);
    Mem(55556, 169319);
    Mem(10502, 3909);
    Mem(36753, 31795);
    Mem(41712, 8377123);
    Mem(63904, 4717);
    Mask("00110111XXX00X1110000001010XX0X0X010");
    Mem(2564, 131432438);
    Mem(28323, 3416844);
    Mem(49852, 3072);
    Mem(42274, 404);
    Mask("011011110X110000000001001000XX1001X1");
    Mem(47316, 107);
    Mem(12286, 12644);
    Mem(37518, 92320);
    Mem(54777, 2007);
    Mem(54708, 8481064);
    Mem(48684, 2372);
    Mask("01X0011000100100011XX0001110XX001100");
    Mem(5220, 1685);
    Mem(11442, 5413142);
    Mask("01001X0101110X0X10100001X0X0X1XX101X");
    Mem(57860, 3766);
    Mem(699, 147751906);
    Mem(16648, 30301);
];;

module IntMap = Map.Make(struct type t = int let compare = compare end)

let solve input =
    let make_mask str =
        let do_mask x_val =
            let new_str = String.map (fun c -> if c = 'X' then x_val else c) str in
            int_of_string ("0b" ^ new_str) in

        let and_mask = do_mask '1' in
        let or_mask = do_mask '0' in
        (and_mask, or_mask) in

    let store mem (and_mask, or_mask) addr value =
        IntMap.add addr (value land and_mask lor or_mask) mem in

    let do_inst (mem, mask) inst =
        match inst with
        | Mask(str) -> (mem, make_mask str)
        | Mem(addr, v) -> (store mem mask addr v, mask) in

    let (mem, _) = List.fold_left do_inst (IntMap.empty, (0, 0)) input in
    IntMap.fold (fun _ v a -> v + a) mem 0
;;

printf "result = %d\n" (solve input);;
