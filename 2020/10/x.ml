
let (--) i j =
    let rec aux n acc =
        if n < i then acc else aux (n-1) (n :: acc)
    in aux j [] ;;

let solve lst =
    let rec loop lst previous =
        match lst with
        | [] -> 1
        | h :: t ->
            if (h - previous) > 3 then
                0
            else
                let with_h = loop t h in
                let without_h = loop t previous in
                with_h + without_h in

    let sorted = List.sort compare lst in
    (loop sorted 0) / 2
;;

for i = 1 to 20 do
    printf "%d;\n" (solve (1--i))
done;;

