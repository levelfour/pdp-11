(* get the first element of tuple *)
let fst tuple =
    match tuple with
    | (x,_) -> x

(* get the second element of tuple *)
let snd tuple =
    match tuple with
    | (_,y) -> y

(* get compliment *)
let compw n = ((n lxor 0xffff) + 1) * (if (n land 0x8000) != 0 then -1 else 1)

(* convert unsigned word -> signed short *)
let signedw n =
    if (n land 0x8000) != 0 then compw n else n
