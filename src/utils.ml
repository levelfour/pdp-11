open Printf

(* get the first element of tuple *)
let fst tuple =
    match tuple with
    | (x,_) -> x

(* get the second element of tuple *)
let snd tuple =
    match tuple with
    | (_,y) -> y

(* return if the given byte value is positive *)
let posb n = ((n lsr 7) land 1) = 0

(* return if the given word value is positive *)
let posw n = ((n lsr 15) land 1) = 0

(* get absolute word value *)
let absw n =
    if not (posw n) then ~-n else n

(* get compliment *)
let compb n = ((n lxor 0xff) + 1) * (if not (posb n) then -1 else 1)

(* get compliment *)
let compw n = ((n lxor 0xffff) + 1) * (if not (posw n) then -1 else 1)

(* convert unsigned byte -> signed byte *)
let signedb n =
    if not (posb n) then compb n else n

(* convert unsigned word -> signed short *)
let signedw n =
    if not (posw n) then compw n else n

(* convert unsigned word -> string (as signed octal word) *)
let signedo o =
    sprintf "%s%o" (if not (posw o) then "-" else "") (absw (signedw o))
