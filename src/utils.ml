open Printf

type byte = int

type architecture = {
    mutable r0: int;
    mutable r1: int;
    mutable r2: int;
    mutable r3: int;
    mutable r4: int;
    mutable r5: int;
    mutable r6: int;
    mutable r7: int;
    mutable ps: int;
}

exception OutOfRange
exception InvalidMode

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

(* pop word from stream according to little-endian *)
let popw stream =
    try
        ((List.nth stream 0) + ((List.nth stream 1) lsl 8),
        ExtList.List.drop 2 stream)
    with Failure("nth") ->
        raise OutOfRange

(* read word from memory according to little-endian *)
let readw n mem =
    try
        ((mem.(n)) + ((mem.(n+1)) lsl 8))
    with Invalid_argument "index out of bounds" ->
        raise OutOfRange

