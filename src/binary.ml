open Printf

type byte = int

exception OutOfRange

let fst tuple =
    match tuple with
    | (x,_) -> x

let snd tuple =
    match tuple with
    | (_,y) -> y

(* pop word from stream according to little-endian *)
let popw stream =
    try
        ((List.nth stream 0) + ((List.nth stream 1) lsl 8),
        ExtList.List.drop 2 stream)
    with Failure("nth") ->
        raise OutOfRange

(* read word from stream according to little-endian *)
let rec readw i stream =
    if i = 0 then
        try
            (List.nth stream 0) + ((List.nth stream 1) lsl 8)
        with e -> raise e
    else
        match stream with
        | _::tl -> readw (i-1) tl
        | [] -> raise OutOfRange

(* interpreter object of byte code *)
class interpreter (inst: byte) =
    object(self)
        val inst = inst
        method interpret =
            match (inst lsr 6) land 0o1777 with
            | 0o0003 -> "swab"
            | 0o0067 -> "sxt"
            | _ -> begin
                match (inst lsr 6) land 0o777 with
                | 0o050 -> "clr"
                | 0o051 -> "com"
                | 0o052 -> "inc"
                | 0o053 -> "dec"
                | 0o054 -> "neg"
                | 0o055 -> "adc"
                | 0o056 -> "sbc"
                | 0o057 -> "tst"
                | 0o060 -> "ror"
                | 0o061 -> "rol"
                | 0o062 -> "asr"
                | 0o063 -> "asl"
                | _ -> begin
                    match (inst lsr 9) land 0o177 with
                    | 0o070 -> "mul"
                    | 0o071 -> "div"
                    | 0o072 -> "ash"
                    | 0o073 -> "ashc"
                    | 0o074 -> "xor"
                    | _ -> begin
                        match (inst lsr 12) land 0o17 with
                        | 0o06 -> "add"
                        | 0o16 -> "sub"
                        | _ -> begin
                            match (inst lsr 12) land 0o7 with
                            | 0o1 -> "mov"
                            | 0o2 -> "cmp"
                            | 0o3 -> "bit"
                            | 0o4 -> "bic"
                            | 0o5 -> "bis"
                            | _ -> "?"
                        end
                    end
                end
            end
    end

(* a.out file format *)
class a_out_format (stream: byte list) =
    object(self)
        val a_magic  = readw  0 stream
        val a_text   = readw  2 stream
        val a_data   = readw  4 stream
        val a_bss    = readw  6 stream
        val a_syms   = readw  8 stream
        val a_entry  = readw 10 stream
        val a_trsize = readw 12 stream
        val a_drsize = readw 14 stream
        val mutable code = ExtList.List.take (readw 2 stream) (ExtList.List.drop 16 stream)
        method magic     = a_magic
        method text_size = a_text
        method data_size = a_data
        method entry     = a_entry
        method code      = code
        method disas     =
            try
                let (inst,s) = popw code in
                code <- s;
                let itp = new interpreter (inst) in
                itp#interpret;
            with e ->
                raise e
    end

(* read in_channel in binary mode and returns byte list *)
let rec read_bin ic = 
    try
        let b = input_byte ic in
        b::(read_bin ic)
    with End_of_file -> []

(* receive byte list then print it like xxd-format *)
let xxd s =
    let rec xxd_sub stream counter =
        match stream with
        | byte::s -> (
            printf "%02x%s" byte (
                if counter mod 16 = 0 then "\n"
                else if counter mod 2 = 0 then " " else "");
            xxd_sub s (counter+1))
        | [] -> ()
    in xxd_sub s 1
