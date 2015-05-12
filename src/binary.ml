open Printf
open Utils

type byte = int

exception OutOfRange
exception InvalidMode

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
class interpreter (stream: byte list) =
    object(self)
        val inst = fst (popw stream)
        val mutable residue = snd (popw stream)
        method residue = residue
        method addressing mode =
            let oprand m =
                let i = (m land 7) in
                if i = 7 then
                    let (v,code) = popw residue in
                    residue <- code;
                    match (m lsr 3) land 7 with
                    | 2 -> sprintf "#%o"  v
                    | 3 -> sprintf "@#%o" v
                    | 6 -> string_of_int  v
                    | 7 -> sprintf "@%o"  v
                    | _ -> raise InvalidMode
                else if i = 6 then
                    "sp"
                else sprintf "r%d" i
            in
            if (mode land 7) != 7 then
                match (mode lsr 3) land 7 with
                | 0 -> sprintf "%s"     (oprand mode)
                | 1 -> sprintf "(%s)"   (oprand mode)
                | 2 -> sprintf "(%s)+"  (oprand mode)
                | 3 -> sprintf "@(%s)+" (oprand mode)
                | 4 -> sprintf "-(%s)"  (oprand mode)
                | 5 -> sprintf "@-(%s)" (oprand mode)
                | 6 -> begin
                    let (v,code) = popw residue in
                    residue <- code;
                    sprintf "%d(%s)" (signedw v) (oprand mode)
                end
                | 7 -> sprintf "@X(%s)" (oprand mode)
                | _ -> raise InvalidMode
            else
                oprand mode
        method interpret =
            match inst with
            | 0 -> "halt"
            | 1 -> "wait"
            | 2 -> "rti"
            | 3 -> "bpt"
            | 4 -> "iot"
            | 5 -> "reset"
            | 6 -> "rtt"
            | _ -> begin
                match (inst lsr 3) with
                | 0o00020 -> "rts"
                | _ -> begin
                    match (inst lsr 5) with
                    | 2 -> "; clear cond"
                    | _ -> begin
                        match (inst lsr 6) land 0o1777 with
                        | 0o0001 -> "jmp"
                        | 0o0003 -> "swab"
                        | 0o0064 -> "mark"
                        | 0o0065 -> "mfpi"
                        | 0o0066 -> "mtpi"
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
                                match (inst lsr 7) with
                                | 0b10001000 -> "emt"
                                | 0b10001001 -> "trap"
                                | _ -> begin
                                    match (inst lsr 8) with
                                    | 1 -> "br"
                                    | 2 -> "bne"
                                    | 3 -> "beq"
                                    | 4 -> "bge"
                                    | 5 -> "blt"
                                    | 6 -> "bgt"
                                    | 7 -> "ble"
                                    | 128 -> "bpl"
                                    | 129 -> "bmi"
                                    | 130 -> "bhi"
                                    | 131 -> "blos"
                                    | 132 -> "bvc"
                                    | 133 -> "bvs"
                                    | 134 -> "bcc"
                                    | 135 -> "bcs"
                                    | _ -> begin
                                        match (inst lsr 9) land 0o177 with
                                        | 0o004 -> "jsr"
                                        | 0o070 -> "mul"
                                        | 0o071 -> "div"
                                        | 0o072 -> "ash"
                                        | 0o073 -> "ashc"
                                        | 0o074 -> "xor"
                                        | 0o077 -> "sob"
                                        | _ -> begin
                                            match (inst lsr 12) land 0o17 with
                                            | 0o06 -> "add"
                                            | 0o16 -> "sub"
                                            | _ -> begin
                                                match (inst lsr 12) land 0o7 with
                                                | 0o1 -> begin
                                                    let src = self#addressing (inst lsr 6) in
                                                    let dst = self#addressing inst in
                                                    sprintf "mov %s, %s" src dst
                                                end
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
                let itp = new interpreter (code) in
                let res = itp#interpret in
                code <- itp#residue;
                res
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
